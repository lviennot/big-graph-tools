#ifndef RAPTOR_HH
#define RAPTOR_HH

#include <assert.h>
#include <vector>
#include <queue>
#include <set>

#include "timetable.hh"
#include "traversal.hh"
#include "connection_scan.hh"
#include "pareto.hh"

#include "mgraph.hh"
#include "traversal.hh"
#include "pruned_landmark_labeling.hh"


class raptor {
private:
    const timetable ttbl;

    typedef timetable::ST ST;
    typedef timetable::S S;
    typedef timetable::R R;
    typedef timetable::T T;

    static const int ntrips_max = 48; // max number of trips in a journey

    const int not_stop_index = -1;

    typedef pareto<T> pset;
    typedef pset::point point;
    
    std::vector<T> st_eat; // earliest arrival time at station, hub
    std::vector<T> stop_prev_dep; // dep time of previous trip at a stop
    std::vector<pset> all_pareto, incr_pareto, tmp_pareto,
        dst_pareto; //eat vs walking time
    //std::vector<T> eat; // earliest arrival time at stop

    struct parent_t {
        bool trip;
        union {
            S stop;
            ST station;
        };
        T eat;
        T dist;
        parent_t() : trip(false), station(-1),
                     eat(std::numeric_limits<int>::max() / 2),
                     dist(std::numeric_limits<int>::max() / 2) {}
        void clear() {
            trip = false; stop = -1;
            eat=std::numeric_limits<int>::max() / 2;
            dist=std::numeric_limits<int>::max() / 2;
        }
    };
    std::vector<std::vector<parent_t> > parent; // not_stop_index for src
    std::vector<ST> n_trips; // number of trips for current eat
    
    std::vector<ST> improved_stations, improved_hubs;
    std::vector<bool> station_has_improved, hub_has_improved;
    std::vector<bool> stop_has_improved;
    std::vector<R> improved_routes;
    std::vector<int> route_has_improved_from, route_has_improved_to;

    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers, rev_inhubs;
    //pruned_landmark_labeling<graph> pll_lb;

    std::unordered_map<ST, T> hub_to_dst; // to compute distances


public:
    raptor(const timetable tt)
        : ttbl(tt),
          st_eat(tt.n_h), n_trips(tt.n_h), stop_prev_dep(tt.n_s),
          all_pareto(tt.n_h), incr_pareto(tt.n_h),
          tmp_pareto(tt.n_s), dst_pareto(ntrips_max + 1),
          //eat(tt.n_s)),
          station_has_improved(tt.n_st, false),
          hub_has_improved(tt.n_h, false),
          stop_has_improved(tt.n_s, false),
          route_has_improved_from(tt.n_r),
          route_has_improved_to(tt.n_r)
          //pll_lb(tt.lowerboundgraph)
    {
        for (R r = 0; r < ttbl.n_r; ++r) {
            route_has_improved_from[r] = not_stop_index;
        }
        improved_stations.reserve(tt.n_h);
        improved_routes.reserve(tt.n_r);
        improved_hubs.reserve(tt.n_h);

        parent.reserve(ntrips_max + 1);
        for (int i = 0; i <= ntrips_max; ++i) {
            parent.push_back(std::vector<parent_t>(tt.n_h));
        }

        // transitive closure of transfer graph:
        std::vector<graph::edge> transf;
        traversal<graph> trav(tt.n_st);
        for (ST st = 0; st < tt.n_st; ++st) {
            trav.clear();
            trav.dijkstra(tt.transfers, st);
            for (int i = 1; i < trav.nvis(); ++i) {
                ST ot = trav.visit(i);
                T t = trav.dist(ot);
                transf.push_back(graph::edge(st, ot, t));
            }
        }
        std::cerr << transf.size() <<" transitive transfers\n";
        transfers.set_edges(transf, tt.n_st);

        rev_inhubs = tt.inhubs.reverse(); // not sorted by weight

        // Check hub distances vs transfers
        graph outh = tt.outhubs.reverse();
        outh = outh.reverse(); // ID sorted

        auto dist = [this, &outh](ST u, ST v) -> T {
            auto uh = outh[u].begin();
            auto ue = outh[u].end();
            auto hv = rev_inhubs[v].begin();
            auto ve = rev_inhubs[v].end();
            T t = ttbl.t_max;
            while (uh != ue && hv != ve) {
                if (uh->dst < hv->dst) { ++uh; }
                else if (uh->dst > hv->dst) { ++hv; }
                else { // uh->dst == hv-->dst
                    T t_uhv = uh->wgt + hv->wgt;
                    if (t_uhv <t) t = t_uhv;
                    ++uh; ++hv;
                }
            }
            return t;
        };

        for (auto u : transfers) {
            for (auto e : transfers[u]) {
                //std::cerr << u <<" -> "<< e.dst <<" : "<< dist(u, e.dst) <<","<< e.wgt <<"\n";
                assert(dist(u, e.dst) <= e.wgt);
            }
        }

        hub_to_dst.reserve(500);

        // ---------------- lower bound graph ---------------------        
        //pll_lb.print_stats(std::cerr);
        
    }
    
    T walking_time(ST src, ST dst) {
        hub_to_dst.clear();            
        for (auto e : rev_inhubs[dst]) {
            hub_to_dst[e.dst] = e.wgt;
        }
        T w = ttbl.t_max;
        for (auto f : ttbl.outhubs[src]) {
            if (f.wgt >= w) break;
            auto h_dst = hub_to_dst.find(f.dst);
            if (h_dst != hub_to_dst.end() && f.wgt + h_dst->second < w) {
                w = f.wgt + h_dst->second;
            }
        }
        return w;
    }

    bool update_eat_trip(ST st, T t, T dt, S par, int k) {
        if (t < st_eat[st]) {
            st_eat[st] = t;
            n_trips[st] = k;
            parent[k][st].trip = true;
            parent[k][st].stop = par;
            parent[k][st].eat = t;
            parent[k][st].dist = dt;
            return true;
        }
        return false;
    }
    
    bool update_eat_walk(ST st, T t, T dt, ST par, int k) {
        if (t < st_eat[st]) {
            st_eat[st] = t;
            n_trips[st] = k;
            parent[k][st].trip = false;
            parent[k][st].station = par;
            parent[k][st].eat = t;
            parent[k][st].dist = dt;
            return true;
        }
        return false;
    }
    
    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int k_max = ntrips_max,
                            connection_scan *earliest_only_csa = nullptr) {

        assert(k_max <= ntrips_max);
        
        // initialize
        for (int i = 0; i < ttbl.n_h; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { n_trips[i] = ntrips_max + 1000; }
        for (int i = 0; i < ttbl.n_s; ++i) { stop_prev_dep[i] = ttbl.t_max; }

        // update helper (first phase)
        auto reach_station_trip = [this, dst](ST st, T t, T dt,
                                              R r, S par, int k) {
            if (t < st_eat[dst] // target pruning
                // bad idea: && t + pll_lb.distance(st, dst) <= st_eat[dst]
                && update_eat_trip(st, t, dt, par, k)
                ) {
                if ( ! station_has_improved[st]) {
                    improved_stations.push_back(st);
                    station_has_improved[st] = true;
                }
            }
        };

        // update helper (second phase)
        auto reach_station_walk =
            [this, dst, min_chg_time](ST st, T t, T dt, S par, int k,
                                      bool self_walk = false) {
            if (t < st_eat[dst] // target pruning
                // bad idea: && t + pll_lb.distance(st, dst) <= st_eat[dst]
                && (self_walk || update_eat_walk(st, t, dt, par, k))
                ) {
                if (st >= ttbl.n_st) return;
                for (S u : ttbl.station_stops[st]) {
                    if (t + min_chg_time <= stop_prev_dep[u]) {
                        R r = ttbl.stop_route[u].first;
                        /*FIXME: if (station_has_improved[st]
                            && parent[k][st].trip
                            && r == ttbl.stop_route[parent[k][st].stop].first) {
                            continue;
                            }*/
                        stop_has_improved[u] = true;
                        int i = ttbl.stop_route[u].second;
                        int i_prev = route_has_improved_from[r];
                        if (i_prev == not_stop_index) {
                            improved_routes.push_back(r);
                            route_has_improved_from[r] = i;
                            route_has_improved_to[r] = i;
                        } else {
                            if (i < i_prev) route_has_improved_from[r] = i;
                            if (i > route_has_improved_to[r])
                                route_has_improved_to[r] = i;
                        }
                    }
                }
            }
        };

        assert(improved_routes.empty());
        assert(improved_stations.empty());

        reach_station_walk(src, t_dep, 0, not_stop_index, 0);

        if (use_transfers) {
            for (auto f : transfers[src]) {
                reach_station_walk(f.dst, t_dep + f.wgt, f.wgt, src, 0);
            }
        }

        if (use_hubs) {
            /* Only ok for pure arrival time (not pareto set with k): */
            if (earliest_only_csa != nullptr){
                st_eat[dst] = 1 + earliest_only_csa
                    ->earliest_arrival_time(src, dst, t_dep,
                                            false, true, min_chg_time,k_max);
                if (st_eat[dst] > ttbl.t_max) st_eat[dst] = ttbl.t_max;
            }
            for (auto e : ttbl.outhubs[src]) {
                if (t_dep + e.wgt >= st_eat[dst]) break; // target prun
                reach_station_walk(e.dst, t_dep + e.wgt, e.wgt, src, 0);
            }
            for (auto e : rev_inhubs[dst]) {
                reach_station_walk(dst, st_eat[e.dst] + e.wgt, e.wgt, e.dst, 0);
            }
            for (auto e : ttbl.outhubs[src]) {
                for (auto f : ttbl.inhubs[e.dst]) {
                    if (st_eat[e.dst] + f.wgt >= st_eat[dst]) break; // targetpr
                    reach_station_walk(f.dst, st_eat[e.dst]+f.wgt, f.wgt,
                                       e.dst, 0);
                }
            }
        }

        int k = 1;
        for (; k <= k_max; ++k) {

            // first phase
            if (improved_routes.empty()) { break; }
            for (const R r : improved_routes) {
                const std::vector<std::vector<std::pair<T,T> > > &trips
                    = ttbl.trips_of[r];
                const std::vector<S> &stops = ttbl.route_stops[r];
                int x = route_has_improved_from[r];
                int x_end = stops.size();
                assert(x >= 0);
                assert(x < x_end);
                int x_last = route_has_improved_to[r];
                assert(x_last >= 0 && x_last < x_end);
                int y_end = trips.size();
                S par = not_stop_index;
                T par_eat = ttbl.t_max;
                for (int y = y_end; x < x_end; ++x) {
                    //std::cerr <<" x="<< x <<" xend="<< x_end <<"\n";
                    S u = stops[x];
                    //std::cerr << "  u="<< u <<" n_s="<< ttbl.n_s
                    //          <<" "<< ttbl.stop_station.size() <<"\n";
                    ST st = ttbl.stop_station[u];
                    T eat = st_eat[st];
                    T arr;
                    if (y < y_end && (arr = trips[y][x].first) < eat) {
                        assert(par != not_stop_index);
                        reach_station_trip(st, arr, arr - par_eat, r, par, k);
                        // if (stop_has_improved[u])
                        // just improved again, ignore previous improve
                    } else {
                        // OK when min_chg_time=0 :
                        //if (arr >= eat && x > x_last && k > 1) break;
                        if (stop_has_improved[u]) {
                            /* dichotomic search does not seem to help 
                            if (y == y_end) {
                                auto lower = std::lower_bound(ttbl.stop_departures[u].begin(), ttbl.stop_departures[u].end(), std::max(eat, eat + min_chg_time));
                                y = std::distance(ttbl.stop_departures[u].begin(), lower);
                            }
                            */
                            T eat_improved = parent[k-1][st].eat;
                            int y_prev = y;
                            while (y-1 >= 0
                                   && ttbl.stop_departures[u][y-1]
                                   >= eat_improved + min_chg_time) {
                                --y;
                            }
                            if (y < y_prev) {
                                par = u;
                                par_eat = eat_improved; // FIXME to measure waiting time: ttbl.stop_departures[u][y];
                            }
                        }
                    }
                    if (y < y_end) {
                        if (y == 0) stop_prev_dep[u] = 0;
                        else stop_prev_dep[u] = trips[y-1][x].second;
                    }
                    stop_has_improved[u] = false;
                }
                route_has_improved_from[r] = not_stop_index;
            }
            improved_routes.clear();

            // second phase
            //std::cerr <<"k="<< k <<" "<< improved_stations.size() <<" stations\n";
            if (improved_stations.empty()) { break; }
            if (use_transfers) {
                // Assumes transfer graph is transitively closed.
                for (ST st : improved_stations) {
                    reach_station_walk(st, st_eat[st], 0, st/* dum */, k, true);
                    for (auto transf : transfers[st]) {
                        if (transf.dst != st) {
                            reach_station_walk(transf.dst,
                                               st_eat[st] + transf.wgt,
                                               transf.wgt, st, k);
                        }
                    }
                }
            }
            if (use_hubs) {
                // Assumes OutHub x InHub is transitively closed.
                for (ST st : improved_stations) {
                    //FIXME: if ( ! parent[k][st].trip) continue; // already done (walk)
                    if ( ! hub_has_improved[st]) {
                        hub_has_improved[st] = true;
                        improved_hubs.push_back(st);
                    }
                    for (auto e : ttbl.outhubs[st]) {
                        if (st_eat[st] + e.wgt >= st_eat[dst]) break; // targpr
                        if (update_eat_walk(e.dst, st_eat[st]+e.wgt, e.wgt,
                                            st, k)) {
                            if ( ! hub_has_improved[e.dst]) {
                                hub_has_improved[e.dst] = true;
                                improved_hubs.push_back(e.dst);
                            }
                        }
                    }
                }
                for (ST h : improved_hubs) {
                    if (h < ttbl.n_st) {
                        reach_station_walk(h, st_eat[h], 0, h, k, true);
                    }
                    for (auto f : ttbl.inhubs[h]) {
                        if (st_eat[h] + f.wgt >= st_eat[dst]) break; // targpr
                        if (f.dst != h && f.dst < ttbl.n_st) {
                            reach_station_walk(f.dst, st_eat[h]+f.wgt,
                                               f.wgt, h, k);
                        }
                    }
                    hub_has_improved[h] = false;
                }
                improved_hubs.clear();
            }
            for (ST st : improved_stations) {
                    station_has_improved[st] = false;
            }
            improved_stations.clear();

        }

        if (false && use_hubs) {
            hub_to_dst.clear();            
            for (auto e : rev_inhubs[dst]) {
                hub_to_dst[e.dst] = e.wgt;
            }
            for (int i = 0; i < ttbl.n_st; ++i) {
                if (st_eat[i] < st_eat[dst]) {
                    for (auto f : ttbl.outhubs[i]) {
                        ST h = f.dst;
                        if (st_eat[i] + f.wgt >= st_eat[dst])
                            break; // target prun
                        auto h_dst = hub_to_dst.find(h);
                        if (h_dst != hub_to_dst.end()) {
                            update_eat_walk(dst,
                                            st_eat[i] + f.wgt + h_dst->second,
                                            f.wgt + h_dst->second,
                                            i, n_trips[i]);
                        }
                    }
                }
            }
        }

        if (k > k_max) {
            for (const R r : improved_routes) {
                const std::vector<S> &stops = ttbl.route_stops[r];
                assert(route_has_improved_from[r] != not_stop_index);
                for (int x=route_has_improved_from[r];
                     x <= route_has_improved_to[r]; ++x) {
                    stop_has_improved[stops[x]] = false;
                }
                route_has_improved_from[r] = not_stop_index;
            }
            improved_routes.clear();
            for (ST st : improved_stations) {
                station_has_improved[st] = false;
            }
            improved_stations.clear();
        }

        if (false && st_eat[dst] < ttbl.t_max && use_hubs) {
            std::cout<< n_trips[dst]
                     <<" trips from "<< src <<"="<< ttbl.station_id[src]
                     <<" at "<< t_dep
                     <<" to "<< dst <<"="<< ttbl.station_id[dst]
                     <<" :\n";
            print_journey(dst, n_trips[dst]);
            print_longest_transfer(src, t_dep, dst, n_trips[dst]);
            std::cout <<"------------\n";
        }

        if (false && use_hubs) {
            // some random destinations :
            for (int n = 0, fnd = 0; n < 1000 && fnd < 100; ++n) {
                ST i = rand() % ttbl.n_st;
                if (st_eat[i] < ttbl.t_max && st_eat[i] <= st_eat[dst]
                    && n_trips[i] > 1) {
                    ++fnd;
                    print_journey(i, n_trips[i]);
                    print_longest_transfer(src, t_dep, i, n_trips[i]);
                    std::cout <<"------------\n";
                }
            }
        }

        return st_eat[dst];
    }

    void print_journey(ST dst, int k, std::ostream &cout = std::cout) {
        const parent_t &par = parent[k][dst];
        ST par_st = par.trip ? ttbl.stop_station[par.stop] : par.station;
        if (par_st == not_stop_index) return;
        int par_k = par.trip ? k-1 : k;
        if (par.trip) {
            assert(k > 0);
            par_st = ttbl.stop_station[par.stop];
            cout <<"trip from "<< par.stop
                 <<" of "<< par_st <<"="<< ttbl.station_id[par_st]
                 <<" by "<< ttbl.stop_route[par.stop].first
                 <<" to "<< dst <<"="<< ttbl.station_id[dst]
                 <<" at "<< par.eat <<" ("<< par.dist <<"s)\n";
        } else {
            cout <<"walk from "<< par_st <<"="<< ttbl.station_id[par_st]
                 <<" to "<< dst <<"="<< ttbl.station_id[dst]
                 <<" at "<< par.eat <<" ("<< par.dist <<"s)\n";
        }
        cout <<"par "<< par_st <<" eat "<< parent[par_k][par_st].eat <<"\n";
        assert(parent[par_k][par_st].eat + par.dist == par.eat);
        print_journey(par_st, par_k, cout);
    }

    void print_longest_transfer(ST src, T t_dep, ST dst, int k,
                                std::ostream &cout = std::cout) {
        std::tuple<T, ST, ST> lg = longest_transfer(dst, n_trips[dst]);
        cout <<"longest "<< std::get<0>(lg)
             <<" from "<< std::get<1>(lg)
             <<"="<< ttbl.station_id[std::get<1>(lg)]
             <<" to "<< std::get<2>(lg)
             <<"="<< ttbl.station_id[std::get<2>(lg)]
             <<" in "<< src <<"="<< ttbl.station_id[src]
             <<" at "<< t_dep
             <<" -> "<< dst <<"="<< ttbl.station_id[dst]
             <<"\n";
    }

    std::tuple<T, ST, ST> longest_transfer(ST dst, int k) {
        std::tuple<T, ST, ST> m{0, dst, dst}, prev{0, dst, dst};
        bool first = true, prev_is_walk = false;
        while (k > 0) {
            const parent_t &par = parent[k][dst];
            ST par_st = par.trip ? ttbl.stop_station[par.stop] : par.station;
            if (par.trip) {
                first = false;
                prev_is_walk = false;
            } else { // walk
                T w = par.dist;
                if (prev_is_walk) prev = std::make_tuple(std::get<0>(prev) + w,
                                                         par_st,
                                                         std::get<2>(prev));
                else prev = std::make_tuple(w, par_st, dst);
                if ((! first) && std::get<0>(prev) > std::get<0>(m)) m = prev;
                prev_is_walk = true;
            }
            dst = par_st;
            k = par.trip ? k-1 : k;
        }
        return m;
    }
    

    // still return earliest arrival time
    T earliest_walk_pareto(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int k_max = ntrips_max) {

        assert(k_max <= ntrips_max);

        for (int i = 0; i < ttbl.n_h; ++i) { all_pareto[i].clear(); }
        for (int i = 0; i < ttbl.n_st; ++i) { incr_pareto[i].clear(); }
        for (int i = 0; i < ttbl.n_s; ++i) { tmp_pareto[i].clear(); }
        for (int i = 0; i <= k_max; ++i) { dst_pareto[i].clear(); }

        auto reach_station_trip = [this, dst](ST st, T t, T w) {
            if ((! all_pareto[dst].dominates(t, w)) // target prun
                && all_pareto[st].add(t, w)) {
                assert(incr_pareto[st].add(t, w));
                if ( ! station_has_improved[st]) {
                    improved_stations.push_back(st);
                    station_has_improved[st] = true;
                }
            }
        };

        auto reach_station_walk = [this, dst, t_dep](ST st, T t, T w,
                                              bool self_walk=false){
            if ((! all_pareto[dst].dominates(t, w)) // target prun
                && (self_walk || all_pareto[st].add(t, w))) {
                for (S u : ttbl.station_stops.at(st)) {
                    //std::cerr << st <<" "<< u <<" : "<< t <<","<< w <<" : "<<;
                    //tmp_pareto[u].print(std::cerr);
                    if (tmp_pareto[u].add(t, w)) {
                        R r = ttbl.stop_route[u].first;
                        //assert(tmp_pareto[u].add(t, w));
                        stop_has_improved[u] = true;
                        int i = ttbl.stop_route[u].second;
                        int i_prev = route_has_improved_from[r];
                        if (i_prev == not_stop_index) {
                            improved_routes.push_back(r);
                            route_has_improved_from[r] = i;
                            route_has_improved_to[r] = i;
                        } else {
                            if (i < i_prev) route_has_improved_from[r] = i;
                            if (i > route_has_improved_to[r])
                                route_has_improved_to[r] = i;
                        }
                    }
                }
            }
        };

        assert(improved_routes.empty());
        assert(improved_stations.empty());

        reach_station_walk(src, t_dep, 0);

        if (use_transfers) {
            for (auto f : transfers[src]) {
                reach_station_walk(f.dst, t_dep + f.wgt, f.wgt);
            }
        }
        if (use_hubs) {
            for (auto e : ttbl.outhubs[src]) {
                incr_pareto[e.dst].add(t_dep + e.wgt, e.wgt);
            }
            for (auto f : rev_inhubs[dst]) {
                assert(incr_pareto[f.dst].pts.size() <= 1);
                for (auto p : incr_pareto[f.dst].pts) {
                    all_pareto[dst].add(p.x+f.wgt, p.y+f.wgt);
                }
            }
            for (auto e : ttbl.outhubs[src]) {
                incr_pareto[e.dst].clear();
                for (auto f : ttbl.inhubs[e.dst]) {
                    if (all_pareto[dst].dominates(t_dep + e.wgt+f.wgt,
                                                e.wgt+f.wgt)) break; // targpr
                    reach_station_walk(f.dst, t_dep + e.wgt+f.wgt, e.wgt+f.wgt);
                }
            }
        }

        dst_pareto[0] = all_pareto[dst];

        int k = 1;
        for (; k <= k_max; ++k) {

            // first phase: follow trips from improved routes
            if (improved_routes.empty()) { break; }
            for (const R r : improved_routes) {
                const std::vector<std::vector<std::pair<T,T> > > &trips
                    = ttbl.trips_of[r];
                const std::vector<S> &stops = ttbl.route_stops[r];
                int x_end = stops.size();
                int y_end = trips.size();
                int y = y_end;
                while (route_has_improved_from[r] != not_stop_index) {
                    assert(route_has_improved_from[r] >= 0
                           && route_has_improved_from[r] < stops.size());
                    int x_beg = route_has_improved_from[r];
                    S fst = stops[x_beg];
                    route_has_improved_from[r] = not_stop_index;
                    const std::vector<point> &pts = tmp_pareto[fst].pts;
                    assert(pts.size() > 0);
                    if (y < y_end) {
                        T lst = pts[pts.size() - 1].x;
                        if (ttbl.stop_departures[fst][y] < lst + min_chg_time) {
                            y = y_end;
                        } else {
                            ++y; // so that y < y_prev bellow
                        }
                    }
                    for (int i = pts.size() - 1; i >= 0; --i) {
                        T arr = pts[i].x, wlk = pts[i].y;
                        int y_prev = y;
                        while (y-1 >= 0
                               && ttbl.stop_departures[fst][y-1]
                                  >= arr + min_chg_time) {
                            --y;
                        }
                        if (y < y_prev) { // improve arrival times along trip
                            for (int x = x_beg + 1; x < x_end; ++x) {
                                S u = stops[x];
                                arr = trips[y][x].first;
                                ST st = ttbl.stop_station[u];
                                reach_station_trip(st, arr, wlk);
                                if (stop_has_improved[u]) {
                                    tmp_pareto[u].del_dominated(arr, wlk);
                                    if (tmp_pareto[u].pts.size() == 0) {
                                        stop_has_improved[u] = false;
                                        if (route_has_improved_from[r] == x) {
                                            route_has_improved_from[r] =
                                                not_stop_index;
                                        }
                                    } else {
                                        if (route_has_improved_from[r]
                                              == not_stop_index) {
                                            route_has_improved_from[r] = x;
                                        }
                                        if (tmp_pareto[u]
                                            .dominates(arr - min_chg_time,
                                                       wlk)) {
                                            break;
                                        }
                                    }
                                }
                            }
                        } else if (y == y_end) {
                            for (int x = x_beg + 1;
                                 x <= route_has_improved_to[r]; ++x) {
                                S u = stops[x];
                                ST st = ttbl.stop_station[u];
                                if (stop_has_improved[u]) {
                                    route_has_improved_from[r] = x;
                                    break;
                                }
                            }
                        }
                    }
                    tmp_pareto[fst].clear();
                    stop_has_improved[fst] = false;
                }
                assert(route_has_improved_from[r] == not_stop_index);
            }
            improved_routes.clear();

            // second phase: walk from improved stations
            if (improved_stations.empty()) { break; }
            if (use_transfers) {
                for (ST st : improved_stations) {
                    for (auto p : incr_pareto[st].pts) {
                        reach_station_walk(st, p.x, p.y, true);
                        for (auto f : transfers[st]) {
                            if (f.dst != st) {
                                reach_station_walk(f.dst, p.x+f.wgt, p.y+f.wgt);
                            }
                        }
                    }
                }
            }
            if (use_hubs) {
                for (ST st : improved_stations) {
                    if ( ! hub_has_improved[st]) {
                        hub_has_improved[st] = true;
                        improved_hubs.push_back(st);
                    }
                    for (auto p : incr_pareto[st].pts) {
                        for (auto e : ttbl.outhubs[st]) {
                            if (all_pareto[dst].dominates(p.x+e.wgt, p.y+e.wgt))
                                break; // target prun
                            if (e.dst != st
                                && all_pareto[e.dst].add(p.x+e.wgt, p.y+e.wgt)){
                                if ( ! hub_has_improved[e.dst]) {
                                    hub_has_improved[e.dst] = true;
                                    improved_hubs.push_back(e.dst);
                                }
                                incr_pareto[e.dst].add(p.x+e.wgt, p.y+e.wgt);
                            }
                        }
                    }
                    station_has_improved[st] = false;
                }
                improved_stations.clear();
                for (ST h : improved_hubs) {
                    for (auto p : incr_pareto[h].pts) {
                        if (h < ttbl.n_st) {
                            reach_station_walk(h, p.x, p.y, true);
                        }
                        for (auto f : ttbl.inhubs[h]) {
                            if (all_pareto[dst].dominates(p.x+f.wgt, p.y+f.wgt))
                                break; // target prun
                            if (f.dst != h && f.dst < ttbl.n_st) {
                                reach_station_walk(f.dst, p.x+f.wgt, p.y+f.wgt);
                            }
                        }
                    }
                    hub_has_improved[h] = false;
                    incr_pareto[h].clear();
                }
                improved_hubs.clear();
            } else {
                for (ST st : improved_stations) {
                    station_has_improved[st] = false;
                    incr_pareto[st].clear();
                }
                improved_stations.clear();
            }
            
            dst_pareto[k] = all_pareto[dst];
        }
        if (k <= k_max) dst_pareto[k] = all_pareto[dst];

        if (k > k_max) {
            for (const R r : improved_routes) {
                const std::vector<S> &stops = ttbl.route_stops[r];
                assert(route_has_improved_from[r] != not_stop_index);
                for (int x=route_has_improved_from[r];
                     x <= route_has_improved_to[r]; ++x) {
                    if (stop_has_improved[stops[x]]) {
                        stop_has_improved[stops[x]] = false;
                        tmp_pareto[stops[x]].clear();
                    }
                }
                route_has_improved_from[r] = not_stop_index;
            }
            improved_routes.clear();
            for (ST st : improved_stations) {
                station_has_improved[st] = false;
                incr_pareto[st].clear();
            }
            improved_stations.clear();
        }
 
        size_t ps_size = 0, m_size = 0;
        for (int i = 0; i <= std::min(k, k_max); ++i) {
            m_size = std::max(m_size, dst_pareto[i].pts.size());
            if (i == 0) { ps_size += dst_pareto[i].pts.size(); }
            else {
                for (auto p : dst_pareto[i].pts) {
                    if ( ! dst_pareto[i-1].dominates(p.x, p.y)) ++ps_size;
                }
            }
        }
        /*
        std::cout <<"k "<< k <<"\n";
        std::cout <<"ps_size "<< ps_size <<"\n";
        std::cout <<"m_size "<< m_size <<"\n";
        */

        if (all_pareto[dst].pts.size() > 0) {
            return all_pareto[dst].pts[0].x;
        } else {
            return ttbl.t_max;
        }
    }

    T test(int n_q, T t_beg, T t_end) {
        assert(t_beg < t_end);
        uint64_t sum = 0, n_reached = 0;
        for (int i = 0; i < n_q; ++i) {
            ST src = rand() % ttbl.n_st;
            ST dst = rand() % ttbl.n_st;
            T t = t_beg + rand() % (t_end - t_beg);
            T arr = earliest_arrival_time(src, dst, t);
            if (arr < ttbl.t_max) {
                ++n_reached;
                sum += arr - t;
            }
        }
        std::cerr << n_reached <<" reached\n";
        return (T) (sum / n_reached);
    }
};


#endif // RAPTOR_HH
