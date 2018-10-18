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
    
    std::vector<T> st_eat, h_eat; // earliest arrival time at station, hub
    std::vector<pset> all_pareto, incr_pareto, tmp_pareto,
        dst_pareto; //eat vs walking time
    //std::vector<T> eat; // earliest arrival time at stop
    std::vector<std::vector<S> > parent; // not_stop_index for src
    std::vector<ST> n_trips; // number of trips for current eat
    
    std::vector<ST> improved_stations, improved_hubs;
    std::vector<bool> station_has_improved, hub_has_improved;
    std::vector<R> station_improved_through;
    std::vector<bool> stop_has_improved;
    std::vector<R> improved_routes;
    std::vector<int> route_has_improved_from, route_has_improved_to;

    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers, rev_inhubs;
    //pruned_landmark_labeling<graph> pll_lb;


public:
    raptor(const timetable tt)
        : ttbl(tt),
          st_eat(tt.n_st), h_eat(tt.n_h), n_trips(tt.n_h),
          all_pareto(tt.n_h), incr_pareto(tt.n_h),
          tmp_pareto(tt.n_s), dst_pareto(ntrips_max + 1),
          //eat(tt.n_s)),
          station_has_improved(tt.n_st, false),
          hub_has_improved(tt.n_h, false),
          station_improved_through(tt.n_st, tt.n_r),
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
            parent.push_back(std::vector<S>(tt.n_h, not_stop_index));
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

        // ---------------- lower bound graph ---------------------        
        //pll_lb.print_stats(std::cerr);
        
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int k_max = ntrips_max,
                            connection_scan *earliest_only_csa = nullptr) {

        assert(k_max <= ntrips_max);
        
        /* Track for debug :
        bool dbg = false;
        std::vector<ST> st_dbg = {};
        R rt1 = -1;
        auto in_st_dbg = [&st_dbg](ST st) -> bool {
            return std::find(st_dbg.begin(), st_dbg.end(), st) != st_dbg.end();
        };
        */

        // initialize
        //for (T &t : eat) { t = ttbl.t_max; }
        //for (T &t : st_eat) { t = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_st; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { h_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { n_trips[i] = ntrips_max + 1000; }
        /*
        improved_stations.clear();
        for (int i = 0; i < ttbl.n_st; ++i) { station_has_improved[i] = false; }
        for (int i = 0; i < ttbl.n_s; ++i) { stop_has_improved[i] = false; }
        improved_routes.clear();
        for (int i = 0; i < ttbl.n_r; ++i)
            { route_has_improved_from[i] = not_stop_index; }
        */
        // update helper (first phase)
        auto reach_station_trip = [this, dst](ST st, T t, R r, S par, int k) {
            if (t < st_eat[st]
                && t < st_eat[dst] // target pruning
                // bad idea: && t + pll_lb.distance(st, dst) <= st_eat[dst]
                ) {
                st_eat[st] = t;
                n_trips[st] = k;
                assert(par != not_stop_index);
                parent[k][st] = par;
                if ( ! station_has_improved[st]) {
                    improved_stations.push_back(st);
                    station_has_improved[st] = true;
                }
                station_improved_through[st] = r;
            }
        };

        // update helper (second phase)
        auto reach_station_walk = [this, dst](ST st, T t, S par, int k) {
            if ((t < st_eat[st]
                 || (t == st_eat[st] && station_has_improved[st]))
                && t < st_eat[dst] // target pruning
                // bad idea: && t + pll_lb.distance(st, dst) <= st_eat[dst]
                ) {
                if (t < st_eat[dst])
                    station_has_improved[st] = false; // improved through transfer, not trip
                st_eat[st] = t;
                n_trips[st] = k;
                parent[k][st] = par;
                for (S u : ttbl.station_stops[st]) {
                    //if (t <= prev_trip_dep) {
                    //    eat[u] = t;
                        R r = ttbl.stop_route[u].first;
                        if (station_has_improved[st]
                            && r == station_improved_through[st]) {
                            //continue;
                        }
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
                        //}
                }
            }
        };

        assert(improved_routes.empty());
        assert(improved_stations.empty());

        reach_station_walk(src, t_dep, not_stop_index, 0);

        if (use_transfers) {
            for (auto f : transfers[src]) {
                reach_station_walk(f.dst, t_dep + f.wgt, not_stop_index, 0);
            }
        } else {
            reach_station_walk(src, t_dep, not_stop_index, 0);
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
                h_eat[e.dst] = t_dep + e.wgt;
                n_trips[e.dst] = 0;
                parent[0][e.dst] = not_stop_index;
            }
            for (auto e : rev_inhubs[dst]) {
                T t = st_eat[dst];
                if (h_eat[e.dst] + e.wgt < t) {
                    t = h_eat[e.dst] + e.wgt;
                }
                st_eat[dst] = t;
                n_trips[dst] = 0;
                parent[0][dst] = not_stop_index;
            }
            for (auto e : ttbl.outhubs[src]) {
                ST h = e.dst;
                for (auto f : ttbl.inhubs[h]) {
                    if (h_eat[h] + f.wgt >= st_eat[dst]) break; // target prun
                    if (f.dst < ttbl.n_st
      // bad idea: && h_eat[h] + f.wgt + pll_lb.distance(h, dst) <= st_eat[dst]
                        ) {
                        reach_station_walk(f.dst, h_eat[h] + f.wgt,
                                           not_stop_index, 0);
                    }
                }
            }
        }

        int k = 1;
        for (; k <= k_max; ++k) {

            // first phase
            if (improved_routes.empty()) {
                //std::cerr << k <<" rounds (no route improved)\n";
                //std::cout <<"max_k "<< k <<"\n";
                break;
            }
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
                for (int y = y_end; x < x_end; ++x) {
                    //std::cerr <<" x="<< x <<" xend="<< x_end <<"\n";
                    S u = stops[x];
                    //std::cerr << "  u="<< u <<" n_s="<< ttbl.n_s
                    //          <<" "<< ttbl.stop_station.size() <<"\n";
                    ST st = ttbl.stop_station[u];
                    T eat = st_eat[st];
                    T arr;
                    if (y < y_end && (arr = trips[y][x].first) < eat) {
                        reach_station_trip(st, arr, r, par, k);
                        // if (stop_has_improved[u])
                        // just improved again, ignore previous improve
                    } else {
                        //if (x > x_last && k > 1) break;
                        /* */
                        if (stop_has_improved[u]) {
                            par = u; // TODO: only if improves walk time
                            /* dichotomic search does not seem to help 
                            if (y == y_end) {
                                auto lower = std::lower_bound(ttbl.stop_departures[u].begin(), ttbl.stop_departures[u].end(), std::max(eat, eat + min_chg_time));
                                y = std::distance(ttbl.stop_departures[u].begin(), lower);
                            }
                            */
                            while (y-1 >= 0
                                   && ttbl.stop_departures[u][y-1]
                                   >= eat + min_chg_time) {
                                --y;
                            }
                        }
                    }
                    stop_has_improved[u] = false;
                }
                route_has_improved_from[r] = not_stop_index;
            }
            improved_routes.clear();

            // second phase
            //std::cerr <<"k="<< k <<" "<< improved_stations.size() <<" stations\n";
            if (improved_stations.empty()) {
                //std::cerr << k <<" rounds (no station improved)\n";
                //std::cout <<"max_k "<< k <<"\n";
                break;
            }
            if (use_transfers) {
                for (ST st : improved_stations) {
                    reach_station_walk(st, st_eat[st], parent[k][st], k);
                    for (auto transf : transfers[st]) {
                        if (transf.dst != st) {
                            reach_station_walk(transf.dst,
                                               st_eat[st] + transf.wgt,
                                               parent[k][st], k);
                        }
                    }
                }
            }
            if (use_hubs) {
                for (ST st : improved_stations) {
                    for (auto e : ttbl.outhubs[st]) {
                        ST h = e.dst;
                        if (st_eat[st] + e.wgt >= st_eat[dst]) break; // targpr
                        if (st_eat[st] + e.wgt < h_eat[h]) {
                            h_eat[h] = st_eat[st] + e.wgt;
                            parent[k][h] = parent[k][st];
                            if ( ! hub_has_improved[h]) {
                                hub_has_improved[h] = true;
                                improved_hubs.push_back(h);
                            }
                        }
                    }
                }
                //std::cerr << improved_hubs.size() <<" hubs improved\n";
                for (ST h : improved_hubs) {
                    for (auto f : ttbl.inhubs[h]) {
                        if (h_eat[h] + f.wgt >= st_eat[dst]) break; // targpr
                        if (f.dst < ttbl.n_st) {
                            reach_station_walk(f.dst, h_eat[h] + f.wgt,
                                               parent[k][h], k);
                        }
                    }
                    hub_has_improved[h] = false;
                }
                improved_hubs.clear();
            }
            for (ST st : improved_stations) {
                    station_has_improved[st] = false;
                    station_improved_through[st] = ttbl.n_r; // no route
            }
            improved_stations.clear();

        }

        if (false && use_hubs) {
            std::unordered_map<ST, T> hub_to_dst;            
            for (auto e : ttbl.inhubs[dst]) {
                hub_to_dst[e.dst] = e.wgt;
            }
            for (int i = 0; i < ttbl.n_st; ++i) {
                if (st_eat[i] < st_eat[dst]) {
                    for (auto f : ttbl.outhubs[i]) {
                        ST h = f.dst;
                        if (st_eat[i] + f.wgt >= st_eat[dst]) break; // target prun
                        auto h_dst = hub_to_dst.find(h);
                        if (h_dst != hub_to_dst.end()
                            && st_eat[i] + f.wgt + h_dst->second < st_eat[dst]){
                            st_eat[dst] = st_eat[i] + f.wgt + h_dst->second;
                            ST h = h_dst->first;
                            int k = n_trips[h];
                            parent[k][dst] = parent[k][h];
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
                station_improved_through[st] = ttbl.n_r;
            }
            improved_stations.clear();
        }

        return st_eat[dst];
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
            if (st < ttbl.n_st
                && (! all_pareto[dst].dominates(t, w)) // target prun
                && (self_walk || all_pareto[st].add(t, w))) {
                for (S u : ttbl.station_stops[st]) {
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
                            if (f.dst != h) {
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
