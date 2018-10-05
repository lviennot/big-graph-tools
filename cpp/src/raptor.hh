#ifndef RAPTOR_HH
#define RAPTOR_HH

#include <assert.h>
#include <vector>
#include <queue>
#include <set>

#include "timetable.hh"
#include "traversal.hh"
#include "connection_scan.hh"

class raptor {
private:
    const timetable ttbl;
    connection_scan csa;

    typedef timetable::ST ST;
    typedef timetable::S S;
    typedef timetable::R R;
    typedef timetable::T T;

    static const int ntrips_max = 32; // max number of trips in a journey

    const int not_stop_index = -1;

    /*
    struct parent_info {
        S stop;
        T wait_time;
        T trip_time;
        T walk_time;
        T eat; // arrival time with the parent
        int ntrips;

        parent_info() {}

        parent_info(S s, T wait, T trp, T wlk, T e, int k)
            : stop(s), wait_time(wait), trip_time(trp), walk_time(wlk),
              eat(e), ntrips(k) {}

        parent_info(const parent_info &par, T wlk) // through transfer of wlk
            : stop(par.stop), wait_time(par.wait_time),
              trip_time(par.trip_time), walk_time(par.walk_time + wlk),
              eat(par.eat + wlk), ntrips(par.ntrips) {}

        parent_info(S s, T wait_s, T dep_s, T arr, int k) // through trip
            : stop(s), wait_time(wait_s), trip_time(arr - dep_s), walk_time(0),
              eat(arr), ntrips(k) {}
    };
    */
    
    std::vector<T> st_eat, h_eat; // earliest arrival time at station, hub
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

public:
    raptor(const timetable tt)
        : ttbl(tt),
          csa(tt),
          st_eat(tt.n_st), h_eat(tt.n_h), n_trips(tt.n_h),
          //eat(tt.n_s)),
          station_has_improved(tt.n_st, false),
          hub_has_improved(tt.n_h, false),
          station_improved_through(tt.n_st, tt.n_r),
          stop_has_improved(tt.n_s, false),
          route_has_improved_from(tt.n_r),
          route_has_improved_to(tt.n_r)
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
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int k_max = ntrips_max) {

        assert(k_max <= ntrips_max);
        
        // Track for debug :
        bool dbg = false;
        std::vector<ST> st_dbg = {};
        R rt1 = -1;
        auto in_st_dbg = [&st_dbg](ST st) -> bool {
            return std::find(st_dbg.begin(), st_dbg.end(), st) != st_dbg.end();
        };

        // initialize
        //for (T &t : eat) { t = ttbl.t_max; }
        //for (T &t : st_eat) { t = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_st; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { h_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { n_trips[i] = ntrips_max + 1000; }
        /*
        improved_stations.clear();
        for (byte &b : station_has_improved) { b = false; }
        for (byte &b : stop_has_improved) { b = false; }
        improved_routes.clear();
        for (int &i : route_has_improved_from) { i = not_stop_index; }
        for (int &i : route_has_improved_to) { i = not_stop_index; }
        */

        // update helper (first phase)
        auto reach_station_trip = [this, dst, dbg, in_st_dbg](ST st, T t, R r,
                                                           S par, int k) {
            if (t < st_eat[st]
                && t < st_eat[dst] // target pruning
                ) {
                if (dbg && in_st_dbg(st))
                    std::cerr <<"by_trip: "<< st
                              <<" at "<< t <<" thrg="<< r
                              <<"\n";
                /* */
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
        auto reach_station_walk = [this, dst, dbg, in_st_dbg, rt1](ST st, T t,
                                                                S par, int k) {
            if ((t < st_eat[st]
                 || (t == st_eat[st] && station_has_improved[st]))
                && t < st_eat[dst] // target pruning
                ) {
                if(dbg && in_st_dbg(st))
                    std::cerr <<"by_walk:"<< st <<" at "<< t
                              <<" vs "<< st_eat[st]
                              << ", "<< st_eat[dst]
                              <<" has_impr="<< station_has_improved[st]
                              <<" thrg="<< station_improved_through[st]
                              <<"\n";
                /* */
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
                        if (dbg && (in_st_dbg(st) || r == rt1))
                            std::cerr <<"add route "<< r
                                      <<" at "<< u <<" of "<< st <<" i="<< i
                                      <<" iprev=" << i_prev
                                      <<" at "<< st_eat[st]
                                      <<"\n";
                        /* */
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


        
        reach_station_walk(src, t_dep, not_stop_index, 0);

        if (dbg) {
            T arr_csa = csa.earliest_arrival_time(src, dst, t_dep,
                                                  use_hubs, use_transfers,
                                                  min_chg_time, k_max);
        }


        if (use_transfers) {
            for (auto f : transfers[src]) {
                reach_station_walk(f.dst, t_dep + f.wgt, not_stop_index, 0);
            }
        } else {
            reach_station_walk(src, t_dep, not_stop_index, 0);
        }

        if (use_hubs) {
            st_eat[dst] = 1 + csa.earliest_arrival_time(src, dst, t_dep,
                                                        false, true,
                                                        min_chg_time, k_max);
            if (st_eat[dst] > ttbl.t_max) st_eat[dst] = ttbl.t_max;
            /*
            */
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
            if (dbg) std::cerr<< dst <<" dest at "<< st_eat[dst] <<"\n";
            for (auto e : ttbl.outhubs[src]) {
                ST h = e.dst;
                for (auto f : ttbl.inhubs[h]) {
                    if (h_eat[h] + f.wgt >= st_eat[dst]) break; // target prun
                    if (f.dst < ttbl.n_st) {
                        reach_station_walk(f.dst, h_eat[h] + f.wgt,
                                           not_stop_index, 0);
                    }
                }
            }
        }

 
        for (int k = 1; k <= k_max; ++k) {

             if (dbg) {
                for (int i = 0; i < ttbl.n_st; ++i) {
                    if (st_eat[i] > csa.st_eat[i] && k-1 >= csa.n_trips[i]
                        && csa.st_eat[i] < st_eat[dst]) {
                        std::cerr << src <<" -> "<< dst << " at "<< t_dep
                                  <<" : "<< " k-1="<< k-1
                                  <<", pb2 at "<< i
                                  <<" : "<< st_eat[i]
                                  <<", "<< csa.st_eat[i]
                                  <<" ntrips="<< csa.n_trips[i]
                                  <<"\n";
                    }
                    assert(st_eat[i] <= csa.st_eat[i] || k-1 < csa.n_trips[i]
                           || csa.st_eat[i] >= st_eat[dst]);
                }
            }
            /*  */

            // first phase
            if (dbg) std::cerr <<"-- k="<< k <<" eat="<< st_eat[dst]
                               <<" "<< improved_routes.size() <<" routes\n";
            if (improved_routes.size() == 0) {
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
                if (dbg && r == rt1) std::cerr <<"route "<< r
                                         <<" x="<< x <<" xend="<< x_end <<"\n";
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
                    if (dbg && r == rt1 && in_st_dbg(st))
                        std::cerr << u <<" of "<< st
                                  <<" at "<< (y < y_end ? trips[y][x].first : ttbl.t_max)
                                  <<" prev "<< (y > 0 ? ttbl.stop_departures[u][y-1] : -1)
                                  <<" vs "<< eat + min_chg_time
                                  <<"\n";
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
            if (improved_stations.size() == 0) {
                //std::cerr << k <<" rounds (no station improved)\n";
                //std::cout <<"max_k "<< k <<"\n";
                break;
            }
            if (use_transfers) {
                for (ST st : improved_stations) {
                    reach_station_walk(st, st_eat[st], parent[k][st], k);
                    if (dbg && in_st_dbg(st))
                        std::cerr <<"improved "<< st<<" at "<<st_eat[st] <<"\n";
                    /* */
                    for (auto transf : transfers[st]) {
                        if (transf.dst != st) {
                            if (dbg && in_st_dbg(transf.dst))
                                std::cerr <<"transf to " << transf.dst
                                          <<" : "<< transf.wgt
                                          <<" from "<< st <<" at "<< st_eat[st]
                                          <<" : "<< st_eat[st] + transf.wgt
                                          <<" <? "<< st_eat[transf.dst]
                                          <<"\n";
                            /* */
                            reach_station_walk(transf.dst,
                                               st_eat[st] + transf.wgt,
                                               parent[k][st], k);
                        }
                    }
                }
            }
            if (use_hubs) {
                for (ST st : improved_stations) {
                    if (dbg && in_st_dbg(st))
                        std::cerr << ttbl.outhubs.degree(st)
                            <<" hubs of "<< st <<" at "<< st_eat[st]
                                  <<" id="<< ttbl.hub_id[st] <<"\n";
                    for (auto e : ttbl.outhubs[st]) {
                        ST h = e.dst;
                        if (dbg && in_st_dbg(st))
                            std::cerr <<"  h="<< h <<" at "
                                      << st_eat[st] + e.wgt
                                      <<" vs "<< h_eat[h] <<"\n";
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

        return st_eat[dst];
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
