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
    
    std::vector<T> st_eat; // earliest arrival time at station
    //std::vector<T> eat; // earliest arrival time at stop
    
    std::vector<ST> improved_stations;
    std::vector<bool> station_has_improved;
    std::vector<R> station_improved_through;
    std::vector<bool> stop_has_improved;
    std::vector<R> improved_routes;
    std::vector<int> route_has_improved_from, route_has_improved_to;

    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers;

    const int not_stop_index = -1;
    
public:
    raptor(const timetable tt)
        : ttbl(tt),
          csa(tt),
          st_eat(tt.n_st),
          //eat(tt.n_s)),
          improved_stations(),
          station_has_improved(tt.n_st, false),
          station_improved_through(tt.n_st, tt.n_r),
          stop_has_improved(tt.n_s, false),
          improved_routes(),
          route_has_improved_from(tt.n_r),
          route_has_improved_to(tt.n_r),
          transfers()
    {
        for (R r = 0; r < ttbl.n_r; ++r) {
            route_has_improved_from[r] = not_stop_index;
        }
        improved_stations.reserve(tt.n_st);
        improved_routes.reserve(tt.n_r);

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
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const T min_chg_time = 60,
                            const int max_ntrips = INT_MAX) {

        
        T arr_csa = csa.earliest_arrival_time(src, dst, t_dep);
        // Track for debug :
        ST st1 = 3780, st2 = 3785; S stop1 = 3866;
        R rt1 = 154;

        // initialize
        //for (T &t : eat) { t = ttbl.t_max; }
        //for (T &t : st_eat) { t = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_st; ++i) { st_eat[i] = ttbl.t_max; }
        /*
        improved_stations.clear();
        for (byte &b : station_has_improved) { b = false; }
        for (byte &b : stop_has_improved) { b = false; }
        improved_routes.clear();
        for (int &i : route_has_improved_from) { i = not_stop_index; }
        for (int &i : route_has_improved_to) { i = not_stop_index; }
        */

        // update helper (first phase)
        auto reach_station_1 = [this, dst, st1, st2](ST st, T t, R r) {
            if (t < st_eat[st]
                && t < st_eat[dst] // target pruning
                ) {
                bool print = st == st1 || st == st2;
                if (print)
                    std::cerr <<"add station "<< st
                              <<" at "<< t <<" thrg="<< r
                              <<"\n";
                /* */
                st_eat[st] = t;
                if ( ! station_has_improved[st]) {
                    improved_stations.push_back(st);
                    station_has_improved[st] = true;
                }
                station_improved_through[st] = r;
            }
        };

        // update helper (second phase)
        auto reach_station_2 = [this, dst, st1, st2, rt1](ST st, T t) {
            bool print = st == st1 || st == st2;
            if(print) std::cerr << st <<" at "<< t <<" vs "<< st_eat[st]
                                << ", "<< st_eat[dst]
                                <<" has_impr="<< station_has_improved[st]
                                <<" thrg="<< station_improved_through[st]
                                <<"\n";
            /* */
            if ((t < st_eat[st]
                 || (t == st_eat[st] && station_has_improved[st]))
                && t < st_eat[dst] // target pruning
                ) {
                if (t < st_eat[dst])
                    station_has_improved[st] = false; // improved through transfer, not trip
                st_eat[st] = t;
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
                        if (print || r == rt1)
                            std::cerr <<"add route "<< r
                                      <<" at "<< u <<" of "<< st <<" i="<< i
                                      <<" iprev=" << i_prev
                                      <<" at "<< st_eat[st]
                                      <<"\n";
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

        reach_station_2(src, t_dep);


        for (int k = 1; k <= max_ntrips; ++k) {

            // first phase
            std::cerr <<"-- k="<< k <<" eat="<< st_eat[dst]
                      <<" "<< improved_routes.size() <<" routes\n";
            if (improved_routes.size() == 0) {
                //std::cerr << k <<" rounds (no route improved)\n";
                break;
            }
            for (const R r : improved_routes) {
                const std::vector<std::vector<std::pair<T,T> > > &trips
                    = ttbl.trips_of[r];
                const std::vector<S> &stops = ttbl.route_stops[r];
                int x = route_has_improved_from[r];
                int x_end = stops.size();
                //std::cerr <<"route "<< r <<" x="<< x <<" xend="<< x_end <<"\n";
                assert(x >= 0);
                assert(x < x_end);
                int x_last = route_has_improved_to[r];
                assert(x_last >= 0 && x_last < x_end);
                int y_end = trips.size();
                for (int y = y_end; x < x_end; ++x) {
                    //std::cerr <<" x="<< x <<" xend="<< x_end <<"\n";
                    S u = stops[x];
                    //std::cerr << "  u="<< u <<" n_s="<< ttbl.n_s
                    //          <<" "<< ttbl.stop_station.size() <<"\n";
                    ST st = ttbl.stop_station[u];
                    T eat = st_eat[st];
                    T arr;
                    if (y < y_end && (arr = trips[y][x].first) < eat) {
                        reach_station_1(st, arr, r);
                        // if (stop_has_improved[u])
                        // just improved again, ignore previous improve
                    } else {
                        //if (x > x_last && k > 1) break;
                        if (u == stop1)
                            std::cerr << u
                                      <<" prev "<< ttbl.stop_departures[u][y-1]
                                      <<" vs "<< eat + min_chg_time
                                      <<"\n";
                        if (stop_has_improved[u]) {
                            while (y-1 >= 0
                                   && ttbl.stop_departures[u][y-1]
                                       - min_chg_time // avoid overflow!
                                   >= eat) {
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
                break;
            }
            for (ST st : improved_stations) {
                reach_station_2(st, st_eat[st]);
                if (st == st1 || st == st2)
                    std::cerr <<"improved "<< st <<" at "<< st_eat[st] <<"\n";
                for (auto transf : transfers[st]) {
                    if (transf.dst != st) {
                        if (transf.dst == st1 || transf.dst == st2)
                            std::cerr <<"transf to " << transf.dst
                                      <<" : "<< transf.wgt
                                      <<" from "<< st <<" at "<< st_eat[st]
                                      <<" : "<< st_eat[st] + transf.wgt
                                      <<" <? "<< st_eat[transf.dst]
                                      <<"\n";
                        reach_station_2(transf.dst, st_eat[st] + transf.wgt);
                    }
                }
                station_has_improved[st] = false;
                station_improved_through[st] = ttbl.n_r; // no route
            }
            improved_stations.clear();

            for (int i = 0; i < ttbl.n_st; ++i) {
                if (st_eat[i] > csa.st_eat[i] && k >= csa.n_trips[i]
                    && csa.st_eat[i] < st_eat[dst]) {
                    std::cerr << src <<" -> "<< dst << " at "<< t_dep
                              <<" : "<< " k="<< k
                              <<", pb2 at "<< i
                              <<" : "<< st_eat[i]
                              <<", "<< csa.st_eat[i]
                              <<" "<< csa.n_trips[i]
                              <<"\n";
                }
                assert(st_eat[i] <= csa.st_eat[i] || k < csa.n_trips[i]
                       || csa.st_eat[i] >= st_eat[dst]);
            }
            /* */
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
