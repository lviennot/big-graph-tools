#ifndef CONNECTION_SCAN_HH
#define CONNECTION_SCAN_HH

#include <assert.h>
#include <vector>
#include <queue>
#include <set>

#include "timetable.hh"
#include "traversal.hh"

class connection_scan {
private:
    const timetable ttbl;

    typedef timetable::ST ST;
    typedef timetable::S S;
    typedef timetable::R R;
    typedef timetable::T T;
    
    std::vector<T> st_eat; // earliest arrival time at station
    //std::vector<T> eat; // earliest arrival time at stop

    friend class raptor;
    
    typedef int TR; // trips
    TR n_tr;
    
    struct connection {
        TR trip;
        S from, to;
        T dep, arr;
        int index;
        connection(TR tr, S u, S v, T d, T a, int i)
            : trip(tr), from(u), to(v), dep(d), arr(a), index(i) {}
    };

    std::vector<connection> conn;
    std::vector<std::pair<R, int> > trip_route;
    std::vector<bool> trip_boarded;
    std::vector<int> trip_ntrips, n_trips;
    //std::vector<TR> scanned_trips;
    std::vector<int> conn_at; // index of first connection at a given minute
    
    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers;

    const int not_stop_index = -1;
    
public:
    connection_scan(const timetable tt)
        : ttbl(tt),
          st_eat(tt.n_st),
          //eat(tt.n_s)),
          conn(),
          trip_route(),
          trip_boarded(),
          trip_ntrips(), n_trips(tt.n_st),
          //scanned_trips(),
          conn_at(),
          transfers()
    {
        int n_conn = 0;
        for (R r = 0; r < ttbl.n_r; ++r) {
            n_tr += tt.trips_of[r].size();
            for (int i = 0; i < tt.trips_of[r].size(); ++i) {
                n_conn += tt.trips_of[r][i].size() - 1;
            }
        }
        trip_boarded.insert(trip_boarded.end(), n_tr, false);
        trip_ntrips.insert(trip_ntrips.end(), n_tr, 0);
        //scanned_trips.reserve(n_tr);

        std::cerr << n_tr <<" trips, "<< n_conn <<" connections\n";

        conn.reserve(n_conn);
        trip_route.reserve(n_tr);
        int i_tr = 0;
        for (R r = 0; r < ttbl.n_r; ++r) {
            const std::vector<S> &stops = tt.route_stops[r];
            for (int i = 0; i < tt.trips_of[r].size(); ++i) {
                for (int j = 1; j < tt.trips_of[r][i].size(); ++j) {
                    conn.emplace_back(i_tr, stops[j-1], stops[j],
                                      tt.trips_of[r][i][j-1].second,
                                      tt.trips_of[r][i][j].first,
                                      j);
                }
                trip_route.emplace_back(r, i);
                ++i_tr;
            }
        }

        std::sort(conn.begin(), conn.end(),
                  [](const connection &c, const connection &d) {
                      if (c.dep == d.dep) {
                          if (c.arr == d.arr) {
                              if (c.trip == d.trip) return c.index < d.index;
                              return c.trip < d.trip;
                          }
                          return c.arr < d.arr;
                      }
                      return c.dep < d.dep;
                  });

        T last = std::max(3600*24, conn.back().dep);
        conn_at.insert(conn_at.end(), last, 0);
        T t = 1;
        for (int i = 0; i < conn.size() ; ++i) {
            while (t < conn[i].dep) { conn_at[t++] = i; }
            if (t == conn[i].dep) conn_at[t++] = i;
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
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const T min_chg_time = 60) {
        // initialize
        for (int i = 0; i < ttbl.n_st; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_st; ++i) { n_trips[i] = n_tr; }
        for (int tr = 0; tr < n_tr; ++tr) { trip_boarded[tr] = false; }
        for (int tr = 0; tr < n_tr; ++tr) { trip_ntrips[tr] = n_tr; }
        //scanned_trips.clear();

        // Track for debug :
        ST st1 = 3780, st2 = 3785;
        R rt1 = 154;

        st_eat[src] = t_dep;
        n_trips[src] = 0;

        std::cerr <<"\n\ncsa : "<< src <<" at "<< st_eat[src]
                  <<" to "<< dst <<" at "<< st_eat[dst]
                  <<" init. "<< n_trips[src] <<","
                  << n_trips[dst] <<" trips\n";

        assert(t_dep < conn_at.size()); // seconds in a day
        assert(conn[conn_at[t_dep]].dep >= t_dep);
        assert(conn_at[t_dep] == 0 || conn[conn_at[t_dep] - 1 ].dep < t_dep);
        for (int i = conn_at[t_dep]; i < conn.size() ; ++i) {
            const connection &c = conn[i];
            if (c.dep >= st_eat[dst]) {
                //std::cerr << i - conn_at[t_dep] << " conn scanned\n";
                break; // target pruning
            }
            ST st_from = ttbl.stop_station[c.from];
            if (trip_boarded[c.trip]
                || st_eat[st_from] <= c.dep - min_chg_time) { // avoid overflow!
                //if ( ! trip_boarded[c.trip] ) {
                    //scanned_trips.push_back(c.trip);
                if (trip_route[c.trip].first == rt1
                    && (n_trips[st_from]+1 < trip_ntrips[c.trip]
                        || ! trip_boarded[c.trip]))
                    std::cerr << "board "<< c.trip
                              <<" of route "<< trip_route[c.trip].first
                              <<" in "<< st_from <<" "<< c.index
                              <<" at "<< c.dep
                              <<" >= "<< st_eat[st_from]
                              <<" ntrips=" << n_trips[st_from]
                              <<" chtm="<< min_chg_time
                              <<"\n";
                /* */
                trip_boarded[c.trip] = true;
                if (st_eat[st_from] <= c.dep - min_chg_time) {
                    trip_ntrips[c.trip] = std::min(trip_ntrips[c.trip],
                                                   n_trips[st_from]+1);
                }
                //}
                ST st_to = ttbl.stop_station[c.to];
                if (c.arr < st_eat[st_to]) {
                    st_eat[st_to] = c.arr;
                    n_trips[st_to] = trip_ntrips[c.trip];
                    if (trip_route[c.trip].first == rt1
                        || st_to == st1 || st_to == st2) {
                        std::cerr << "  conn of "<< c.trip
                                  <<" on "<< trip_route[c.trip].first
                                  <<" "<< c.index
                                  <<" : "<< st_from <<" at "<< c.dep
                                  <<" -> "<< st_to <<" at "<< c.arr
                                  <<" ntrips="<< trip_ntrips[c.trip]
                                  <<"\n";
                    }
                    /* */
                    // transfers :
                    for (auto transf : transfers[st_to]) {
                        if (c.arr + transf.wgt < st_eat[transf.dst]) {
                            if (transf.dst == st1 || transf.dst == st2)
                                std::cerr << transf.dst
                                          <<" is "<< transf.wgt <<"s from "
                                          << st_to <<" in trip "<< c.trip
                                          <<" of route "
                                          << trip_route[c.trip].first
                                          <<","<< trip_route[c.trip].second
                                          <<" at "<< c.arr + transf.wgt
                                          <<" ntrips="<< trip_ntrips[c.trip]
                                          <<"\n";
                            /* */
                            st_eat[transf.dst] = c.arr + transf.wgt;
                            n_trips[transf.dst] = trip_ntrips[c.trip];
                        }
                    }
                }
            }
        }

        //for (TR tr : scanned_trips) { trip_boarded[tr] = false; }

        std::cerr <<"\neat="<< st_eat[dst]
                  <<" in "<< n_trips[dst] <<" trips\n";
        
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


#endif // CONNECTION_SCAN_HH
