#ifndef CONNECTION_SCAN_HH
#define CONNECTION_SCAN_HH

#include <assert.h>
#include <vector>
#include <queue>
#include <set>

#include "timetable.hh"
#include "traversal.hh"

/*

\com{LV}{Tricky note}
Note that a special care has to be taken when a stop $u$ is also a hub $h$. The two roles must be treated separately. In particular, two arrival times have to be maintained for each role. When the arrival time is updated by walk, both $\tau(u)$ and $\tau(h)$ must be updated. However, when it is updated by traversing a connection, only $\tau(u)$ is updated. It would be incorrect to update $\tau(h)$ as well: when scanning a connection $v,w$ at time $t$, we update arrival time to $v$ through walking from $h$ and must ignore any update at $u$ coming from a connection within interval $[t-d(h,v),t]$. 
*/

class connection_scan {
private:
    const timetable &ttbl;

    typedef timetable::ST ST;
    typedef timetable::S S;
    typedef timetable::R R;
    typedef timetable::T T;
    
    std::vector<T> st_eat, h_eat; // earliest arrival time at station, hub
    //std::vector<T> eat; // earliest arrival time at stop
    std::vector<std::vector<S> > parent; // a stop used at previous station

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
    std::vector<S> trip_board; // last stop where trip can be boarded
    std::vector<int> trip_ntrips, n_trips, eat_trip;
    //std::vector<TR> scanned_trips;
    std::vector<int> conn_at; // index of first connection at a given minute
    std::vector<int> conn_at_last; // last connection at a given minute
    
    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers, rev_inhubs;

    const int not_stop_index = -1;
    static const int ntrips_max = 48, not_trip_index = -1;
    
public:
    connection_scan(const timetable &tt)
        : ttbl(tt), n_tr(0),
          st_eat(tt.n_h), h_eat(tt.n_h),
          //eat(tt.n_s)),
          n_trips(tt.n_h), eat_trip(tt.n_h)
    {
        parent.reserve(ntrips_max + 1);
        for (int i = 0; i <= ntrips_max; ++i) {
            parent.push_back(std::vector<S>(tt.n_h, not_stop_index));
        }

        int n_conn = 0;
        for (R r = 0; r < ttbl.n_r; ++r) {
            n_tr += tt.trips_of[r].size();
            for (int i = 0; i < tt.trips_of[r].size(); ++i) {
                n_conn += tt.trips_of[r][i].size() - 1;
            }
        }
        trip_board.insert(trip_board.end(), n_tr, not_stop_index);
        trip_ntrips.insert(trip_ntrips.end(), n_tr, 48);
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
                  [&tt](const connection &c, const connection &d) {
                      if (c.dep == d.dep) {
                          if (c.arr == d.arr) {
                              // Be careful to 0 delay connections:
                              if (c.trip == d.trip) return c.index < d.index;
                              /* if c.dep == c.arr == d.dep == d.arr and
                                 min_chg_time is 0, we can have problems
                                 with 0 delay connections:
                                 would need no loop with 0 delay and top. sort.
                                 The following heuristic is far from sufficient:
                              */
                              if (c.dep == c.arr) {// 0 delay connections!
                                  if (tt.stop_station[c.to]
                                      == tt.stop_station[d.from]) return true;
                                  if (tt.stop_station[d.to]
                                      == tt.stop_station[c.from]) return false;
                              }
                              return c.trip < d.trip;
                          }
                          return c.arr < d.arr;
                      }
                      return c.dep < d.dep;
                  });

        // Check
        for (auto c : conn) {
            R r = trip_route[c.trip].first;
            int y = trip_route[c.trip].second;
            assert(ttbl.stop_route[c.from].first == r);
            int x = ttbl.stop_route[c.from].second;
            assert(ttbl.route_stops[r][x] == c.from);
            assert(ttbl.route_stops[r][x+1] == c.to);
            assert(c.index == x+1);
            //very busy second at London: if (c.dep == 65460 && c.arr == c.dep)
            //    std::cerr << c.from <<" -> "<< c.to <<"\n";
        }

        T last = std::max(3600*24, conn.back().dep);
        conn_at.insert(conn_at.end(), last+1, 0);
        T t = 1;
        for (int i = 0; i < conn.size() ; ++i) {
            while (t < conn[i].dep) { conn_at[t++] = i; }
            if (t == conn[i].dep) conn_at[t++] = i;
        }
        //
        last = std::max(3600*24, conn.back().arr);
        conn_at_last.insert(conn_at.end(), last+1, conn.size() - 1);
        t = last - 1;
        for (int i = conn.size() - 1; i != -1; --i) {
            while (t > conn[i].arr) { conn_at[t--] = i; }
            if (t == conn[i].arr) conn_at[t--] = i;
        }

        // transitive closure of transfer graph:
        std::vector<graph::edge> transf;
        traversal<graph> trav(tt.transfers.n());
        for (ST st = 0; st < tt.n_st; ++st) {
            trav.clear();
            trav.dijkstra(tt.transfers, st);
            for (int i = 1; i < trav.nvis(); ++i) {
                ST ot = trav.visit(i);
                T t = trav.dist(ot);
                if (ot < tt.n_st) transf.push_back(graph::edge(st, ot, t));
            }
        }
        transfers.set_edges(transf, tt.n_st);
        size_t asym = transfers.asymmetry(false); 
        std::cerr << transf.size() <<" transitive transfers: "
                  << asym << " reverse links miss, "
                  << transfers.asymmetry(true) <<" reverse weights differ\n";
        assert(asym <= 100); // strange network otherwise

        rev_inhubs = tt.inhubs.reverse(); // not sorted by weight
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int ntr_max = ntrips_max // FIXME : works only when the number of trips remains <= ntr_max
                            ) {

        assert(ntr_max <= ntrips_max);
        assert(min_chg_time > 0);

        T eat_estim = ttbl.t_max;
        if (use_hubs) {
            eat_estim = 1 + earliest_arrival_time(src, dst, t_dep,
                                                  false, true,
                                                  min_chg_time, ntr_max);
        }
        /*
        */
        
        // initialize
        for (int i = 0; i < ttbl.n_h; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { n_trips[i] = ntrips_max + 1000; }
        for (int i = 0; i < ttbl.n_h; ++i) { eat_trip[i] = not_trip_index; }
        for (int tr = 0; tr < n_tr; ++tr) { trip_board[tr] = not_stop_index; }
        for (int tr = 0; tr < n_tr; ++tr) { trip_ntrips[tr] = n_tr; }
        //scanned_trips.clear();

        /* Track for debug :
        bool dbg = false;
        std::vector<ST> st_dbg = {14797, 2457, 9008, 9009};
        R rt1 = 341;
        auto in_st_dbg = [&st_dbg](ST st) -> bool {
            return std::find(st_dbg.begin(), st_dbg.end(), st) != st_dbg.end();
        };
        */

        st_eat[src] = t_dep;
        S s_src = ttbl.station_stops[src][0];
        n_trips[src] = 0;
        for (int k = 0; k <= ntr_max; ++k) {
            parent[k][src] = s_src;
        }

        auto update_eat = [this](ST st, T t, S par, ST by_st, int k,
                                 int by_trip = not_trip_index) {
            if (t < st_eat[st]) {
                st_eat[st] = t;
                eat_trip[st] = by_trip;
                n_trips[st] = k;
                parent[k][st] = par;
            }
        };
        
        if (use_transfers) {
            for (auto transf : transfers[src]) {
                if (st_eat[src] + transf.wgt < st_eat[transf.dst]) {
                    update_eat(transf.dst, st_eat[src] + transf.wgt,
                               s_src, src, 0);
                }
            }
        }

        if (use_hubs) {
            st_eat[dst] = std::min(eat_estim, ttbl.t_max);
            for (auto e : ttbl.outhubs[src]) {
                if (t_dep + e.wgt >= st_eat[dst]) break; // target prun
                update_eat(e.dst, t_dep + e.wgt,
                           s_src, src, 0);
            }
            for (auto e : rev_inhubs[dst]) {
                update_eat(dst, st_eat[e.dst] + e.wgt,
                           s_src, e.dst, 0);
            }
            for (auto e : ttbl.outhubs[src]) {
                ST h = e.dst;
                for (auto f : ttbl.inhubs[h]) {
                    if (st_eat[h] + f.wgt >= st_eat[dst]) break; // target prun
                    update_eat(f.dst, st_eat[h] + f.wgt,
                               s_src, h, 0);
                }
            }
        }
        
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
            ST st_to = ttbl.stop_station[c.to];                
            // do we need st_eat[st_from] ?
            if (use_hubs && trip_board[c.trip] == not_stop_index) {
                for (auto f : rev_inhubs[st_from]) {
                    int k = n_trips[f.dst];
                    if (k < ntr_max) {
                        update_eat(st_from, st_eat[f.dst] + f.wgt,
                                   (f.dst >= ttbl.n_st ? parent[k][f.dst]
                                    : ttbl.station_stops[f.dst][0]),
                                   f.dst, k);
                    }
                }
            }
            /* For debug purposes :
            if (c.from == 4889 || c.from == 4890 || c.to == 18855) {
                R r = trip_route[c.trip].first;
                int y = trip_route[c.trip].second;
                int x = ttbl.stop_route[c.from].second;
                std::cerr <<"..... "<< r <<"["<< y <<"]"
                          <<" "<< ttbl.trips_of[r][y][x].second
                          <<" from "<< c.from <<" idx="<< x
                          <<" at "<< c.dep <<" board="<< trip_board[c.trip]
                          <<" to "<< c.to
                          <<" at "<< c.arr 
                          <<" : "<< st_eat[st_from]
                          <<" "<< st_eat[st_to] <<"\n";
            }
            // */
            if (trip_board[c.trip] != not_stop_index
                || st_eat[st_from] + min_chg_time <= c.dep) {
                //if (trip_board[c.trip] == not_stop_index) {
                    //scanned_trips.push_back(c.trip);
                if (trip_board[c.trip] == not_stop_index
                    /* try to optimize nb trips but tricky */
                    || (n_trips[st_from] + (eat_trip[st_to] == c.trip ? 0 : 1)
                               < trip_ntrips[c.trip]
                        // TODO : if ==, check walking time
                        && st_eat[st_from] + min_chg_time <= c.dep)
                    ) {
                    trip_board[c.trip] = c.from;
                    trip_ntrips[c.trip] = n_trips[st_from] + 1;
                }
                //}
                if (trip_board[c.trip] != not_stop_index
                    && trip_ntrips[c.trip] <= ntr_max
                    && (c.arr < st_eat[st_to]
                        //|| trip_ntrips[c.trip] < n_trips[st_to]
                        // PB : what if c.arr > st_eat[st_to] ?
                        // to fix it, need eat for each ntrips
                        )) {
                    update_eat(st_to, c.arr,
                               trip_board[c.trip],
                               ttbl.stop_station[c.from],
                               trip_ntrips[c.trip], c.trip);
                    // transfers :
                    if (use_transfers) {
                        for (auto transf : transfers[st_to]) {
                            update_eat(transf.dst, c.arr + transf.wgt,
                                       c.to,
                                       st_to,
                                       trip_ntrips[c.trip]);
                        }
                    }
                    if (use_hubs) {
                        for (auto e : ttbl.outhubs[st_to]) {
                            ST h = e.dst;
                            if (st_eat[st_to] + e.wgt >= st_eat[dst])
                                break; // target pruning
                            update_eat(h, st_eat[st_to] + e.wgt,
                                       c.to,
                                       st_to,
                                       trip_ntrips[c.trip]);
                        }
                    }
                }                
            }
        }

        if (use_hubs) {
            for (auto f : rev_inhubs[dst]) {
                int k = n_trips[f.dst];
                if (k <= ntrips_max)
                    update_eat(dst, st_eat[f.dst] + f.wgt,
                               (f.dst >= ttbl.n_st ? parent[k][f.dst] // hub
                                : ttbl.station_stops[f.dst][0]),
                               f.dst, k);
            }
        }

        //for (TR tr : scanned_trips) { trip_boarded[tr] = false; }

        return st_eat[dst];
    }

    T eat(ST u) { return st_eat[u]; }

    void print_journey(ST dst,
                       const bool use_hubs = true,
                       const bool use_transfers = false,
                       const T min_chg_time = 60,
                       std::ostream &cout = std::cout) {
        for (int i = 0; i < ttbl.n_h; ++i) { h_eat[i] = ttbl.t_max; }
        int k = n_trips[dst];
        assert(k <= ntrips_max);
        S par = parent[k][dst];
        T t = st_eat[dst];
        //cout << dst <<" at "<< t <<" :\n";
        while (dst != ttbl.stop_station[par]) {
            ST st_par = ttbl.stop_station[par];
            T t_par = 0;
            bool walk = false;
            // try walk:
            if (use_transfers) {
                for (auto f : transfers[dst]) {
                    if (f.dst == st_par && t - f.wgt > t_par) {
                        walk = true;
                        t_par = t - f.wgt;
                    }
                }
            }
            if (use_hubs) {
                for (auto f : rev_inhubs[dst]) { h_eat[f.dst] = t - f.wgt; }
                for (auto e : ttbl.outhubs[st_par]) {
                    if (h_eat[e.dst] != ttbl.t_max
                        && h_eat[e.dst] - e.wgt > t_par) {
                        walk = true;
                        t_par = h_eat[e.dst] - e.wgt;
                    }
                }
                for (auto f : rev_inhubs[dst]) { h_eat[f.dst] = ttbl.t_max; }
            }
            // try trip:
            R r = ttbl.stop_route[par].first;
            int x_par = ttbl.stop_route[par].second;
            int y = -1;
            for (S s : ttbl.station_stops[dst]) {
                if (ttbl.stop_route[s].first == r
                    && ttbl.stop_route[s].second >= x_par) {
                    // find last trip arriving at t:
                    while (y+1 < ttbl.stop_arrivals[s].size()
                           && ttbl.stop_arrivals[s][y+1] <= t) {
                        ++y;
                    }
                    // arrival time at parent:
                    if (y >= 0
                        && ttbl.stop_departures[par][y] - min_chg_time > t_par){
                        walk = false;
                        t_par = ttbl.stop_departures[par][y] - min_chg_time;
                    }
                }
            }
            cout << (walk ? "walk " : "trip ") << k;
            if ( ! walk) cout << "="<<  r <<"["<< y <<"]";
            cout <<" from "<< st_par <<"="<< ttbl.hub_id[st_par]
                 <<" (stop "<< par <<") at "<< t_par
                 <<" to "<< dst <<"="<< ttbl.hub_id[dst] <<" at "
                 << t <<">="<< st_eat[dst] <<"\n";
            dst = st_par;
            t = t_par;
            k = k - (walk ? 0 : 1);
            assert(k >= 0);
            par = parent[k][dst];
        }
        cout <<"\n";
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
