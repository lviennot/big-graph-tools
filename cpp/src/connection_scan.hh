#ifndef CONNECTION_SCAN_HH
#define CONNECTION_SCAN_HH

#include <assert.h>
#include <vector>
#include <queue>
#include <set>

#include "timetable.hh"
#include "mgraph.hh"
#include "traversal.hh"
#include "pareto_rev.hh"


class connection_scan {
private:
    const timetable &ttbl;

    typedef timetable::ST ST;
    typedef timetable::S S;
    typedef timetable::R R;
    typedef timetable::T T;
    
    typedef pareto_rev<T> pset;
    typedef pset::point point;  // x = est.arr.time, y = - last.dep.time
    std::vector<pset> all_pareto;

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

    std::vector<T> st_eat, h_eat, wlk_dst; // earliest arrival time at station, hub, walking time to destination
    std::vector<std::vector<S> > parent; // a stop used at previous station
    std::vector<connection> conn;
    std::vector<std::pair<R, int> > trip_route;
    std::vector<S> trip_board; // last stop where trip can be boarded
    std::vector<int> n_trips, eat_trip; // nb of trips, and last trip for st_eat
    std::vector<int> trip_ntrips, trip_eat; // nb of trips to reach trip, eat of  trip
    std::vector<int> conn_at; // index of first connection at a given minute
    std::vector<int> conn_at_last; // last connection at a given minute
    
    // transitively closed transfers:
    typedef timetable::graph graph;
    graph transfers, rev_transfers, rev_inhubs;

    const int not_stop_index = -1;
    static const int ntrips_max = 48, not_trip_index = -1;
    
public:
    connection_scan(const timetable &tt)
        : ttbl(tt), n_tr(0),
          st_eat(tt.n_h), h_eat(tt.n_h), wlk_dst(tt.n_h),
          all_pareto(tt.n_h),
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
        trip_eat.insert(trip_eat.end(), n_tr, tt.t_max);
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

        T last = std::min(std::max(3600*24, conn.back().dep), 48*3600);
        conn_at.insert(conn_at.end(), last+1, 0);
        T t = 1;
        for (int i = 0; i < conn.size() ; ++i) {
            while (t < conn[i].dep && t <= last) { conn_at[t++] = i; }
            if (t == conn[i].dep && t <= last) conn_at[t++] = i;
        }
        //
        last = std::max(3600*24, conn.back().arr);
        conn_at_last.insert(conn_at_last.end(), last+1, conn.size() - 1);
        t = last - 1;
        for (int i = conn.size() - 1; i != -1; --i) {
            while (t > conn[i].arr && t >= 0) { conn_at_last[t--] = i; }
            if (t == conn[i].arr && t >= 0) conn_at_last[t--] = i;
        }

        // transitive closure of transfer graph:
        std::vector<graph::edge> transf;
        traversal<graph> trav(tt.transfers.n());
        for (ST st = 0; st < tt.n_st; ++st) {
            trav.clear();
            trav.dijkstra(tt.transfers, st);
            for (int i = 0; i < trav.nvis(); ++i) {
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
        rev_transfers = transfers.reverse();

        for (ST u : tt.transfers) {
            for (auto e : tt.transfers[u]) {
                assert(rev_transfers.edge_weight(e.dst, u) == e.wgt);
            }
        }
    }

    T earliest_arrival_time(const ST src, const ST dst, const T t_dep,
                            const bool use_hubs = true,
                            const bool use_transfers = false,
                            const T min_chg_time = 60,
                            const int ntr_max = ntrips_max, // FIXME : works only when the number of trips remains <= ntr_max
                            const T t_end = 0 // scan until t_end reached (used for profile pre-processing)
                            ) {

        assert(ntr_max <= ntrips_max);
        assert(0 <= t_dep && t_dep <= 3600*24);
        assert(min_chg_time > 0); // 0 is problematic with 0 delay connections

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
            if (c.dep >= st_eat[dst] && c.arr > t_end) {
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
                            T t = st_eat[st_to] + e.wgt;
                            if (t >= st_eat[dst])
                                break; // target pruning
                            update_eat(e.dst, t, c.to, st_to,
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
                       std::ostream &cout = std::cout,
                       const T min_chg_time = 60
                       ) {
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

    pareto_rev<T> profile(const ST src, const ST dst,
                const T t_beg, const T t_end,
                const bool use_hubs = true,
                const bool use_transfers = false,
                const T min_chg_bef = 60, const T min_chg_aft = 0,
                const int ntr_max = ntrips_max, // FIXME : not implemented
                const bool do_pre_scan = false
                ) {

        assert(0 <= t_beg && t_beg <= t_end && t_end <= 3600*24);
        assert(ntr_max <= ntrips_max);
        assert(min_chg_bef > 0 || min_chg_aft > 0); // 0 is problematic with 0 delay connections
        
        // Regular scan to find reachable trips (30-40ms, HL: 100-300ms):
        if (do_pre_scan) {
            assert(min_chg_aft == 0);
            earliest_arrival_time(src, dst, t_beg, use_hubs, use_transfers,
                                  min_chg_bef, ntr_max, t_end);
        }
        
        // Initialize, but preserve: trip_board
        
        for (int i = 0; i < ttbl.n_h; ++i) { wlk_dst[i] = ttbl.t_max; }
        //for (int i = 0; i < ttbl.n_h; ++i) { st_eat[i] = ttbl.t_max; }
        for (int i = 0; i < ttbl.n_h; ++i) { all_pareto[i].clear(); }
        //for (int i = 0; i < ttbl.n_h; ++i) { n_trips[i] = ntrips_max + 1000; }
        //for (int i = 0; i < ttbl.n_h; ++i) { eat_trip[i] = not_trip_index; }
        for (int tr = 0; tr < n_tr; ++tr) { trip_eat[tr] = ttbl.t_max; }
        //for (int tr = 0; tr < n_tr; ++tr) { trip_ntrips[tr] = n_tr; }
        
        if (use_transfers) {
            for (auto e : rev_transfers[dst]) {
                wlk_dst[e.dst] = e.wgt;
            }
        }

        if (use_hubs) { // need walking time from src anyway
            for (auto e : rev_inhubs[dst]) {
                wlk_dst[e.dst] = e.wgt;
            }
            for (int st = 0; st < ttbl.n_st; ++st) {
                T t_st = wlk_dst[st];
                for (auto e : ttbl.outhubs[st]) {
                    if (e.wgt >= t_st) break; // pruning
                    T t = e.wgt + wlk_dst[e.dst];
                    if (t < t_st) t_st = t;
                }
                wlk_dst[st] = t_st;
            }
        }

        assert(t_end < conn_at_last.size());
        assert(conn[conn_at_last[t_end]].arr <= t_end);
        assert(conn_at_last[t_end] == conn.size() - 1
               || conn[conn_at_last[t_end] + 1].arr > t_end);

        int n_conn = 0, n_conn_skipped = 0;
        
        for (int i = conn_at_last[t_end]; i != -1; --i) {
            const connection &c = conn[i];
            ++n_conn;
            if (do_pre_scan && trip_board[c.trip] == not_stop_index) {
                // trip is not reachable, a lot for Switzerland
                ++n_conn_skipped;
                continue;
            }
            if (c.dep < t_beg) break;
            
            ST st_from = ttbl.stop_station[c.from];
            ST st_to = ttbl.stop_station[c.to];

            // Estimate arrival time from c:
            T c_eat = ttbl.t_max;

            T t_walk = c.arr + wlk_dst[st_to];
            if (t_walk < c_eat) c_eat = t_walk;
            
            T t_trip = trip_eat[c.trip];
            if (t_trip < c_eat) c_eat = t_trip;
            
            // t_ransfer:
            if (use_transfers) {
                for(auto e : transfers[st_to]) {
                    T t = all_pareto[e.dst].smallest_x_bellow(ttbl.t_max,
                                              - (c.arr + min_chg_aft + e.wgt));
                    if (t < c_eat) c_eat = t;
                }
            }
            if (use_hubs) {
                for(auto e : ttbl.outhubs[st_to]) {
                    T t = all_pareto[e.dst].smallest_x_bellow(ttbl.t_max,
                                              - (c.arr + min_chg_aft + e.wgt));
                    if (t < c_eat) c_eat = t;
                }
                
            }

            // Update eat:
            if (c_eat < trip_eat[c.trip]) trip_eat[c.trip] = c_eat;
            if ( ! all_pareto[src].dominates(c_eat, - c.dep - min_chg_bef)) {// source dominat
                //bool seen =false;
                if (use_transfers) {
                    for(auto e : rev_transfers[st_from]) {
                        T last_dep = c.dep - min_chg_bef - e.wgt;
                        if (c_eat < t_end && last_dep >= t_beg)
                            all_pareto[e.dst].add(c_eat, - last_dep);
                        //if (e.dst == st_from) seen = true;
                    }
                }
                if (use_hubs) {
                    for(auto e : rev_inhubs[st_from]) {
                        T last_dep = c.dep - min_chg_bef - e.wgt;
                        if (c_eat < t_end && last_dep >= t_beg)
                            all_pareto[e.dst].add(c_eat, - last_dep);
                        //if (e.dst == st_from) seen = true;
                    }
                }
                //assert(seen);
            }
        }

        // Walk from src to trip:
        if (use_hubs) {
            pset &src_par = all_pareto[src];
            for(auto e : ttbl.outhubs[src]) {
                T wt = e.wgt;
                for (auto p : all_pareto[e.dst].pts) { // decr order of last_dep
                    T arr = p.x;
                    T last_dep = (- p.y) - wt - min_chg_aft;
                    if (last_dep < t_beg) break;
                    src_par.add(arr, - last_dep);
                }
            }
        }
        
        // Direct walk from src to dst:
        pset src_pareto;
        bool walk_faster = false;
        int n_walk = 0;
        for (auto p : all_pareto[src].pts) {
            T arr = p.x, dep = - p.y;
            if (arr - dep > wlk_dst[src]) {
                if ( ! walk_faster) {
                    ++n_walk;
                    //std::cout <<"walk("<< wlk_dst[src] <<") ";
                }
                walk_faster = true;
            } else {
                src_pareto.add(arr, - dep);
                //std::cout << dep <<","<< arr <<" ";
                walk_faster = false;
            }
        }
        
        //all_pareto[src].print();
        //all_pareto[dst].print();

        std::cout <<"   without_dir="<< all_pareto[src].size()
                  <<" nwalk="<< n_walk <<"    ";
        return src_pareto;
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
