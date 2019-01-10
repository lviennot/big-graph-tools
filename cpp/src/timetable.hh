#ifndef TIMETABLE_HH
#define TIMETABLE_HH

#include <assert.h>
#include <limits>
#include <iostream>
#include <zlib.h>
#include <algorithm>
#include <vector>
#include <set>
#include <cstdarg>
#include <cmath>
#include <string>
#include <unordered_map>
#include <map>

#include "mgraph.hh"

class timetable {
    // a station has several stops, each stop belongs to exactly one route
    // all trips in a route have same sequence of stops, one trip never overpass another
public:
    typedef int ST; // stations
    typedef int S; // stops
    typedef int R; // routes
    typedef int T; // time

    const int t_min = 0, t_max = std::numeric_limits<int>::max() / 2;

    typedef std::string id;
    typedef mgraph<ST, T> graph;

    size_t n_st, n_tr, n_s, n_r, n_h; // number of stations, transfer nodes, stops, routes, hubs
    std::vector<std::vector<S> > station_stops; // stops of a station
    std::vector<id> station_id, hub_id; // id from the gtfs data
    std::vector<ST> stop_station;
    std::vector<std::pair<R, int> > stop_route; // route at a stop, and index of stop in the route sequence
    std::vector<std::vector<T> > stop_departures; // departure time
    std::vector<std::vector<T> > stop_arrivals; // arrival time
    std::vector<std::vector<S> > route_stops; // stop sequence of a route
    std::vector<std::vector<std::vector<std::pair<T,T> > > > trips_of; // trips of a route : arrival/departure times at each stop
    graph transfers, inhubs, outhubs, lowerboundgraph;
    
    std::map<id, ST> id_to_station, id_to_hub;

public:
    timetable(std::string day, std::string date,
              std::string calendar, std::string calendar_dates,
              std::string tripsfile, std::string stop_times,
              std::string transfersfile, bool symmetrize = false)
        : n_st(0), n_tr(0), n_s(0), n_r(0), n_h(0)
    {
        auto services = services_at(day, date, calendar, calendar_dates);
        std::cerr << services.size() <<" services\n";
        auto trips = trips_served(services, tripsfile);
        std::cerr << trips.size() <<" trips\n";
        auto trip_seq = trips_sequences(stop_times, trips);
        int events = 0;
        for (auto seq : trip_seq) events += seq.second.size();
        std::cerr << events <<" events\n";
        auto transf = station_transfers(transfersfile);
        build(trip_seq, transf, symmetrize);
    }

    timetable(std::string stop_times,
              std::string transfersfile, bool symmetrize = false)
        : n_st(0), n_tr(0), n_s(0), n_r(0), n_h(0)
    {
        auto trip_seq = trips_sequences_oneday(stop_times);
        int events = 0;
        for (auto seq : trip_seq) events += seq.second.size();
        std::cerr << events <<" events\n";
        auto transf = station_transfers_2(transfersfile);
        build(trip_seq, transf, symmetrize);
    }

    /*timetable(const timetable &&tt)
        : n_st(tt.n_st), n_s(tt.n_s), n_r(tt.n_r), n_h(tt.n_h),
          station_stops(tt.station_stops), station_id(tt.station_id),
          hub_id(tt.hub_id), stop_station(tt.stop_station),
          stop_route(tt.stop_route), stop_departures(tt.stop_departures),
          stop_arrivals(tt.stop_arrivals), route_stops(tt.route_stops),
          trips_of(tt.trips_of), transfers(tt.transfers), inhubs(tt.inhubs),
          outhubs(tt.outhubs), lowerboundgraph(tt.lowerboundgraph),
          id_to_station(tt.id_{
        
          }*/

    timetable(std::string stop_times,
              std::string inhubsfile, std::string outhubsfile,
              std::string transfersfile, bool symmetrize = false,
              std::string walkingfile="", T t_from=0, T t_to=0)
        : n_st(0), n_tr(0), n_s(0), n_r(0), n_h(0)
    {
        auto trip_seq = trips_sequences_oneday(stop_times);
        int events = 0;
        for (auto seq : trip_seq) events += seq.second.size();
        std::cerr << events <<" events\n";
        auto transf = station_transfers_2(transfersfile);
        build(trip_seq, transf, symmetrize);
        // hubs :
        assert(n_st + n_tr == station_id.size());
        hub_id.reserve(n_st + n_tr);
        for (int st = 0; st < n_st + n_tr; ++st) {
            hub_id.push_back(station_id[st]);
            id_to_hub[station_id[st]] = st;
        }
        n_h = hub_id.size();
        auto get_hub = [this](std::string h) -> ST {
            auto search = id_to_hub.find(h);
            if (search == id_to_hub.end()) {
                assert(n_h == hub_id.size());
                hub_id.push_back(h);
                id_to_hub[h] = n_h;
                return n_h++;
            }
            return search->second;
        };
        // in-hubs :
        std::vector<graph::edge> edg;
        std::vector<bool> seen(n_st, false);
        auto rows = read_tuples(inhubsfile, 3);
        edg.reserve(rows.size());
        for (auto r : rows) {
            auto st = id_to_station.find(r[1]);
            if (st != id_to_station.end()) {
                ST h = get_hub(r[0]);
                ST s = st->second;
                seen[s] = true;
                T delay = std::stoi(r[2]);
                edg.push_back(graph::edge(h, s, delay));
            }
        }
        for (int st = 0; st < n_st; ++st)
            if ( ! seen[st]) edg.push_back(graph::edge(st, st, 0));
        std::sort(edg.begin(), edg.end(), [](const graph::edge &e,
                                             const graph::edge &f) {
                      return e.wgt < f.wgt;
                  });
        inhubs.set_edges(edg, n_h);
        std::cerr << inhubs.n() <<" in-hubs, avg in-degree "
                  << (inhubs.m() / n_st) <<"\n";
        // out-hubs :
        edg.clear();
        for (int st = 0; st < n_st; ++st) seen[st] = false;
        rows = read_tuples(outhubsfile, 3);
        edg.reserve(rows.size());
        for (auto r : rows) {
            auto st = id_to_station.find(r[0]);
            if (st != id_to_station.end()) {
                ST s = st->second;
                auto h = id_to_hub.find(r[1]);
                if (h != id_to_hub.end()) {
                    T delay = std::stoi(r[2]);
                    edg.push_back(graph::edge(s, h->second, delay));
                }
            }
        }
        for (int st = 0; st < n_st; ++st)
            if ( ! seen[st]) edg.push_back(graph::edge(st, st, 0));
        std::sort(edg.begin(), edg.end(), [](const graph::edge &e,
                                             const graph::edge &f) {
                      return e.wgt < f.wgt;
                  });
        outhubs.set_edges(edg, n_h);
        std::cerr << outhubs.n() <<" out-hubs, avg out-degree "
                  << (outhubs.m() / n_st) <<"\n";
        std::cerr << n_h <<" hubs\n";

        // Check weight sort:
        for (ST u : outhubs) {
            T dt = 0;
            for (auto e : outhubs[u]) { assert(dt <= e.wgt); dt = e.wgt; }
        }
        for (ST u : inhubs) {
            T dt = 0;
            for (auto e : inhubs[u]) { assert(dt <= e.wgt); dt = e.wgt; }
        }
        
        // lower-bound graph
        if (walkingfile != "") {
            size_t n_lb = n_h;
            std::map<id, ST> id;
            auto get_node = [this, &n_lb, &id](std::string u) -> ST {
                auto search = id_to_hub.find(u);
                if (search != id_to_hub.end()) {
                    return search->second;
                } // else
                search = id.find(u);
                if (search != id.end()) {
                    return search->second;
                } // else
                id[u] = n_lb;
                return n_lb++;
            };

            std::vector<graph::edge> edg;
            auto rows = timetable::read_tuples(walkingfile, 3);
            edg.reserve(rows.size() + 100000);
            for (auto r : rows) {
                ST u = get_node(r[0]), v = get_node(r[1]);
                T delay = std::stoi(r[2]);
                edg.push_back(graph::edge(u, v, delay));
            }

            // fastest trip for each connection
            if (t_to == 0) t_to = t_max;
            for (R r = 0; r < n_r; ++r) {
                const std::vector<std::vector<std::pair<T,T> > > &trips
                    = trips_of[r];
                const std::vector<S> &stops = route_stops[r];
                for (int x = 1; x < stops.size(); ++x) {
                    ST u = stop_station[stops[x-1]];
                    ST v = stop_station[stops[x]];
                    T t = t_max;
                    for (int y = 0; y < trips.size(); ++y) {
                        if (trips[y][x-1].second >= t_from
                            && trips[y][x].first <= t_to) {
                            T dt = trips[y][x].first - trips[y][x-1].second;
                            t = std::min(t, dt);
                        }
                    }
                    if (t < t_max) {
                        edg.push_back(graph::edge(u, v, t));
                    }
                }
            }

            lowerboundgraph.set_edges(edg);
            std::cerr <<"lower-bound graph "<< lowerboundgraph.n()
                      <<" nodes, "<< lowerboundgraph.m() <<" edges\n";
        }
    }

    // Hack for last departure time requests through EAT query:
    void reverse_time() {
        for (R r = 0; r < n_r; ++r) {
            // stop sequence :
            rev_vector(route_stops[r]);
            for (int i = 0; i < route_stops[r].size(); ++i) {
                S s = route_stops[r][i];
                stop_route[s] = std::make_pair(r, i);
            }
            //trips :
            rev_vector(trips_of[r]);
            for (int a = 0; a < trips_of[r].size(); ++a) {
                std::vector<std::pair<T,T> > &trip = trips_of[r][a];
                rev_vector(trip);
                for (int i = 0; i < trip.size(); ++i) {
                    T rev_dep = - trip[i].first, rev_arr = - trip[i].second;
                    trip[i] = std::make_pair(rev_arr, rev_dep);
                    S s = route_stops[r][i];
                    stop_departures[s][a] = rev_dep;
                    stop_arrivals[s][a] = rev_arr;
                }
            }
        }
        // graphs
        transfers = transfers.reverse();
        transfers.sort_neighbors_by_weight();
        graph tmpin = inhubs;
        inhubs = outhubs.reverse();
        inhubs.sort_neighbors_by_weight();
        outhubs = tmpin.reverse();
        outhubs.sort_neighbors_by_weight();
        check();
    }

private:
    void build(std::unordered_map<id, std::vector<std::tuple<T, T, id, int> > > &trip_seq,
               std::vector<std::tuple<id, id, T> > &transf,
               bool symmetrize) {
        auto create_station = [this](id st) {
            if (id_to_station.find(st) == id_to_station.end()) {
                id_to_station[st] = n_st++;
                station_id.push_back(st);
                station_stops.push_back(std::vector<S>{});
            }
        };

        std::vector<std::vector<std::tuple<T, T, id, int> > > inter;
        inter.reserve(trip_seq.size());
        for (auto seq : trip_seq) {
            auto s = seq.second;
            // sort trip according to stop_sequence:
            std::sort(s.begin(), s.end(),
                      [](const std::tuple<T, T, id, int> &a,
                         const std::tuple<T, T, id, int> &b) {
                          return std::get<3>(a) < std::get<3>(b);
                      });
            // check time increases along trip:
            for (int i = 0; i < s.size(); ++i) {
                const std::tuple<T, T, id, int> &a = s[i];
                assert(std::get<0>(a) <= std::get<1>(a));
                if (i+1 < s.size()) {
                    const std::tuple<T, T, id, int> &b = s[i+1];
                    if (std::get<1>(a) > std::get<0>(b)) {
                        std::cerr << "decr time in "<< seq.first <<" : "
                                  << std::get<2>(a) <<","<< std::get<3>(a) <<"\n";
                    }
                    assert(std::get<1>(a) <= std::get<0>(b));
                }
            }
            inter.push_back(s);
        }
        // sort trips
        std::sort(inter.begin(), inter.end(),
                  [](const std::vector<std::tuple<T, T, id, int> > &a,
                     const std::vector<std::tuple<T, T, id, int> > &b) {
                      return std::get<1>(a.at(0)) < std::get<1>(b.at(0));
                  });
        // construct timetables
        std::map<std::vector<id>, R> stations_to_route, stations_to_route2;
        int n_overpass = 0;
        for (auto s : inter) {
            // partition trips into routes with same stop sequence
            std::vector<id> stations;
            stations.reserve(s.size());
            for (int i = 0; i < s.size(); ++i) {
                const std::tuple<T, T, id, int> &a = s[i];
                stations.push_back(std::get<2>(a));
            }
            R rte = -1;
            bool overpass = false, new_route = true;
            auto search = stations_to_route.find(stations);
            if (search != stations_to_route.end()) {
                new_route = false;
                rte = search->second;
                assert(route_stops[rte].size() == s.size());
                const auto &prev = trips_of[rte].back();
                assert(prev[0].second <= std::get<1>(s[0]));
                for (int i = 0; i < s.size(); ++i) {
                    T arr = std::get<0>(s[i]);
                    T dep = std::get<1>(s[i]);
                    if (arr < prev[i].first || dep < prev[i].second) {
                        overpass = true;
                        break;
                    }
                }
            }
            if (overpass) {
                search = stations_to_route2.find(stations);
                if (search != stations_to_route2.end()) {
                    new_route = false;
                    rte = search->second;
                    assert(route_stops[rte].size() == s.size());
                } else {
                    new_route = true;
                    ++n_overpass;
                }
            }
            if (new_route) {
                rte = n_r++;
                if (overpass) { stations_to_route2[stations] = rte; }
                else { stations_to_route[stations] = rte; }
                // create stations:
                for (auto st : stations) {
                    create_station(st);
                }
                // create a stop for each station in the new route:
                std::vector<S> stops(stations.size());
                for (int i = 0; i < stations.size(); ++i) {
                    ST st = id_to_station[stations[i]];
                    S s = n_s++;
                    station_stops[st].push_back(s);
                    stop_station.push_back(st);
                    stops[i] = s;
                    stop_route.push_back(std::make_pair(rte, i));
                }
                route_stops.push_back(stops);
                trips_of.push_back(std::vector<std::vector<std::pair<T,T> > >{});
            }
            // create trip in route table:
            std::vector<std::pair<T,T> > trp{s.size()};
            for (int i = 0; i < s.size(); ++i) {
                T arr = std::get<0>(s[i]);
                T dep = std::get<1>(s[i]);
                trp[i] = std::make_pair(arr, dep);
            }
            trips_of[rte].push_back(trp);
        }
        std::cerr << n_st <<" stations, "<< n_s <<" stops, "
                  << n_r <<" routes ("<< n_overpass <<" for overpasses)\n";

        //for (ST u = 0; u < n_st; ++u) std::cout << station_id[u] <<"\n";
        
        n_overpass = 0;
        for(R rte = 0; rte < n_r; ++rte) {
            std::sort(trips_of[rte].begin(), trips_of[rte].end(),
                      [](const std::vector<std::pair<T,T> > &p,
                         const std::vector<std::pair<T,T> > &q) {
                          for (int i = 0; i < p.size(); ++i) {
                              if (p[i].second < q[i].second) // departs before
                                  return true;
                              if (p[i].second > q[i].second) // departs after
                                  return false;
                          }
                          return false; // equality
                      });
            for(int i = 1; i < trips_of[rte].size() ; ++i) {
                // check trip does not overpass:
                bool overpass = false;
                const auto &prev = trips_of[rte][i-1];
                auto &trp = trips_of[rte][i];
                for (int i = 0; i < trp.size(); ++i) {
                    if (trp[i].second < prev[i].second // departs before
                        || trp[i].first < prev[i].first) { // arrives before
                            overpass = true;
                            // quick fix :
                            trp[i].second = std::max(trp[i].second,
                                                     prev[i].second);
                            trp[i].first = std::max(trp[i].first,
                                                    prev[i].first);
                    }
                }
                if(overpass) {
                    ++n_overpass;
                    std::cerr << "overpass in " << rte <<" : ";
                    for (S u : route_stops[rte]) {
                        std::cerr << station_id[stop_station[u]] <<" ";
                    }
                    std::cerr <<"\n";
                }
            }
        }
        if (n_overpass > 0) {
            std::cerr <<"WARNING: timetable modified to fix "
                      << n_overpass <<" remaining overpasses\n";
        }

        stop_departures.reserve(n_s);
        stop_arrivals.reserve(n_s);
        for (S s = 0; s < n_s; ++s) {
            R rte = stop_route[s].first;
            int is = stop_route[s].second;
            assert(is < route_stops[rte].size());
            int ntrips = trips_of[rte].size();
            std::vector<T> deps, arrs;
            deps.reserve(ntrips);
            arrs.reserve(ntrips);
            for (int i = 0; i < ntrips; ++i) {
                assert(trips_of[rte][i].size() == route_stops[rte].size());
                arrs.push_back(trips_of[rte][i][is].first);
                deps.push_back(trips_of[rte][i][is].second);
            }
            stop_arrivals.push_back(arrs);
            stop_departures.push_back(deps);
        }

        // transfers :
        std::vector<graph::edge> st_transf;
        st_transf.reserve(transf.size());
        assert(n_st == station_id.size());
        for (auto tr : transf) {
            id from = std::get<0>(tr);
            id to = std::get<1>(tr);
            ST st_from, st_to;
            if (id_to_station.find(from) == id_to_station.end()) {
                st_from = n_st + n_tr++;
                station_id.push_back(from);
                id_to_station[from] = st_from;
            } else { st_from = id_to_station[from]; }
            if (id_to_station.find(to) == id_to_station.end()) {
                st_to = n_st + n_tr++;                
                station_id.push_back(to);
                id_to_station[to] = st_to;
            } else { st_to = id_to_station[to]; }
            T delay = std::get<2>(tr);
            st_transf.push_back(graph::edge(st_from, st_to, delay));
        }
        transfers.set_edges(st_transf, n_st + n_tr);
        if (symmetrize) { // add missing reverse links
            graph r = transfers.reverse();
            for (ST u : r) {
                for (auto e : r[u]) {
                    if ( ! r.has_edge(e.dst, u)) {
                        st_transf.push_back(graph::edge(u, e.dst, e.wgt));
                        //std::cout << station_id[u] <<","<< station_id[e.dst]
                        //          <<","<< e.wgt <<"\n";
                    }
                }
            }
            std::cout.flush();
            transfers.set_edges(st_transf, n_st + n_tr);
        }
        //transfers = transfers.simple();
        std::cerr << transfers.m() << " transfers with "
                  << n_st <<" + "<< n_tr <<" nodes ";
        size_t n_asym = transfers.asymmetry(false);
        if (n_asym == 0) {
            n_asym = transfers.asymmetry(true);
            std::cerr <<"(symmetric: "
                      << n_asym <<" reverse weights differ)\n";
        } else {
            std::cerr <<"(asymmetric: "
                      << n_asym <<" reverse links miss)\n";
        }
        transfers.sort_neighbors_by_weight();

        check();

    }

public:
    void check() {
        assert(station_stops.size() == n_st);
        assert(stop_station.size() == n_s);
        for (ST st = 0; st < n_st; ++st) {
            for (S u : station_stops[st]) { assert(stop_station[u] == st); }
        }
        assert(trips_of.size() == n_r);
        assert(route_stops.size() == n_r);
        assert(stop_route.size() == n_s);
        assert(stop_departures.size() == n_s);
        assert(stop_arrivals.size() == n_s);
        for (S u = 0; u < n_s; ++u) {
            R r = stop_route[u].first;
            int i = stop_route[u].second;
            assert(stop_departures[u].size() == trips_of[r].size());
            assert(stop_arrivals[u].size() == trips_of[r].size());
            assert(i < route_stops[r].size());
            assert(route_stops[r][i] == u);
            for (int p = 0; p < trips_of[r].size(); ++p) {
                assert(trips_of[r][p][i].first == stop_arrivals[u][p]);
                assert(trips_of[r][p][i].second == stop_departures[u][p]);
            }
        }
        for (R r = 0; r < n_r; ++r) {
            for (S u : route_stops[r]) {
                assert(u >= 0 && u < n_s);
                assert(stop_route[u].first == r);
            }
        }
    }

    
    static std::set<id> services_at(std::string day, std::string date,
                                    std::string calendar,
                                    std::string calendar_dates) {
        std::set<id> services;
        // read calendar.txt file
        for (auto r : read_csv(calendar, 4,
                               "service_id", day.c_str(),
                               "start_date", "end_date")) {
            if (r[1] == "1" && r[2] <= date && date <= r[3]) {
                services.insert(r[0]);
            }
        }
        // read calendar_dates.txt file
        if (std::string{""} != calendar_dates) {
            for (auto r : read_csv(calendar_dates, 3,
                                   "service_id", "date", "exception_type")) {
                if (r[1] == date) {
                    if (r[2] == "1") services.insert(r[0]);
                    else if (r[2] == "2") services.erase(r[0]);
                    else assert(false);
                }
            }
        }
        return services;
    }

    static std::set<id> trips_served(const std::set<id> &services,
                                     std::string tripsfile) {
        std::set<id> trips;
        // read trips.txt file
        for (auto r : read_csv(tripsfile, 2, "service_id", "trip_id")) {
            if (services.find(r[0]) != services.end()) trips.insert(r[1]);
        }
        return trips;
    }

    static std::vector<std::tuple<id, id, T> >
    station_transfers(std::string transfersfile) {
        std::vector<std::tuple<id, id, T> > transf;
        for (auto r : read_csv(transfersfile, 4,
                               "from_stop_id", "to_stop_id", "transfer_type",
                               "min_transfer_time")) {
            assert(r[2] == "2");
            T t = std::stoi(r[3]);
            transf.push_back(std::make_tuple(r[0], r[1], t));
        }
        return transf;
    }

    static std::vector<std::tuple<id, id, T> >
    station_transfers_2(std::string transfersfile) {
        std::vector<std::tuple<id, id, T> > transf;
        for (auto r : read_csv(transfersfile, 3,
                               "from_stop_id", "to_stop_id",
                               "min_transfer_time")) {
            T t = std::stoi(r[2]);
            transf.push_back(std::make_tuple(r[0], r[1], t));
        }
        return transf;
    }

    static std::unordered_map<id, std::vector<std::tuple<T, T, id, int> > >
    trips_sequences(std::string stop_times,
                    const std::set<id> &trips = std::set<id>()) {
        std::unordered_map<id, std::vector<std::tuple<T, T, id, int> > >
            trip_seq;
        for (auto r : read_csv(stop_times, 5,
                                "trip_id", "arrival_time", "departure_time",
                                "stop_id", "stop_sequence")) {
            std::string trp = r[0];
            if (trips.empty() || trips.find(trp) != trips.end()) {
                T arr = time_of_string(r[1]);
                T dep = time_of_string(r[2]);
                id stp = r[3];
                int seq = std::stoi(r[4]);
                trip_seq[trp].push_back(std::make_tuple(arr, dep, stp, seq));
            }
        }
        return trip_seq;
    }

    static std::unordered_map<id, std::vector<std::tuple<T, T, id, int> > >
    trips_sequences_oneday(std::string stop_times) {
        std::unordered_map<id, std::vector<std::tuple<T, T, id, int> > >
            trip_seq;
        for (auto r : read_csv(stop_times, 5,
                                "trip_id", "arrival_time", "departure_time",
                                "stop_id", "stop_sequence")) {
            std::string trp = r[0];
            {
                T arr = std::stoi(r[1]);
                T dep = std::stoi(r[2]);
                id stp = r[3];
                int seq = std::stoi(r[4]);
                trip_seq[trp].push_back(std::make_tuple(arr, dep, stp, seq));
            }
        }
        return trip_seq;
    }

    static T time_of_string(const std::string &str) {
        auto v = split(str, ':');
        if (v.size() != 3) std::cerr << "buggy time : "<< str <<"\n";
        assert(v.size() == 3);
        int h = std::stoi(v[0]), m = std::stoi(v[1]), s = std::stoi(v[2]);
        return h * 3600 + m * 60 + s;
    }
    
public:
    static std::vector<std::vector<std::string> >
    read_tuples(const std::string filename, const size_t ncols) {
        std::vector<std::vector<std::string> > rows;
        FILE *in = filename != "-" ? fopen(filename.c_str(), "r") : stdin;
        assert(in != nullptr);
        char line[100000];

        for ( ; fgets(line, sizeof line, in) != NULL ; ) {
            auto v = split(line, ' ');
            if (v.size() != ncols) {
                std::cerr <<"wrong ncols : '"<< line <<"'\n";
                assert(v.size() == ncols);
            }
            rows.push_back(v);
        }
        return rows;
    }
    
    static std::vector<std::vector<std::string> >
    read_tuples_2(const std::string filename, const size_t ncols) {
        std::vector<std::vector<std::string> > rows;
        gzFile gz_in; FILE *in;
        bool gzipped = filename.size() > 3
                       && filename.substr(filename.size() - 3) == ".gz";
        std::cerr << filename <<" "<< gzipped <<"\n";
        if (gzipped) {
            gz_in = gzopen(filename.c_str(), "r");
        } else {
            in = filename != "-" ? fopen(filename.c_str(), "r") : stdin;
        }
        assert(gzipped ? gz_in  != nullptr : in  != nullptr);
        char line[100000];

        for ( ; (gzipped ? gzgets(gz_in, line, sizeof line)
                         : fgets(line, sizeof line, in)) != NULL ; ) {
            auto v = split(line, ' ');
            if (v.size() != ncols) {
                std::cerr <<"wrong ncols : '"<< line <<"'\n";
                assert(v.size() == ncols);
            }
            rows.push_back(v);
        }
        if (gzipped) {
            gzclose(gz_in);
        } else {
            fclose(in);
        }
        return rows;
    }

    static std::vector<std::vector<std::string> >
    read_csv(const std::string filename, const size_t ncol, ...) { // ... = column names
        std::vector<std::vector<std::string> > rows;
        FILE *in = filename != "-" ? fopen(filename.c_str(), "r") : stdin;
        assert(in != nullptr);
        char line[100000];
        bool first = true;

        std::vector<std::string> colnames(ncol);
        va_list args;
        va_start(args, ncol);
        for (int j = 0; j < ncol; ++j) {
            colnames[j] = va_arg(args, char *);
        }
        va_end(args);

        std::vector<int> cols(ncol, -1);
        for ( ; fscanf(in, " %s \n", line) >= 1 ; ) {
            if (first) {
                first = false;
                int i = 0;
                for (auto s : split(line, ',')) {
                    for (int j = 0; j < ncol; ++j) {
                        if (s == colnames[j]) cols[j] = i;
                    }
                    ++i;
                }
                for (int j = 0; j < ncol; ++j) {
                    if (cols[j] < 0)
                        throw std::invalid_argument("missing column : "
                                                    + colnames[j]);
                }
            } else {
                auto v = split(line, ',');
                std::vector<std::string> r(ncol);
                for (int j = 0; j < ncol; ++j) {
                    assert(cols[j] < v.size());
                    r[j] = v[cols[j]];
                }
                rows.push_back(r);
            }
        }
        return rows;
    }

    static std::vector<std::vector<std::string> >
    read_csv_2(const std::string filename, const size_t ncol, ...) { // ... = column names
        std::vector<std::vector<std::string> > rows;
        FILE *in = filename != "-" ? fopen(filename.c_str(), "r") : stdin;
        assert(in != nullptr);
        char line[100000];
        bool first = true;

        std::vector<std::string> colnames(ncol);
        va_list args;
        va_start(args, ncol);
        for (int j = 0; j < ncol; ++j) {
            colnames[j] = va_arg(args, char *);
        }
        va_end(args);

        std::vector<int> cols(ncol, -1);
        for ( ; fscanf(in, " %s \n", line) >= 1 ; ) {
            if (first) {
                first = false;
                int i = 0;
                for (auto s : split(line, ',')) {
                    for (int j = 0; j < ncol; ++j) {
                        if (s == colnames[j]) cols[j] = i;
                    }
                    ++i;
                }
                for (int j = 0; j < ncol; ++j) {
                    if (cols[j] < 0)
                        throw std::invalid_argument("missing column : "
                                                    + colnames[j]);
                }
            } else {
                auto v = split(line, ',');
                std::vector<std::string> r(ncol);
                for (int j = 0; j < ncol; ++j) {
                    assert(cols[j] < v.size());
                    r[j] = v[cols[j]];
                }
                rows.push_back(r);
            }
        }
        return rows;
    }

    static std::vector<std::string> split(const std::string &s,
                                          const char delim) {
        std::vector<std::string> v;
        /*
        std::string field{""};
        for (auto c : s) {
            if (c != delim) field += c;
            else { v.push_back(field); field = ""; }
        }
        v.push_back(field);
        */
        size_t pos = 0, pos_prev = 0;
        while ((pos = s.find(delim, pos_prev)) != std::string::npos) {
            v.push_back(s.substr(pos_prev, pos - pos_prev));
            pos_prev = pos + 1;
        }
        v.push_back(s.substr(pos_prev, s.size() - pos_prev));
        return v;
    };

    template<typename T>
    static void rev_vector(std::vector<T> &v) {
        int l = 0, r = v.size() - 1;
        while (l < r) {
            std::swap(v[l],v[r]);
            ++l; --r;
        }
    }

    static T decimeters_to_seconds(const int d) {
        return (T)(std::lround(3600.0 * d / (4 * 10000))); // 4 Km/h
    }
};


#endif // TIMETABLE_HH
