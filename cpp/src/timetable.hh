#ifndef TIMETABLE_HH
#define TIMETABLE_HH

#include <assert.h>
#include <limits>
#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <cstdarg>
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

    const int t_min = 0, t_max = std::numeric_limits<int>::max();

    typedef std::string id;
    typedef mgraph<ST, T> graph;

    size_t n_st, n_s, n_r; // number of stations, stops and routes
    std::vector<std::vector<S> > station_stops; // stops of a station
    std::vector<id> station_id; // id from the gtfs data
    std::vector<ST> stop_station;
    std::vector<std::pair<R, int> > stop_route; // route at a stop, and index of stop in the route sequence
    std::vector<std::vector<T> > stop_departures; // departure time
    std::vector<std::vector<S> > route_stops; // stop sequence of a route
    std::vector<std::vector<std::vector<std::pair<T,T> > > > trips_of; // trips of a route : arrival/departure times at each stop
    graph transfers;
    
    std::map<std::vector<id>, R> stations_to_route;
    std::map<id, ST> id_to_station;

public:
    timetable(std::string day, std::string date,
              std::string calendar, std::string calendar_dates,
              std::string tripsfile, std::string stop_times,
              std::string transfersfile, bool symmetrize = true)
        : n_st(0), n_s(0), n_r(0),
          station_stops(), station_id(),
          stop_route(), stop_departures(),
          route_stops(), trips_of(), transfers(),
          stations_to_route(), id_to_station() 
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
        std::cerr << transf.size() <<" transfers\n";
        build(trip_seq, transf, symmetrize);
    }

    timetable(std::string stop_times,
              std::string transfersfile, bool symmetrize = true)
        : n_st(0), n_s(0), n_r(0),
          station_stops(), station_id(),
          stop_route(), stop_departures(),
          route_stops(), trips_of(), transfers(),
          stations_to_route(), id_to_station() 
    {
        auto trip_seq = trips_sequences_oneday(stop_times);
        int events = 0;
        for (auto seq : trip_seq) events += seq.second.size();
        std::cerr << events <<" events\n";
        auto transf = station_transfers_2(transfersfile);
        std::cerr << transf.size() <<" transfers\n";
        build(trip_seq, transf, symmetrize);
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
        
        int n_overpass = 0;
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
            // partition trips into routes with same stop sequence
            std::vector<id> stations(s.size());
            for (int i = 0; i < s.size(); ++i) {
                const std::tuple<T, T, id, int> &a = s[i];
                stations[i] = std::get<2>(a);
            }
            R rte = -1;
            auto search = stations_to_route.find(stations);
            if (search == stations_to_route.end()) {
                rte = n_r++;
                stations_to_route[stations] = rte;
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
            } else {
                rte = search->second;
                assert(route_stops[rte].size() == s.size());
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
        std::cerr << n_st <<" stations, "<< n_s <<" stops, "<< n_r <<" routes\n";

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
        std::cerr << n_overpass <<" overpasses\n";

        stop_departures.reserve(n_s);
        for (S s = 0; s < n_s; ++s) {
            R rte = stop_route[s].first;
            int is = stop_route[s].second;
            assert(is < route_stops[rte].size());
            int ntrips = trips_of[rte].size();
            std::vector<T> deps;
            deps.reserve(ntrips);
            for (int i = 0; i < ntrips; ++i) {
                assert(trips_of[rte][i].size() == route_stops[rte].size());
                deps.push_back(trips_of[rte][i][is].second);
            }
            stop_departures.push_back(deps);
        }

        // transfers :
        std::vector<graph::edge> st_transf;
        st_transf.reserve(transf.size());
        for (auto tr : transf) {
            id from = std::get<0>(tr);
            id to = std::get<1>(tr);
            create_station(from);
            create_station(to);
            ST st_from = id_to_station[from];
            ST st_to = id_to_station[to];
            T delay = std::get<2>(tr);
            st_transf.push_back(graph::edge(st_from, st_to, delay));
            if (symmetrize)
                st_transf.push_back(graph::edge(st_to, st_from, delay));
        }
        transfers.set_edges(st_transf, n_st);

        // check
        assert(station_stops.size() == n_st);
        assert(stop_station.size() == n_s);
        for (ST st = 0; st < n_st; ++st) {
            for (S u : station_stops[st]) { assert(stop_station[u] == st); }
        }
        assert(trips_of.size() == n_r);
        assert(route_stops.size() == n_r);
        assert(stop_route.size() == n_s);
        assert(stop_departures.size() == n_s);
        for (S u = 0; u < n_s; ++u) {
            R r = stop_route[u].first;
            int i = stop_route[u].second;
            assert(stop_departures[u].size() == trips_of[r].size());
            assert(i < route_stops[r].size());
            assert(route_stops[r][i] == u);
            assert(trips_of[r].size() == stop_departures[u].size());
            for (int p = 0; p < trips_of[r].size(); ++p) {
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
    read_csv(const std::string filename, const size_t ncol, ...) { // ... = column names
        std::vector<std::vector<std::string> > rows;
        FILE *in = filename != "-" ? fopen(filename.c_str(), "r") : stdin;
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
};


#endif // TIMETABLE_HH
