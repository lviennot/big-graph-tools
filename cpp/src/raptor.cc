#include <iostream>

#include "timetable.hh"
#include "raptor.hh"
#include "connection_scan.hh"
#include "logging.hh"

void usage_exit (char **argv) {
    auto paragraph = [](std::string s, int width=80) -> std::string {
        std::string acc;
        while (s.size() > 0) {
            int pos = s.size();
            if (pos > width) pos = s.rfind(' ', width);
            std::string line = s.substr(0, pos);
            acc += line + "\n";
            s = s.substr(pos);
        }
        return acc;
    };
    
    std::cerr <<"Usage: "<< argv[0]
              <<" day date gtfs_dir\n";
    exit(1);
}


int main (int argc, char **argv) {
    logging main_log("--");

    // ------------------------ usage -------------------------
    if (argc != 4) {
        usage_exit(argv);
    }

    // ------------------------ time -------------------------
    main_log.cerr() << "start\n";
    double t = main_log.lap();

    int t_from = std::stoi(argv[1]), t_to = std::stoi(argv[2]);

    /* ------------------------- load csv ----------------------
    auto rows = timetable::read_csv(argv[1], 2, "service_id", "tuesday");
    for (auto r : rows) {
        for (auto c : r) std::cout << c <<" ";
        std::cout <<"\n";
    }
    main_log.cerr() << "csv\n";
    t = main_log.lap();
    */

    // ------------------------- load timetable ----------------------
    std::string dir{argv[3]};
    dir += "/";
    /*
    timetable ttbl{argv[1], argv[2],
            dir+"calendar.txt", dir+"calendar_dates.txt",
            dir+"trips.txt", dir+"stop_times.txt", dir+"transfers.txt"};
    */
    timetable ttbl{dir+"stop_times.csv",
            dir+"in_hubs.gr", dir+"out_hubs.gr",
            dir+"transfers.csv", true};
    //dir+"walking_and_transfers.gr", t_from, t_to};
    std::cerr << ttbl.n_r <<" routes, "<< ttbl.n_st <<" sations, "
              << ttbl.n_s <<" stops\n";
    /*
    int r = 43;
    std::cerr <<"route "<< r <<" : ";
    for (auto s : ttbl.route_stops[r]) {
        std::cerr << s <<","<< ttbl.stop_station[s] <<" ";
    }
    std::cerr <<"\n";
    int s = 19097;
    std::cerr <<"departures at "<< s <<" : ";
    for (int t : ttbl.stop_departures[s]) {
        std::cerr << t <<" ";
    }
    std::cerr <<"\n";
    int st = 116;
    for (int u : ttbl.station_stops[st]) {
        int r = ttbl.stop_route[u].first;
        int i = ttbl.stop_route[u].second;
        std::cerr <<"stop "<< u <<" in station "<< st
                  <<" at pos "<< i <<" in route "<< r
                  <<"\n";
    }
    std::cerr <<"\n";
    */
    main_log.cerr(t) << "timetable\n";
    t = main_log.lap();
    //exit(0);

    //std::cerr <<"first rands: "<< rand() <<" "<< rand() <<" "<< rand() <<"\n";

    // --------------- earliest arrival time through Raptor ---------
    raptor rpt(ttbl);
    main_log.cerr(t) << "raptor initialized\n";
    t = main_log.lap();
    /*
    std::cerr << rpt.earliest_arrival_time(2637, 967, 0) <<"\n";
    std::cerr << rpt.earliest_arrival_time(2543, 2549, 0) <<"\n";
    std::cerr << rpt.earliest_arrival_time(ttbl.id_to_station["3750014"],
                                           ttbl.id_to_station["5709848"],
                                           0) <<"\n";
    */
    connection_scan csa(ttbl);
    main_log.cerr(t) << "csa initialized\n";
    t = main_log.lap();

    std::cerr << dir <<"\n";

    //bool hub=false, trf=true;
    bool hub=true, trf=false;
    int chg=60, km=48;

    // make n_q successful queries
    t = main_log.lap();
    int n_q = 1000, t_beg = 5*3600, t_end = 21*3600;
    t_beg = t_from; t_end = t_to;
    std::vector<std::tuple<int, int, int> > queries;
    uint64_t sum = 0, n_ok = 0;
    int n_try = 0, n_err = 0;
    while (queries.size() < n_q) {
        ++n_try;
        int src = rand() % ttbl.n_st;
        int dst = rand() % ttbl.n_st;
        int t = t_beg + rand() % (t_end - t_beg);
        // 13890 -> 19202 at 10130
        //PB ferm trans. : src = 13890; dst = 19202; t = 10130;
        int arr1 = rpt.earliest_arrival_time(src, dst, t, false, true, chg, km);
        //int arr1 = rpt.earliest_walk_pareto(src, dst, t);
        int arr2 = csa.earliest_arrival_time(src, dst, t, false, true, chg, km);
        if (arr1 != arr2 && n_err++ < 10) {
            std::cerr <<" csa diff : "<< src <<" -> "<< dst <<" at "<< t
                      <<" : "<< arr1 <<", "<< arr2 <<"\n"; 
        }
        assert(arr1 <= arr2);
        if (arr2 < ttbl.t_max) { ++n_ok; }
        if (true || (arr1 < ttbl.t_max && arr2 < ttbl.t_max)) {
            sum += arr2 - arr1;
            queries.push_back(std::make_tuple(src, dst, t));
        }
    }
    main_log.cerr(t) <<"random query success rate : "
                     << (n_q*100/n_try) <<"% for "<< n_try <<" queries\n";
    std::cerr << n_ok <<" rpt==csa, E[eat_csa - eat_rpt] = "<< (sum/n_q) <<"\n";
    // go Pareto
    sum = 0, n_ok = 0;
    t = main_log.lap();
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        assert(t <= t_end);
        //int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        int arr = rpt.earliest_walk_pareto(src, dst, t, hub, trf, chg, km);
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            sum += arr - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " PHLRaptor queries done, avg_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();

    // go HLRaptor
    t = main_log.lap();
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        assert(t <= t_end);
        int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            sum += arr - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " HLRaptor queries done, avg_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();

    // go restricted walk
    t = main_log.lap();
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        assert(t <= t_end);
        int arr = rpt.earliest_arrival_time(src, dst, t, false, true, chg, km);
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            sum += arr - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " Raptor queries done, avg_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    // go CSA
    t = main_log.lap();
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        assert(t <= t_end);
        int arr = csa.earliest_arrival_time(src, dst, t, false, true, chg, km);
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            sum += arr - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " CSA queries done, avg_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    // go HLCSA
    t = main_log.lap();
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        assert(t <= t_end);
        int arr = csa.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            sum += arr - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " HLCSA queries done, avg_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    // ------------------------ end -------------------------
    main_log.cerr() << "end\n";

}
