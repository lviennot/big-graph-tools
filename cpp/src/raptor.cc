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
              <<" [options] gtfs_dir\n"
              <<"Options:\n"
              << paragraph("  -fast : at most 10 queries")
              <<"\n";
    exit(1);
}

std::vector<std::string> get_args(int argc, char **argv) {
    std::vector<std::string> a;
    for (int i = 0; i < argc; ++i) {
        std::string s(argv[i]);
        if (s[0] == '-') continue; // option
        a.push_back(s);
    }
    return a;
}

bool has_opt(int argc, char **argv, std::string opt) {
    assert(opt[0] == '-');
    for (int i = 0; i < argc; ++i) {
        std::string s(argv[i]);
        if (s == opt) return true;
    }
    return false;
}

std::string get_opt(int argc, char **argv,
                    std::string opt_prefix, std::string dft) {
    int len = opt_prefix.size();
    assert(opt_prefix[0] == '-'
           && (opt_prefix[len-1] == '=' || opt_prefix[len-1] == ':'));
    for (int i = 0; i < argc; ++i) {
        std::string s(argv[i]);
        if (s.size() >= len && s.substr(0, len) == opt_prefix) {
            return s.substr(len);
        }
    }
    return dft;
}

int get_int_opt(int argc, char **argv,
                std::string opt_prefix, int dft) {
    return std::stoi(get_opt(argc, argv, opt_prefix, std::to_string(dft)));
}

int main (int argc, char **argv) {
    logging main_log("--");

    // ------------------------ usage -------------------------
    std::vector<std::string> args = get_args(argc, argv);
    if (args.size() != 2) {
        usage_exit(argv);
    }
    std::string dir{args[1]};
    std::cerr <<"--------------- "<< dir <<" ---------------------\n";
    dir += "/";

    // ------------------------ time -------------------------
    main_log.cerr() << "start\n";
    double t = main_log.lap();

    //std::cerr <<"first rands: "<< rand() <<" "<< rand() <<" "<< rand() <<"\n";


    // ------------------------- load timetable ----------------------
    /*
    timetable ttbl{args[1], args[2],
            dir+"calendar.txt", dir+"calendar_dates.txt",
            dir+"trips.txt", dir+"stop_times.txt", dir+"transfers.txt", true};
    */
    timetable ttbl{dir+"stop_times.csv",
            dir+"in_hubs.gr", dir+"out_hubs.gr",
            dir+"transfers.csv", true};
    //dir+"walking_and_transfers.gr", t_from, t_to};
    std::cerr << ttbl.n_r <<" routes, "<< ttbl.n_st <<" sations, "
              << ttbl.n_s <<" stops\n";
    main_log.cerr(t) << "timetable\n";
    t = main_log.lap();
    //exit(0);

    // --------------- earliest arrival time through Raptor and CSA ---------
    raptor rpt(ttbl);
    main_log.cerr(t) << "raptor initialized\n";
    t = main_log.lap();

    timetable rev_ttbl(ttbl);
    rev_ttbl.check();
    rev_ttbl.reverse_time();
    raptor rev_rpt(rev_ttbl);
    main_log.cerr(t) << "rev raptor initialized\n";
    t = main_log.lap();

    connection_scan csa(ttbl);
    main_log.cerr(t) << "csa initialized\n";
    t = main_log.lap();

    const bool hub=true, trf=false;
    const int chg=std::stoi(get_opt(argc, argv, "-min-change-time=", "60")),
        km=48;

    uint64_t sum = 0, n_ok = 0;


    // ------------------ random queries -------------------
    int n_q = 0;
    std::vector<std::tuple<int, int, int> > queries;
    {
        auto rows = timetable::read_csv
            (dir + get_opt(argc, argv, "-query-file=", "queries.csv"),
             3, "source", "target", "time");
        for (auto r : rows) {
            if (has_opt(argc, argv, "-fast") && n_q >= 10) break;
            int src = ttbl.id_to_station[r[0]];
            int dst = ttbl.id_to_station[r[1]];
            int t = std::stoi(r[2]);
            queries.push_back(std::make_tuple(src, dst, t));
            ++n_q;
        }
    }
    main_log.cerr(t) << n_q << " queries\n";
    t = main_log.lap();

    /*
    // make n_q successful queries
    t = main_log.lap();
    int n_q = 1000, t_beg = 0*3600, t_end = 24*3600;
    std::vector<std::tuple<int, int, int> > queries;
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
        //assert(arr1 <= arr2);
        if (arr2 < ttbl.t_max) { ++n_ok; }
        if (true || (arr1 < ttbl.t_max && arr2 < ttbl.t_max)) {
            sum += arr2 - arr1;
            queries.push_back(std::make_tuple(src, dst, t));
        }
    }
    main_log.cerr(t) <<"random query success rate : "
                     << (n_q*100/n_try) <<"% for "<< n_try <<" queries\n";
    std::cerr << n_ok <<" rpt==csa, E[eat_csa - eat_rpt] = "<< (sum/n_q) <<"\n";

    t = main_log.lap();
    */


    //* CHECK
    n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        if (has_opt(argc, argv, "-skip") && ttbl.station_id[src] != "4561")
            continue;
        src = ttbl.id_to_station[get_opt(argc, argv, "-src=",
                                         ttbl.station_id[src])];
        dst = ttbl.id_to_station[get_opt(argc, argv, "-dst=",
                                         ttbl.station_id[dst])];
        t = get_int_opt(argc, argv, "-t=", t);
        std::cerr << ttbl.station_id[src] <<","<< ttbl.station_id[dst]
                  <<","<< t <<" ("<< n_ok <<")\n";
        int arr1 = rpt.earliest_arrival_time(src, dst, t, false, true, chg, km);
        int arr2 = csa.earliest_arrival_time(src, dst, t, false, true, chg, km);
        if (arr1 != arr2) {
            std::cerr <<"---------- \n";
            rpt.print_journey(dst, std::cerr, -1, chg);
            std::cerr <<"---------- \n";
            csa.print_journey(dst, false, true, chg, std::cerr);
            std::cerr <<"--- \n";
            int u = get_int_opt(argc, argv, "-u=", 3837);
            std::cerr <<"EAT "<< u <<" at "<< csa.eat(u) <<"\n";;
            std::cerr <<"---------- \n";
            std::cerr << arr1 <<" "<< arr2 <<"\n";
        }
        assert(arr1 == arr2);
        int arrHL1 = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        int arrHL2 = csa.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        std::cerr << arrHL1 <<" "<< arrHL2 <<"\n";
        assert(arrHL1 == arrHL2);
        std::cout << ttbl.station_id[src] <<","<< ttbl.station_id[dst] <<","<< t
                  <<","<< arr1 <<","<< arrHL1
                  <<","<< rpt.walking_time(src, dst) <<"\n";
        ++n_ok;
    }
    std::cout.flush();
    main_log.cerr(t) << n_ok << " CHECK\n";
    t = main_log.lap();
    // */

    if (has_opt(argc, argv, "-exit")) exit(0);
    
    // go Raptor restricted walk
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int arr = rpt.earliest_arrival_time(src, dst, t, false, true, chg, km);
        //std::cout << arr <<"\n";
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


    // go HLRaptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        //if (arr < ttbl.t_max) rpt.print_journey(dst);
        //std::cout << src <<","<< dst <<","<< t <<" : "<< (arr - dep) <<"\n";
        //std::cout << arr <<"\n";
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
    
    // go CSA
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int arr = csa.earliest_arrival_time(src, dst, t, false, true, chg, km);
        //std::cout << arr <<"\n";
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
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
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


    // go last departure Raptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int arr = rpt.earliest_arrival_time(src, dst, t, false, true, 0, km);
        int dep = - rev_rpt.earliest_arrival_time(dst, src, - arr, false, true, 0, km);
        int arr2 = rpt.earliest_arrival_time(src, dst, dep, false, true, 0, km);
        //std::cout << t <<" "<< dep <<" "<< arr <<" "<< arr2 <<"\n";
        //assert(arr < ttbl.t_max);
        if (arr < ttbl.t_max) {
            assert(arr == arr2);
            sum += dep - t;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " last dep Raptor queries done, avg_dep_time = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    // go profile Raptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int ntrips = rpt.profile(rev_rpt, src, dst, 0, 24*3600,
                                 false, true, km);
        //std::cout << ntrips <<"\n";
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile Raptor queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    //* go Pareto
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int npath = rpt.earliest_walk_pareto(src, dst, t, false, true, chg, km);
        if (npath > 0) {
            sum += npath;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " Pareto Raptor queries done, avg_npaths = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */
    
    //* go HLPareto
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        //int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg, km);
        int npath = rpt.earliest_walk_pareto(src, dst, t, hub, trf, chg, km);
        if (npath > 0) {
            sum += npath;
            ++n_ok;
        }
    }
    main_log.cerr(t) << n_q << " Pareto HLRaptor queries done, avg_npaths = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */


    // go profile HLRaptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        //std::cout << src <<" "<< dst <<" :\n";
        int ntrips = rpt.profile(rev_rpt, src, dst, 0, 24*3600,
                                 hub, trf, km);
        //std::cout << ntrips <<"\n";
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile HLRaptor queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */    


    // ------------------------ end -------------------------
    main_log.cerr() << "end\n";

}
