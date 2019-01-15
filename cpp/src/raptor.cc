#include <iostream>

#include "timetable.hh"
#include "raptor.hh"
#include "connection_scan.hh"
#include "logging.hh"
#include "file_util.hh"

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

typedef pareto_rev<int> pset;

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
    timetable ttbl{dir+"stop_times.csv.gz",
            dir+"in_hubs.gr.gz", dir+"out_hubs.gr.gz",
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


    // ------------------ read queries -------------------
    int n_q = 0;
    std::vector<std::tuple<int, int, int> > queries;
    if (get_opt(argc, argv, "-query-file=", "queries.csv") != "") {
        auto rows = read_csv
            (dir + get_opt(argc, argv, "-query-file=", "queries.csv"),
             3, "source", "target", "time");
        for (auto r : rows) {
            if (has_opt(argc, argv, "-1") && n_q >= 1) break;
            if (has_opt(argc, argv, "-10") && n_q >= 10) break;
            if (has_opt(argc, argv, "-100") && n_q >= 100) break;
            int src = ttbl.id_to_station[r[0]];
            int dst = ttbl.id_to_station[r[1]];
            int t = std::stoi(r[2]);
            queries.push_back(std::make_tuple(src, dst, t));
            ++n_q;
        }
        main_log.cerr(t) << n_q << " queries\n";
        t = main_log.lap();
    }
    // */


    // make andom successful queries
    if (get_opt(argc, argv, "-random-queries=", "") != "") {
        n_q = std::stoi(get_opt(argc, argv, "-random-queries=", "1000"));
        int max_delay = std::stoi(get_opt(argc, argv, "-max-delay=",
                                          std::to_string(ttbl.t_max)));
        int t_beg = 0*3600, t_end = 24*3600;
        int n_try = 0, n_err = 0;
        while (queries.size() < n_q) {
            ++n_try;
            int src = rand() % ttbl.n_st;
            int dst = rand() % ttbl.n_st;
            int t = t_beg + rand() % (t_end - t_beg);
            int arr = csa.earliest_arrival_time(src, dst, t, false, true,
                                                 chg);
            if (arr <= t + max_delay) {
                ++n_ok;
                queries.push_back(std::make_tuple(src, dst, t));
            }
        }
        main_log.cerr(t) << n_q <<" Random queries, success rate : "
                         << (n_q*100/n_try) <<"%\n";
        
        t = main_log.lap();
    }
    // */


    //* Print a journey
    if (has_opt(argc, argv, "-journey")) {
        int src = std::stoi(get_opt(argc, argv, "-src=", "4372"));
        int dst = std::stoi(get_opt(argc, argv, "-dst=", "5948"));
        int t = std::stoi(get_opt(argc, argv, "-t=", "32401"));
        bool hubs = has_opt(argc, argv, "-hubs");

        int arr = rpt.earliest_arrival_time(src, dst, t, hubs, ! hubs, chg);
        std::cout <<" -------- "<< (hubs ? "HL" : "") <<"Raptor "
                  << src << "=" << ttbl.station_id[src] 
                  <<" to "<< dst <<"="<< ttbl.station_id[dst]
                  <<" at "<< t <<" : EAT="<< arr
                  <<" ("<< rpt.nb_trips_to(dst) <<")"
                  <<"\n";
        rpt.print_journey(dst, std::cout, chg);

        int dep = rev_rpt.earliest_arrival_time(dst, src, -arr,
                                                hubs, ! hubs, 0, chg);
        std::cout <<" -------- "<< (hubs ? "HL" : "") <<"RaptorREV "
                  << dst << "=" << ttbl.station_id[dst] 
                  <<" to "<< src <<"="<< ttbl.station_id[src]
                  <<" at "<< arr <<" : EAT="<< - dep <<"\n";
        rev_rpt.print_journey(src, std::cout, 0, chg);
        
        arr = csa.earliest_arrival_time(src, dst, t, hubs, ! hubs, chg);
        std::cout <<" -------- "<< (hubs ? "HL" : "") <<"CSA "
                  << src << "=" << ttbl.station_id[src] 
                  <<" to "<< dst <<"="<< ttbl.station_id[dst]
                  <<" at "<< t <<" : EAT="<< arr <<"\n";
        csa.print_journey(dst, hubs, ! hubs, std::cout, chg);
    }

    //* Arrival times
    if (has_opt(argc, argv, "-arrival-times")) {
        std::cout <<"src,dst,tdep,eat,eat_Unrestricted_Walking,eat_Walk_Only\n";
        n_ok = 0;
        for (auto q : queries) {
            int src = std::get<0>(q);
            int dst = std::get<1>(q);
            int t = std::get<2>(q);
            int arr1 = rpt.earliest_arrival_time(src, dst, t, false, true, chg);
            int arr2 = csa.earliest_arrival_time(src, dst, t, false, true, chg);
            // assert(arr1 == arr2); // can fail if chg == 0
            int arrHL1 = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg);
            int arrHL2 = csa.earliest_arrival_time(src, dst, t, hub, trf, chg);
            //assert(arrHL1 == arrHL2); // can fail if chg == 0
            std::cout << ttbl.station_id[src] <<","<< ttbl.station_id[dst]
                      <<","<< t <<","<< arr1 <<","<< arrHL1
                      <<","<< (t + rpt.walking_time(src, dst)) <<"\n";
            std::cout.flush();
            ++n_ok;
        }
        main_log.cerr(t) << n_ok << " arrival times\n";
        t = main_log.lap();
    }
    // */

    
    // go profile CSA
    sum = 0, n_ok = 0;
    bool prescan = has_opt(argc, argv, "-csa-profile-prescan");
    int t_get = std::stoi(get_opt(argc, argv, "-t-beg=", "0"));
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        std::cout << src <<"(="<< ttbl.station_id[src] <<") "
                  << dst <<"(="<< ttbl.station_id[dst] <<") " <<" : ";
        pset prof = csa.profile(src, dst, 0, 24*3600, false, true,
                                 chg, 0, km, prescan);
        int ntrips = prof.size();
        std::cout << ntrips <<"\n";
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile CSA queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */    


    // go profile Raptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        std::cout << src <<" "<< dst <<" : ";
        pset prof = rpt.profile(rev_rpt, src, dst, 0, 24*3600,
                                 false, true, chg);
        int ntrips = prof.size();
        std::cout << ntrips <<"\n";
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile Raptor queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();


    if (has_opt(argc, argv, "-exit-pr")) exit(0);

    
    // go profile HLCSA
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        std::cout << src <<" "<< dst <<" : ";
        pset prof = csa.profile(src, dst, 0, 24*3600, hub, trf,
                                 chg, 0, km, prescan);
        int ntrips = prof.size();
        std::cout << ntrips <<"\n";
        if (src == std::stoi(get_opt(argc, argv, "-src=", "-1")))
            prof.print();
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile HLCSA queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */    

    
    // go profile HLRaptor
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        std::cout << src <<" "<< dst <<" : ";
        pset prof = rpt.profile(rev_rpt, src, dst, 0, 24*3600,
                                 hub, trf, chg);
        int ntrips = prof.size();
        std::cout << ntrips <<"\n";
        if (src == std::stoi(get_opt(argc, argv, "-src=", "-1")))
            prof.print();
        sum += ntrips;
        ++n_ok;
    }
    main_log.cerr(t) << n_q << " Profile HLRaptor queries done, avg_ntrips = "
                     << (sum / n_ok)
                     << "  "<< n_ok <<"/"<< queries.size() <<" ok\n";
    t = main_log.lap();
    // */    


    
    if (has_opt(argc, argv, "-exit")) exit(0);
    
        
    
    // go Raptor restricted walk
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int arr = rpt.earliest_arrival_time(src, dst, t, false, true, chg);
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
        int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg);
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
        int arr = csa.earliest_arrival_time(src, dst, t, false, true, chg);
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
        int arr = csa.earliest_arrival_time(src, dst, t, hub, trf, chg);
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
        int arr = rpt.earliest_arrival_time(src, dst, t, false, true, 0);
        int dep = - rev_rpt.earliest_arrival_time(dst, src, - arr, false, true, 0);
        int arr2 = rpt.earliest_arrival_time(src, dst, dep, false, true, 0);
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


    //* go Pareto
    sum = 0, n_ok = 0;
    for (auto q : queries) {
        int src = std::get<0>(q);
        int dst = std::get<1>(q);
        int t = std::get<2>(q);
        int npath = rpt.earliest_walk_pareto(src, dst, t, false, true, chg);
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
        //int arr = rpt.earliest_arrival_time(src, dst, t, hub, trf, chg);
        int npath = rpt.earliest_walk_pareto(src, dst, t, hub, trf, chg);
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



    // ------------------------ end -------------------------
    main_log.cerr() << "end "<< dir <<"\n";

}
