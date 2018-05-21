#include <cstdlib>
#include <sys/time.h>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <string>

#include "mgraph.hh"
#include "traversal.hh"
#include "pruned_landmark_labeling.hh"

typedef mgraph<int64_t> graph;

double top (double t1, std::string msg) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    double t2 = tv.tv_sec + tv.tv_usec * 1e-6;
    std::cerr << "-- time " << msg << " : " << (t2 - t1) << "s\n";
    std::cerr.flush();
    return t2;
}

void usage_exit (char **argv) {
    std::cerr <<"Usage: "<< argv[0] <<" [command] [graph] [OPT [subset]]\n"
        "Computes a hub labeling of the graph G in file [graph] and\n"
        " outputs labels of selected nodes listed in the optional\n"
        "file [subset] (all nodes if no file is specified).\n"
        "With command 'out-hubs' (resp. 'in-hubs'), it outputs the list\n"
        "of arcs from selected nodes to their out-hubs (resp. from\n"
        "in-hubs to selected nodes).\n"
        "The promise is that gathering transitive arcs obtained by\n"
        "following one arc of the 'out-hubs' list and one arc of the\n"
        "'in-hubs' list provides the subgrap G* induced by selected nodes\n"
        "in the transitive closure of G.\n"
        "Command 'trans' computes G* and outputs its arcs.\n"
        "A '-' for [graph] or [subset] stands for standard input.\n"
        "  Graph format: one arc [src_id] [dst_id] [wgt] per line\n"
        "  Subset format: one node [id] per line.\n"
        "  Command format: [in-hubs] [out-hubs] [transitive]\n"
        "  Output format: arcs [node id] [hub/node id] [weight].\n";
        exit(1);
}

int main (int argc, char **argv) {
    // ------------------------ usage -------------------------
    if (argc < 3) {
        usage_exit(argv);
    }

    
    double t = top (0., "start");
    double t_start = t;

    // ------------------------- load graph ----------------------
    std::unordered_map<std::string,int> vi; // vertex index
    std::vector<std::string> lab;
    graph g;
    size_t n = 0;
    {
        std::vector<graph::edge> edg;
        FILE *in = (std::string("-") != argv[2]) ? fopen(argv[2], "r") : stdin;
        char u[1024], v[1024];
        long long int w;
        for ( ; fscanf(in, " %s %s %lld \n", u, v, &w) >= 3 ; ) {
            if (vi[u] == 0) { lab.push_back(u); vi[u] = 1+n++; }
            if (vi[v] == 0) { lab.push_back(v); vi[v] = 1+n++; }
            edg.push_back(graph::edge(vi[u]-1, vi[v]-1, w));
            //if(symmetrize) edg.push_back(graph::edge(vi[v]-1, vi[u]-1, w));
        }
        // graph
        g.set_edges(edg);
    }
    assert(n == g.n());
    size_t m = g.m();
    std::cerr << "graph with n=" << n << " nodes, m=" << m <<" edges\n";
    t = top (t, "graph loaded");
    
    // ------------------------- load subset -----------------------
    std::vector<int> sel;
    if (argc > 3) {
        FILE *in = (std::string("-") != argv[3]) ? fopen(argv[3], "r") : stdin;
        char u[1024];
        for ( ; fscanf(in, " %s \n", u) >= 1 ; ) {
            if (vi[u] != 0) {
                int v = vi[u] - 1;
                sel.push_back(v);
            }
        }
    } else {
        for (int u = 0; u < n; ++u) {
            sel.push_back(u);
        }
    } 
    std::vector<bool> is_sel(n, false);
    for (int v : sel) { is_sel[v] = true; }
    int n_sel = sel.size();
    std::cerr << "subset of "<< n_sel <<" nodes\n";
    t = top(t, "subset loaded");
    
    // ------------------------- hub labeling -----------------------
    pruned_landmark_labeling<graph> hl(g);
    hl.print_stats(std::cerr, is_sel, is_sel);
    t = top (t, "hub lab");

    // ----------------------------- output ------------------------
    std::string cmd(argv[1]);
    std::vector<graph::edge> edg; 
    if (cmd == "out-hubs") {
        edg = hl.out_hub_edges(is_sel, is_sel);
    } else if (cmd == "in-hubs") {
        edg = hl.in_hub_edges(is_sel, is_sel);
    } else if (cmd == "trans") {
        std::vector<graph::edge> edg_out = hl.out_hub_edges(is_sel, is_sel);
        std::vector<graph::edge> edg_in = hl.in_hub_edges(is_sel, is_sel);
        edg.reserve(n_sel * n_sel);
        graph g_out(edg_out), g_in(edg_in);
        for (int u : g_out) {
            if (is_sel[u]) {
                for (auto e : g_out[u]) {
                    if (e.wgt < INT_MAX) for (auto f : g_in[e.dst]) {
                        if (f.wgt < INT_MAX)
                            edg.push_back(graph::edge(u, f.dst, e.wgt + f.wgt));
                    }
                }
            }
            if (edg.size() > 2 * n_sel * n_sel) {
                std::cerr << "compact " << edg.size() <<" edges\n";
                graph g(n, edg);
                edg = g.simple().edges();
            }
        }
        graph g(n, edg);
        edg = g.simple().edges();
    } else {
        usage_exit(argv);
    }
    for (const graph::edge &e : edg) {
        std::cout << lab[e.src] <<" "<< lab[e.dst] <<" "<< e.wgt <<"\n";
    }
    
    t = top (t_start, "end");
    exit(0);
}

