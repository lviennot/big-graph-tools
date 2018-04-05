#include <sys/time.h>
#include <vector>
#include <unordered_map>
#include <iostream>

#include "mgraph.hh"
#include "traversal.hh"
#include "skeleton.hh"
#include "pruned_landmark_labeling.hh"
#include "hub_labeling.hh"

typedef mgraph<int> graph;

double top (double t1, std::string msg) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    double t2 = tv.tv_sec + tv.tv_usec * 1e-6;
    std::cerr << "-- time " << msg << " : " << (t2 - t1) << "s\n";
    std::cerr.flush();
    return t2;
}

int main (int argc, char **argv) {
    double t = top (0., "start");
    
    // ------------------------- load ----------------------
    std::vector<graph::edge> edg;
    std::unordered_map<std::string,int> vi; // vertex index
    std::vector<std::string> lab;
    size_t n = 0;
    {
        FILE *in = (argc > 1) ? fopen(argv[1], "r") : stdin;
        char u[1024], v[1024];
        long long int w;
        for ( ; fscanf(in, " %s %s %lld \n", u, v, &w) >= 3 ; ) {
            if (vi[u] == 0) { lab.push_back(u); vi[u] = 1+n++; }
            if (vi[v] == 0) { lab.push_back(v); vi[v] = 1+n++; }
            edg.push_back(graph::edge(vi[u]-1, vi[v]-1, w));
            //if(symmetrize) edg.push_back(graph::edge(vi[v]-1, vi[u]-1, w));
        }
    }
    size_t m = edg.size();
    std::cerr << "n=" << n << " m=" << m <<  std::endl;
    t = top (t, "load");

    // ------------------------- graph -----------------------
    graph g(edg);
    n = g.n(); m = g.m();
    std::cerr << "n=" << n << " m=" << m <<  std::endl;
    t = top (t, "graph");

    // ------------------------- read -----------------------
    int64_t sum = 0, min = INT64_MAX, max = INT64_MIN;
    for (int u : g) {
        for (auto const e : g[u]) {
            sum += e.wgt;
            if (e.wgt < min) min = e.wgt; 
            if (e.wgt > max) max = e.wgt; 
        }
    }
    std::cerr << "sum edge weights = " << sum
              << ", min = " << min <<", max = "<< max 
              <<  std::endl;
    t = top (t, "graph");

    // ------------------------- dijkstra -----------------------
    traversal<graph> trav(n);
    int u = 0, v = n - 1;
    trav.dijkstra(g, u);
    std::cerr << trav.nvis() <<" nodes visited\n";
    std::cerr << "dist "<< u <<" to "<< v <<" = "<< trav.dist(v) <<  std::endl;

    std::cerr << "  rev path:";
    int hops = 0;
    for (int w = v; w != u; w = trav.parent(w), ++hops) {
        std::cerr <<" "<< w;
    }
    std::cerr <<" "<< u <<"\n"<<"  nb hops = "<< hops <<"\n";

    std::cerr <<"  root sons:";
    graph tree = trav.digraph_to_sons();
    std::vector<traversal<graph>::node> nodes = trav.digraph_nodes();
    for (int s : tree[0]) { // sons of the root
        std::cerr <<" "<< nodes[s].vtx ;//<<" ("<< nodes[s].dist_max <<"),";
    }
    std::cerr <<"\n";
    t = top (t, "dijkstra");

    // ------------------------ skeleton ---------------------------
    skeleton<traversal<graph> > sk(n);
    sk.of_traversal(trav, 1, 2 /* alpha = 1/2 */,
                    skeleton<traversal<graph> >::traversal_metric);
    int width = sk.width();
    double iwdt = sk.integrated_width(trav);
    std::cerr << "skel. width = " << width
              << ", integr. skel. width = " << iwdt
              << ", skel. size = " << sk.size()
              <<  std::endl;
    sk.clear();
    sk.of_traversal(trav, 1, 2 /* alpha = 1/2 */,
                    skeleton<traversal<graph> >::hop_count_metric);
    width = sk.width();
    iwdt = sk.integrated_width(trav);
    std::cerr << "skel. width (hop) = " << width
              << ", integr. skel. width = " << iwdt
              << ", skel. size = " << sk.size()
              <<  std::endl;
    t = top (t, "skeleton");
    
    // ------------------------- pruned ll -----------------------
    /*{
    std::vector<int> perm(n);
    for (int i = 0; i < n; ++i) perm[i] = i;
    for (int i = n-1; i > 0; --i) {
        std::swap(perm[i], perm[rand() % (i+1)]);
    }
    pruned_landmark_labeling<mgraph<int> > pll(g, perm);
    pll.print_stats(std::cerr);
    std::cerr << "dist "<< u <<" -> "<< v
              << " = " << pll.distance(u, v) <<  std::endl;
    t = top (t, "pruned ll");
    int64_t sd = 0;
    for (int i = 0; i < 1000 * 1000; ++i)
        sd = std::max(sd, pll.distance(rand() % n, rand() % n));
    std::cerr <<"  avg dist: "<< sd / 1000 / 1000 << std::endl;
    t = top (t, "pruned ll 1M req dist");
    }*/
    
    
    // ------------------------- hub labeling -----------------------
    hub_labeling<graph> hl(g);
    hl.print_stats(std::cerr);
    std::cerr << "dist "<< u <<" -> "<< v
              << " = " << hl.distance(u, v) <<  std::endl;
    t = top (t, "hub lab");
    int64_t sd = 0;
    for (int i = 0; i < 1000 * 1000; ++i)
        sd = std::max(sd, hl.distance(rand() % n, rand() % n));
    std::cerr <<"  avg dist: "<< sd / 1000 / 1000 << std::endl;
    t = top (t, "hl 1M req dist");
    
    exit(0);

}
