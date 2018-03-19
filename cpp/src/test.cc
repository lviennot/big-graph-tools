#include <sys/time.h>
#include <vector>
#include <unordered_map>
#include <iostream>

#include "mgraph.hh"
#include "traversal.hh"
//#include "pruned_landmark_labeling.hh"
//#include "skeleton.hh"

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
        FILE *in = stdin;
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
    int64_t sum = 0;
    for (int u : g) {
        for (auto const e : g[u]) sum += e.wgt;
    }
    std::cerr << "sum edge weights = " << sum <<  std::endl;
    t = top (t, "graph");

    // ------------------------- dijkstra -----------------------
    traversal<graph> trav(n);
    int u = 0, v = n - 1;
    trav.dijkstra(g, u);
    std::cerr << trav.nvis() <<" nodes visited\n";
    std::cerr << "dist "<< u <<" to "<< v <<" = "<< trav.dist(v) <<  std::endl;

    std::cerr << "rev path:";
    for (; v != u; v = trav.parent(v)) {
        std::cerr <<" "<< v;
    }
    std::cerr <<" "<< u <<"\n";
    t = top (t, "dijkstra");

}
