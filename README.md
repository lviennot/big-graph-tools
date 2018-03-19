# BigGraphTools

(Author : Laurent Viennot, Inria 2015)

Tools for manipulating large graphs:
 * a minimalist library in c++,
 * and a [libray in ocaml](ocaml/README.md).

[Home page](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/)

Example:
```
   typedef mgraph<float> graph; // graph with int vertices and float weights on edges 
   std:vector<graph::edge> edges;
   edges.push_back(graph::edge(1,2,10.)); // edge 1 --> 2 with weight 10.
   edges.push_back(graph::edge(2,3,20.));
   graph g(5, edges); // vertices are 0,1,..,4
   for (int u : g)
      for (auto e : g[u]) 
         std::cout << u << " " << e.dst << " " << e.wgt << std::endl;

   // ignoring weights :
   for (int u : g)
      for (int v : g[u]) 
         std::cout << u << " " << v << std::endl;

   traversal<graph> trav(n);
   trav.dijkstra(g, 1);
   std::cout << "dist(1,3) = "<< trav.dist(3) <<  std::endl;
```


## Try it:

```
cd cpp
make
```


## License

GNU LGPL.

