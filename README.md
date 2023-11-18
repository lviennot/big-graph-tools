# BigGraphTools

(Author : Laurent Viennot, Inria 2015)

Tools for manipulating large graphs:
 * a minimalist library in c++,
 * and a [libray in ocaml](ocaml/README.md).

[Home page](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/)



This project has been split into smaller modules:

 * [int-graph-ml](https://github.com/lviennot/int-graph-ml) (Ocaml): the core of the ocaml part providing a graph representation with compact memory usage compared to ocamlgraph).
 * [weighted-diameter](https://github.com/lviennot/weighted-diameter) (C++): effcient computation of diameter, radius and all eccentricities of a graph (supports both directed/undirected and weighted/unweighted).
 * [hub-labeling](https://github.com/lviennot/hub-labeling) (C++): compact representation of the distance matrix through a hub labeling (aka 2-hop labeling) of a weighted directed graph. 
 * [contraction-hierarchies](https://github.com/lviennot/contraction-hierarchies) (C++): contracting a graph to a distance preserver allowing efficient shortest path computations.
 * [hl-csa-raptor](https://github.com/lviennot/hl-csa-raptor) (C++): classical public transit routing algorithms (inputing GTFS format) and supporting unrestricted walking through a hub labeling of the footpath graph.

### C++ example:
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

### Ocaml example:
```
  let module G = IntDigraph in
  let g = G.create () in
  List.iter (fun (u,v) -> G.add_edge g u v) 
  [2,4; 1,8; 1,2; 2,4; 2,3; 3,4; 2,5; 4,1; 2,9; 1,9; 2,8; 1,7; 2,4; 2,7;] ;
  iter_vertex (fun u ->
      Printf.printf "%d -> " u ;
      iter_succ (fun v ->
        Printf.printf " %d" v
      ) g u ;
      Printf.printf "\n" ;
  ) g ;

  let module Bfs = Traversal.Bfs(G) in
  let t = Bfs.tree g 1 in
  Printf.printf "dist(1,2) = %d\n" (Bfs.dist t 2) ;
```

The ocaml code is an alternative to ocamlgraph (which is imported for comparison tests).

## Try it:

```
cd cpp
make
```

or

```
cd ocaml
apt-get install opam
opam init --comp 4.02.2
opam install ocamlgraph camlzip
make
```

## License

GNU LGPL.

