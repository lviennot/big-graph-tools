# BigGraphTools : Various graph algorithms implemented in ocaml.

(Author : Laurent Viennot, Inria 2015)

This library provides an ocaml graph implementation with pretty low memory
usage: a graph with `n` nodes and `m` edges is represented within `2n+m` words
when edges with same source are added consecutively. Using bigarrays, unweighted
graphs with less than `2^31` nodes can be represented withing `12n+4m` bytes.

[Home page](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/)

## Install

You can install ocaml and required modules with opam, something like :
```
apt-get install opam
opam init --comp 4.02.2
opam install ocamlgraph camlzip
```

Then :
```
make
```

## Usage

Tools provided for the moment : 
 * Computing the diameter and radius of a graph. (Diameter is the maximum
excentricity of a node, and radius minimum excentricity.)
 * Computing ``skeleton graphs''. (A skeleton
of a graph is a small dominating subgraph with similar distances.)

### Example:

Compute the diameter of the largest strongly connected component of a weighted 
directed graph:
```
./bgtool.native -verbose read-src-dst-wgt weighted_edges.csv diameter
```

This reads the edges of a graph as a sequence of integer triples 
`source destination weight` (tab or space separated). The strongly connected 
components and the reverse graph are then computed. A SumSweep like algorithm
is then used to compute the diameter of the largest strongly connected 
component.

Compute a skeleton graph:
```
./skeleton.native -verbose 1.2 4 12 graph_edges.csv > skeleton.csv
```

This reads the edges of a graph from file `graph_edges.csv` and writes in file
`skeleton.csv` the edges of a skeleton of the graph.  See
[`examples/g_n2984_m19024.pdf`](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/examples/g_n2984_m19024.pdf)
for an example (the graph is in `examples` dir).

Color the edges of the graph :
```
./skeleton.native 1.2 4 12 graph_edges.csv gdf > graph_skel.gdf
```

This outputs the graph read in `graph_edges.csv` with GDF format. Edges of the
skeleton are indicated by green and red color attributes. You can then
visualize the graph and its skeleton with [Gephi](http://gephi.github.io/)
for example.



## Vizualisation with Graphviz

[Graphviz](http://graphviz.org/) provides command line tools for drawing graphs.
Try it with:
```
apt-get install graphviz
make graph_edges_skel.pdf
```

This gives
[`examples/g_n2984_m19024_skel.pdf`](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/examples/g_n2984_m19024_skel.pdf)
on the above skeleton example.


## Documentation

See [skeleton computation
documentation](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.doc/Skeleton.Skeleton.html)
and
[API](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.doc/).




## License

GNU LGPL.

