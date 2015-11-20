# BigGraphTools : Various graph algorithms implemented in ocaml.

(Author : Laurent Viennot, Inria 2015)

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

Mainly one tool for the moment : computing ``skeleton graphs''. (A skeleton
of a graph is a small dominating subgraph with similar distances.)

### Example:

Compute a skeleton graph:
```
./skeleton.native -verbose 1.2 4 12 graph_edges.csv > skeleton.csv
```

This reads the edges of a graph from file `graph_edges.csv` and 
writes in file `skeleton.csv` the edges of a skeleton of the graph.
See [`examples/g_n2984_m19024.pdf`](tree/master/examples/) for an example.

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



## Documentation

See [skeleton computation
documentation](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.doc/Skeleton.Skeleton.html)
and
[API](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.doc/).




## License

GNU LGPL.

