# BigGraphTools : Various Graph algorithms implemented in ocaml.

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

Mainly one tool for the moment : computing ``skeleton graphs''.

### Example:

Compute a skeleton graph :
```
./skeleton.native 1.2 4 12 graph_edges.csv
```

This outputs edges of the skeleton of the graph whose edges are in file
`graph_Edges.csv`.

Color the edges of the graph :
```
./skeleton.native 1.2 4 12 graph_edges.csv gdf
```

This outputs the graph read in `graph_edges.csv` with GDF format with edges
of the skeleton colored in green and red. You can then visualize the graph
and its skeleton with Gephi for example.



## Vizualisation with Graphviz
```
apt-get install graphviz
make -f viz.make graph_edges_skel.dot
```

This produces a skeleton graph in file `graph_edges_skel.dot` with dot format
from the graph in file `graph_edges.csv`. You can also produce a pdf file with:
```
make -f viz.make graph_edges_skel.pdf
```


## Documentation

See [skeleton computation
documentation](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.docdir/Skeleton.Skeleton.html)
and
[API](https://who.rocq.inria.fr/Laurent.Viennot/dev/big-graph-tools/api.docdir/).




## TODO

- classical linked list in edge vector, or even binary search tree, or a mix
- advantage : index of edges are fixed
- concl: sort should send a new graph (delete the old one if you wish)
- take few bits to mark nodes (explicitly added are marked) or edges


## License

GNU LGPL.

