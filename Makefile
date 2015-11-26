TARGETS:=skeleton bgtool
MODS:=traverse.ml
LIBS:=-lib unix -lib bigarray -lib str
#  -lib graphics -cflags -I,+lablgtk
PACKAGES:=-package ocamlgraph
# -package batteries 
APIDOC:=IntDigraph Vector Traversal Skeleton
SRCS:=$(wildcard src/*.ml) $(wildcard src/*.mli)


binaries: _tags
	 ocamlbuild $(LIBS) $(PACKAGES) $(patsubst %,%.native,$(TARGETS))

all: binaries api.doc


.PHONY: _tags

_tags:
	(echo "true:      inline(0)" ; echo "true:       debug" ; echo "<src>:      include") > _tags

%.native: _tags $(SRCS)
	ocamlbuild $(LIBS) $(PACKAGES) $@

unit: _tags $(SRCS)
	ocamlbuild $(LIBS) $(PACKAGES) unit.native --


mods.cma: $(patsubst %,src/%,$(MODS))
	echo $(patsubst %,src/%,$(MODS)) > mods.mllib
	ocamlbuild $(LIBS) $(PACKAGES) $@

mods.top: $(patsubst %,_src/%,$(MODS))
	echo $(patsubst %,_build/src/%,$(MODS)) > mods.mltop
	ocamlbuild $(LIBS) $(PACKAGES) $@

ocaml:
	utop || rlwrap ocaml || ocaml 



.PHONY: doc

api.doc: 
	rm -f $@ api.odocl
	for m in $(APIDOC) ; do echo $$m >> api.odocl ; done
	ocamlbuild -docflags -charset,UTF-8,-keep-code,-colorize-code,-html,-short-functors $(LIBS) $(PACKAGES) api.docdir/index.html
	rm -f api.odocl
	mv api.docdir $@


clean:
	rm -f *~ src/*~
	ocamlbuild -clean
	rm -fr _build _tags api.odocl mods.top mods.mltop


-include viz.make manage.make

