SKELETON:=./skeleton.native -verbose 0. 6 12
VIZMAKE:=make -f viz.make
GRAPHVIZ:=neato -Ksfdp -Goverlap=scale -Gsplines=curved -Nlabel="" -Earrowhead=none -Nshape=circle -Nstyle=filled -Nwidth=.1 -Ncolor="\#00000060" -Ecolor="\#00000020"
# -Nstyle=filled -Nheight=1 -Nwidth=1 -Nfixedsize=true

help:
	@echo "Visualize some graphs."
	@echo "Install : apt-get install graphviz nodejs"
	@echo "Usage : make -f viz.make filename.pdf"
	@echo "  (to produce a pdf from file filename.csv)"

%_skel.csv: %.csv
	$(SKELETON) $< > $@

%_skel.dot: %.csv
	$(SKELETON) $< dot > $@

%.dot: %.csv
	$(SKELETON) $< fulldot > $@

%.gdf: %.csv
	$(SKELETON) $< gdf > $@

%.pdf: %.dot
	$(GRAPHVIZ) -o $@ -Tpdf $<

%.svg: %.dot
	$(GRAPHVIZ) -o $@ -Tsvg $<

%.force:
	rm -f $*
	$(VIZMAKE) $*


.PHONY: examples

examples:
	$(VIZMAKE) examples/g_n2984_m19024.pdf
	$(VIZMAKE) examples/g_n2984_m19024_skel.pdf

