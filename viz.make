SKELETON:=./skeleton.native 1.2 4 12
THISMAKE:=make -f viz.make

help:
	@echo "Visualize some graphs."
	@echo "Install : apt-get install graphviz nodejs"
	@echo "Usage : make -f viz.make filename.pdf"
	@echo "  (to produce a pdf from file filename.csv)"

%_skel.csv: %.csv
	$(SKELETON) $< > $@

%_skel.dot: %.csv
	$(SKELETON) $< dot > $@

%.gdf: %.csv
	$(SKELETON) $< gdf > $@

%.pdf: %.dot
	neato -o $@ -Tpdf $<

%.svg: %.dot
	neato -o $@ -Nlabel= -Tsvg $<

%.force:
	rm -f $*
	$(THISMAKE) $*
