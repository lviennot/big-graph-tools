MHDRS:=$(addprefix src/, edge.hh mgraph.hh)
#dyn_graph.hh traversal.hh eccentricity.hh graphgen.hh treedec.hh pruned_landmark_labeling.hh skeleton.hh verbose.hh)

main: test
	gunzip -c ex/corse.gr.gz | ./test

test: src/test.cc $(HDRS)
	g++ -std=c++11 -O3 -g -rdynamic -pthread -o $@ $<

clean:
	rm -f test *~ src/*~
	rm -fr test.dSYM
