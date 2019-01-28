RSYNC:=rsync -v -e ssh --inplace -r --relative --exclude '*~' --exclude '\#*' 
CLUST:=rioc.inria.fr
CLUST_DIR:=graph/cpp


# -------------------- compile ------------------------

HDRS:=$(wildcard src/*.hh)
CTRS:=$(wildcard src/*.cc)
LIBS:=-lz

main: raptor.o

hltrans: src/HL_trans.cc $(HDRS)
	g++ -std=c++11 -O3 -g -rdynamic -pthread -o $@ $<

ch: src/CH.cc $(HDRS)
	g++ -std=c++11 -O3 -g -rdynamic -pthread -o $@ $<

gtfs2graph: src/gtfs2graph.cc $(HDRS)
	g++ -std=c++11 -O3 -g -rdynamic -pthread -o $@ $<

test: test.o

%.o: src/%.cc $(HDRS)
	g++ -std=c++11 -O3 -g -rdynamic -pthread $(LIBS) -o $@ $<

headers:
	@echo $(HDRS)


unit: unit.og
	./unit.og

%.og: src/%.cc $(HDRS)
	g++ -std=c++11 -g -rdynamic -pthread $(LIBS) -o $@ $<

clean:
	rm -f test *~ src/*~
	rm -fr test.dSYM



# -------------------- walking graph ------------------

london:
	make raptor.o && ./raptor.o 0 86400 ~/dev/pt/Public-Transit-Data/London

london_lb:
	make tt_lb_graph.o && (./tt_lb_graph.o 0 86400 ~/dev/pt/Public-Transit-Data/London | gzip -c > ~/dev/pt/Public-Transit-Data/London/lower_bound.gr.gz)
	gunzip -c ~/dev/pt/Public-Transit-Data/London/lower_bound.gr.gz ~/dev/pt/Public-Transit-Data/London/walking_and_transfers.gr.gz | ./hltrans hubs - | gzip -c > ~/dev/pt/Public-Transit-Data/London/lower_bound-hubsall.gr.gz

hubs: hltrans
	make London.hltrans
	make Paris.hltrans
	make Switzerland.hltrans

.PHONY: %.hltrans
%.hltrans:
	make $*/out_hubs.gr.gz $*/in_hubs.gr.gz

.PRECIOUS: %/hubs.gr.gz
%/hubs.gr.gz: %/walking_graph.gr.gz %/stops.txt hltrans
	gunzip -c $< | ./hltrans hubs - $*/stops.txt | gzip -c > $@

%/out_hubs.gr.gz: %/hubs.gr.gz
	gunzip -c $< | grep -e '^o ' | cut -d' ' -f2-4 | gzip -c > $@

%/in_hubs.gr.gz: %/hubs.gr.gz
	gunzip -c $< | grep -e '^i ' | cut -d' ' -f2-4 | gzip -c > $@


_output/%-hubsall.gr.gz: ex/%.gr.gz hltrans
	mkdir -p _output
	gunzip -c $< | ./hltrans hubs - | gzip -c > $@

_output/%-clos.gr.gz: ex/%.gr.gz ex/%.stops hltrans
	mkdir -p _output
	gunzip -c $< | ./hltrans closure - ex/$*.stops | cut -d' ' -f2-4 | gzip -c > $@




%.stop_paris:
	lab () { while [ $$# -gt 0 ]; do st=$$1; stop=`grep -e ",$${st}[^0-9]" labels/stop_label.csv | sed -e 's/,.*//'`; echo -n "$$st is "; grep -e "^$$stop," gtfs/stops.txt; shift; done } ;\
	points () { lab $$* | cut -d , -f5,6 | awk '{printf "&p=%s", $$1}'; echo ""; } ;\
	pts () { lab $$*; points $$*; } ;\
	cd ../../../../dev/pt/Public-Transit-Data/Paris ; pts $*

%/walking_graph.gr.gz: %/walking_graph_seconds.gr.gz %/transfers.csv.gz %/stops.txt
	cd $*; \
	gunzip -c transfers.csv.gz | tail -n +2 | tr "," " " | gzip -c > transfers.gr.gz ;\
	cat stops.txt | awk '{print $$1,$$1,0;}' | gzip -c > stops_self_loops.gr.gz ;\
	gunzip -c transfers.gr.gz walking_graph_seconds.gr.gz stops_self_loops.gr.gz | gzip -c > walking_graph.gr.gz


three: raptor.o
	echo $$HOSTNAME $$OAR_JOB_ID
	for dir in London Paris Switzerland; do \
		./raptor.o -query-file=queries-all.csv -exit $$dir > CSA/$${dir}_times.csv ;\
	done

%.Raptor: raptor.o
	echo $$HOSTNAME $$OAR_JOB_ID
	./raptor.o $*

threeCSA:
	$(eval HUNDR := grep -v -e '^[0-9]$$$$' -e '^[0-9][0-9]$$$$' -e '^[0-9][0-9][1-9]$$$$' -e '^[0-9][1-9][0-9]$$$$')
	echo $$HOSTNAME $$OAR_JOB_ID
	cd CSA/build ;\
	for dir in London Paris Switzerland; do \
	for opt in '' '--hl' '--profile' '--hl --profile'; do \
		d="------------" ;\
		echo "$$d csa $$opt $$dir $$d"; ./csa $$opt $$dir | $(HUNDR) ;\
	done ;\
	done

%.fourthCSA:
	echo $$HOSTNAME $$OAR_JOB_ID
	cd CSA/build ;\
	opt='--hl --profile' ;\
	b="------------ csa $$opt"; e="-------------" ;\
	echo "$$b $* $$e"; ./csa $$opt $* ;\

# ------------------- public transit data --------------

PT_DIRS:=London Paris Switzerland

alleat: queries.csv.eat.rioc3

bigeat:
	for qry in queries queries-2h queries-loc10pct; do \
		echo $$dir $$qry; \
		make $${qry}.csv.eat.rioc3; \
	done

alleatpar:
	echo $$HOSTNAME $$OAR_JOB_ID
	make London/arrival_times.csv2 & \
	(sleep 10; make Paris/arrival_times.csv2) & \
	(sleep 20; make Switzerland/arrival_times.csv2)

%.three:
	for dir in London Paris Switzerland; do \
		make $${dir}/$*; \
	done

%.raptor: hostinfo raptor.o 
	make $*

.PHONY: hostinfo
hostinfo:
	echo $$HOSTNAME $$OAR_JOB_ID

%.3:
	for dir in London Paris Switzerland; do \
		make $${dir}/$*;\
	done

%.rioc3: _scp
	for dir in London Paris Switzerland; do \
		make $${dir}/$*.raptor.oarsub.ssh; sleep 1;\
	done

%.rioc1: _scp
	make $*.raptor.oarsub.ssh

riocCSA3: _scp
	for dir in London Paris Switzerland; do \
		make $${dir}.CSA.rioc RESA=yes; sleep 1;\
	done

MKTMP=$(shell mktemp -u $@.XXXX~)

%.eat: %
	$(eval TMP:=$(MKTMP))
	$(eval DIR:=`dirname $*`)
	$(eval QRY:=`basename $*`)
	./raptor.o $(DIR) -query-file=$(QRY) -arrival-times -exit > $(TMP)
	mv -f $(TMP) $@


%.all:
	$(eval TMP:=$(MKTMP))
	make $(TMP).mkdir
	$(eval DIR:=`dirname $*`)
	$(eval QRY:=`basename $*`)
	$(eval PRESCAN:=$(if $(findstring Switzerland,$(DIR)),-prescan,))
	./raptor.o $(DIR) -query-file=$(QRY) -arrival-times $(PRESCAN) > $(TMP)
	mv -f $(TMP) $@

prt:
	p () { head -n 5 $1.stdout | grep -e node -e make | head -n 2; tail -n 16 $1.stderr | sed -e 's/-- +//' -e 's/s.*://' | grep -v 'last dep' | awk '{if((0+$2) != 0) print 1000.0*$1/$2,$0}'; }
	id () {for i in $*; do echo -n $i,; lab=`grep -e ",${i}[^0-9]" labels/stop_label.csv | cut -d, -f1`; echo -n $lab,; grep -e "^${lab}," gtfs/stops.txt; done }

%.get3:
	for dir in London Paris Switzerland; do \
		scp "$(CLUST):$(CLUST_DIR)/$${dir}/*.$*" $${dir}/ ;\
	done

%.cp:
	cp $* /Volumes/files.inria.fr/gang/road/graphs/public_transport/$*
	scp $* $(CLUST):$(CLUST_DIR)/$*

table_all:
	grep -e '_avg_time' London/queries-unif.csv.all | sed -e 's/_avg_time//' | cut -d' ' -f1 > $@
	for dir in London Paris Switzerland; do \
		for res in queries-unif.csv.all queries-rank.csv.all; do \
			mv -f $@ $@~;\
			grep avg_time $${dir}/$${res} | cut -d' ' -f2 | paste -d, $@~ - > $@ ;\
		done ;\
	done

CORRECT=| tr . , | awk '{printf "%s %.1f\n", $$1, $$2;}' | tr , .

table:
	grep -e '^HL.*_avg_time' London/queries-unif.csv.all | sed -e 's/_avg_time//' | cut -d' ' -f1 > $@
	for dir in London Paris Switzerland; do \
		for res in queries-unif.csv.all queries-rank.csv.all; do \
			mv -f $@ $@~;\
			grep avg_time $${dir}/$${res} $(CORRECT) | grep -v -e '^HL' | cut -d' ' -f2 | paste -d, $@~ - > $@ ;\
		done ;\
		for res in queries-unif.csv.all queries-rank.csv.all; do \
			mv -f $@ $@~;\
			grep avg_time $${dir}/$${res} $(CORRECT) | grep -e '^HL' | cut -d' ' -f2 | paste -d, $@~ - > $@ ;\
		done ;\
	done

split: table.force
	cat table | awk -F, '{print $$1; printf "  & %s & %s & %s & %s\n",$$2,$$3,$$4,$$5; printf "  & %s & %s & %s & %s ",$$6,$$7,$$8,$$9; printf "  & %s & %s & %s & %s\\\\\n",$$10,$$11,$$12,$$13;}'


%.2hrange:
	$(eval TMP:=$(MKTMP))
	make $(TMP).mkdir
	$(eval DIR:=`dirname $*`)
	$(eval QRY:=`basename $*`)
	$(eval PRESCAN:=$(if $(findstring Switzerland,$(DIR)),-prescan,))
	./raptor.o $(DIR) -query-file=$(QRY) -2h-range $(PRESCAN) > $(TMP)
	mv -f $(TMP) $@

%.CSA:
	echo $$HOSTNAME $$OAR_JOB_ID
	$(eval HUNDR := grep -v -e '^[0-9]$$$$' -e '^[0-9][0-9]$$$$' -e '^[0-9][0-9][1-9]$$$$' -e '^[0-9][1-9][0-9]$$$$')
	cd CSA/build ;\
	for opt in '' '--hl' '--profile' '--hl --profile'; do \
		d="------------" ;\
		echo "$$d csa $$opt $* $$d"; ./csa $$opt $* | $(HUNDR) ;\
	done


# ---------------- queries -------------------


%-rank.csv: %-unif.csv
	$(eval DIR:=`dirname $<`)
	$(eval QRY:=`basename $<`)
	./raptor.o $(DIR) -exit -query-file=$(QRY) -nq=100 -rank -all-ranks > $@ 

%-rank-long.csv: %-unif.csv
	$(eval DIR:=`dirname $<`)
	$(eval QRY:=`basename $<`)
	./raptor.o $(DIR) -exit -query-file=$(QRY) -nq=1000 -rank -all-ranks > $@ 

%-unif.csv: #%.csv
	$(eval DIR:=`dirname $<`)
	$(eval QRY:=`basename $<`)
	./raptor.o $(DIR) -exit -query-file=$(QRY) -nq=1000 -rerank > $@ 


%/queries-loc10pct.csv: %/arrival_times.csv
	(echo "source,target,time"; tail -n +2 $^ | awk -F, '{ print $$1, $$2, $$3, $$6-$$3}' | sort -n -k 4 | head -n 1000 | tr " " "," | cut -d, -f1-3) > $@

%/queries-loc10pct.tail: %/arrival_times.csv
	tail -n +2 $^ | awk -F, '{ print $$1, $$2, $$3, $$6-$$3}' | sort -n -k 4 | head -n 1000 | grep -v -E '[0-9]{10}' | tail -n 1

%/queries-prox10pct.csv: %/arrival_times.csv
	(echo "source,target,time"; tail -n +2 $^ | awk -F, '{ print $$1, $$2, $$3, $$4-$$3}' | sort -n -k 4 | head -n 1000 | tr " " "," | cut -d, -f1-3) > $@

%/queries-prox10pct.tail: %/arrival_times.csv
	tail -n +2 $^ | awk -F, '{ print $$1, $$2, $$3, $$4-$$3}' | sort -n -k 4 | head -n 1000 | grep -v -E '[0-9]{10}' | tail -n 1

%/queries-2h.csv: %/arrival_times.csv
	(echo "source,target,time"; tail -n +2 $^ | awk -F, '{if ($$4 - $$3 <= 7200) print $$1","$$2","$$3}' | head -n 1000) > $@

%/queries-4h.csv: %/arrival_times.csv
	(echo "source,target,time"; tail -n +2 $^ | awk -F, '{if ($$4 - $$3 <= 14400) print $$1","$$2","$$3}' | head -n 1000) > $@


# ------------------ result analysis -----------------------

getres:
	for dir in $(PT_DIRS) ; do \
		$(RSYNC) "rioc.inria.fr:c/$$dir/*.eat" $$dir/ ; \
	done

%.delay_day: %
	tail -n +2 $< | awk -F, '{if($$4 < 100000000 && $$3 >= 5 * 3600 && $$4 < 21 * 3600) print $$1, $$2, $$3, $$5, $$5, $$6, 100.*($$4-$$5)/($$4-$$3)}' | sort -n -k 7 > $@

%.delay: %
	tail -n +2 $< | awk -F, '{if($$4 < 100000000) print $$1, $$2, $$3, $$5, $$5, $$6, 100.*($$4-$$5)/($$5-$$3)}' | sort -n -k 7 > $@



%.mkdir:
	@d=`dirname $*` ;\
	if [ "$$d" != "." -a ! -e "$$d" ] ; then \
	p=`dirname $$d`;\
	make $$p.mkdir ;\
	mkdir -p $$d ;\
	fi

%.cat: %
	cat $*

%.wc: %
	wc $*

%.nlines: %
	wc -l $* | sed -e 's/^ *//' | cut -d' ' -f1

%.head: %
	head $*

%.tail: %
	tail $*

%.median: %
	nlines=`make -s $*.nlines`; echo $$nlines lines; \
	head -n $$(( $$nlines / 2 )) $* | tail

%.min_moy_max: %
	cat $* | grep -e '^#' | head -n 2 ;\
	cat $* | grep -v -e '^#' | gawk 'BEGIN { cols=0; n=0; } { cols=NF; for (i=1; i<=NF; i++) { v=$$(i); sum[i]+=v; if (n == 0 || v > max[i]) max[i]=v; if (n == 0 || v < min[i]) min[i]=v; } n+=1; } END { for (i=1; i<=NF; i++) printf "%d\t",i; print ""; for (i=1; i<=NF; i++) printf "%.2f\t",min[i]; print "";  for (i=1; i<=NF; i++) printf "%.2f\t",sum[i]/n; print ""; for (i=1; i<=NF; i++) printf "%.2f\t",max[i]; print ""; print n; }'

%.min_moy_max_int: %
	cat $* | grep -e '^#' | head -n 2 ;\
	cat $* | grep -v -e '^#' | gawk 'BEGIN { cols=0; n=0; } { cols=NF; for (i=1; i<=NF; i++) { v=$$(i); sum[i]+=v; if (n == 0 || v > max[i]) max[i]=v; if (n == 0 || v < min[i]) min[i]=v; } n+=1; } END { for (i=1; i<=NF; i++) printf "%d\t",i; print ""; for (i=1; i<=NF; i++) printf "%d\t",min[i]; print "";  for (i=1; i<=NF; i++) printf "%d\t",sum[i]/n; print ""; for (i=1; i<=NF; i++) printf "%d\t",max[i]; print ""; print n; }'

%.avg:
	make $*.min_moy_max_int



# ------------- manage running ---------------

cpuinfo: hostinfo
	cat /proc/cpuinfo

%.oarsub:
	$(eval TMP:=$(shell mktemp -u oarsub.XXXX~))
	#oarsub -p "host like 'node04%' and not host like 'node040'" -l nodes=1/cpu=2/core=10,walltime=96:00:00 "make $*" | tee $(TMP)
	oarsub -p "host like 'node06%' and not host like 'node060'" -l nodes=1/cpu=2/core=18,walltime=96:00:00 "make $*" | tee $(TMP)
	JOBID=`grep OAR_JOB_ID $(TMP) | sed -e 's/.*=//'` ;\
	sleep 5; echo " --- stdout:"; cat  OAR.$${JOBID}.stdout ;\
	sleep 10; echo; echo " ---stderr:"; cat  OAR.$${JOBID}.stderr
	rm $(TMP)
	echo

%.ssh: _scp
	ssh $(CLUST) 'cd $(CLUST_DIR); make $*'

%.force:
	rm -f $*; make $*

%.rioc: _scp
	$(eval ALL:=nodes=1/cpu=2/core=12,walltime=96:00:00)
	$(eval CPU:=$(if $(RESA),$(ALL),nodes=1/cpu=1/core=$(if $(CORE),$(CORE),1),walltime=24:00:00))
	echo $(CPU)
	ssh $(CLUST) 'cd $(CLUST_DIR); oarsub -p mem\>=160 -l $(CPU) "make $*"; sleep 5; echo; echo " --- stdout:"; cat  `ls OAR.*.stdout | tail -n 1`; sleep 10; echo; echo " ---stderr:"; cat  `ls OAR.*.stderr | tail -n 1`; echo'

stat:
	ssh $(CLUST) oarstat

scp: _scp

_scp: myMakefile Makefile $(HDRS) $(CTRS)
	$(RSYNC) myMakefile Makefile src $(CLUST):$(CLUST_DIR)/
	cp -f Makefile myMakefile
	touch $@

%.scp:
	$(RSYNC) $* $(CLUST):$(CLUST_DIR)/

%.get:
	scp $(CLUST):$(CLUST_DIR)/$* $*


# ------------------ test some macros -----------------------

%.t:
	$(eval ALL:=$(if $(ALL),yes,no))
ifeq ($(ALL),y)
	echo "---- y ---"
endif
	echo "chg? ---$(ALL)---"

define MAC
1
2
3 4
endef
export MAC
mac:
	echo "$$MAC" 

