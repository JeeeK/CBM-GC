#
# Makefile garbcol/image/
#
# 2013-2021 J.E. Klasek, johann at klasek.at
#


.SUFFIXES:


all: garbcol.d64


garbcol.d64:
	c1541 -format garbcol,jk d64 $@
	c1541 $@ -write ../garbcol-c000.o garbcol-c000,p
	c1541 $@ -write ../garbcol.o garbcol-ng,p
	c1541 $@ -write ../bugfixed/garbcol.o garbcol,p
	c1541 $@ -write ../orig-distrib/g1.prg g1,p
	c1541 $@ -write ../orig-distrib/g2.prg g2,p
	c1541 $@ -write ../orig-distrib/g3.prg g3,p
	c1541 $@ -write ../orig-distrib/g4.prg g4,p
	c1541 $@ -write "../orig-distrib/garbage start.prg" "garbage start,p"
	c1541 $@ -write ../orig-distrib/garbcol1.prg garbcol1,p
	c1541 $@ -write ../orig-distrib/garbcol2.prg garbcol2,p
	c1541 $@ -write ../orig-distrib/garbcol3.prg garbcol3,p
	c1541 $@ -write ../orig-distrib/garbcol4.prg garbcol4,p
	c1541 $@ -write ../orig-distrib/installation.prg installation,p
	c1541 $@ -write ../orig-distrib/svop.prg svop,p
	c1541 $@ -dir

clean:
	rm *.d64

