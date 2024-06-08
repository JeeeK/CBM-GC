#
# Makefile Back-Link On Demand Garbage Collection
# 2021 Johann E. Klasek j klasek.at
#
# ACME cross-assembler environment
#

VERSION=1.2-rel1-2024-06-09

# Start address (note $ quoting)
START = $$C500


# disable default suffixes and rules
.SUFFIXES:

F=blod-gc

all: blod-gc.o


$(F).o: $(F).asm
	acme -Dstart='$(START)' -v3 -f cbm -o $(F).o -l $(F).l -r $(F).r $(F).asm


clean:
	rm -f blod-gc*.r blod-gc*.l *.out blod-gc*.o


# Source code checks

# any code differences between english and german version?
check:
	sed -e 's/;.*//' blod-gc.asm > 1 ;\
	sed -e 's/;.*//' blod-gc.de.asm > 2 ;\
	diff -wB 1 2 ;\
	rm -f 1 2


