#
# Makefile Super Garbage Collection
# 1985-2020 J. E. Klasek j klasek.at
#
# ACME cross-assembler environment
#

VERSION=2.2-rel1-2020-10-08-New

# disable default suffixes and rules
.SUFFIXES:

F=jk-gc

all: jk-gc-basic.o jk-gc-irqa.o jk-gc-irqf.o jk-gc.o


# Default version is now the IRQ hook veriant with buffer at $A000 ...
$(F).o: $(F)-irqa.o
	cp -p $(F)-irqa.o $(F).o

# create all variants ...

# BASIC ROM copy in RAM patch hook and buffer from $E000 to $FFFF
$(F)-basic.o: $(F).asm debug.asm
	acme -Dbasic_patch=1 -v3 -f cbm -o $(F)-basic.o -l $(F)-basic.l -r $(F)-basic.r $(F).asm

# IRQ hook with buffer from $E000 to $FFFF
$(F)-irqf.o: $(F).asm debug.asm
	acme -Dno_basic_rom_buffer=1 -v3 -f cbm -o $(F)-irqf.o -l $(F)-irqf.l -r $(F)-irqf.r $(F).asm

# IRQ hook with buffer from $A000 to $BFFF
$(F)-irqa.o: $(F).asm debug.asm
	acme -Dbasic_rom_buffer=1 -v3 -f cbm -o $(F)-irqa.o -l $(F)-irqa.l -r $(F)-irqa.r $(F).asm


clean:
	rm -f *.r *.l *.out *.o


