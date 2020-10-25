#
# Distribution Image Makefile
#
# 2020 Johann E. Klasek j klasek at
#

# basename for the image
F=supergc-2020-distrib

# files for the image ...
PRGS=gc-demo.prg gc-test.prg supergc.prg supergc-basic.prg supergc-a000.prg supergc-e000.prg supergc-loader.prg

# tools path 
TOOLS=../../../tools

# images path
IMAGES=../../../images


# remove Make's default rules
.SUFFIXES:


all: $(IMAGES)/$(F).d64

image $(IMAGES)/$(F).d64: $(PRGS)
	$(TOOLS)/FILE2DISK $(IMAGES)/$(F).d64 

gc-test.prg: ../progs/gc-test.bas
	cp -p $< .
	$(TOOLS)/bas2prg -f gc-test.bas

gc-demo.prg: ../progs/gc-demo.bas
	cp -p $< .
	$(TOOLS)/bas2prg -f gc-demo.bas

supergc-loader.prg: supergc-loader.bas
	$(TOOLS)/bas2prg -f $<

supergc.prg: ../jk-gc.o
	cp -p "$<" "$@"

supergc-basic.prg: ../jk-gc-basic.o
	cp -p "$<" "$@"

supergc-a000.prg: ../jk-gc-irqa.o
	cp -p "$<" "$@"

supergc-e000.prg: ../jk-gc-irqe.o
	cp -p "$<" "$@"



clean:
	rm -f *.d64 *.prg

