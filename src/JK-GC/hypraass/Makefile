#
# HypraAss Helper Makefile
#
# Super Garbage Collection
# 1985-2020 J. E. Klasek - j klasek.at
#

# tools ...
PETCAT=petcat
HACAT=../../../tools/hacat


# do all conversion ...
all: l-gc-8-dok.bas s-gc-8-dok.asm


# convert list ouptut to ASCII

l-gc-8-dok.bas: l-gc-8-dok
	$(PETCAT) $< > $@

# convert PRG source into ASCII source without line numbers

s-gc-8-dok.asm: s-gc-8-dok.prg
	$(HACAT) -n $< > $@

