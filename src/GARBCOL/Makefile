

# ROM-Patch-Version
F=garbcol

# Standaolone-Version
START=c000
FS=$(F)-$(START)


all: $(F).o $(FS).o


### ROM-Patch-Version

$(F).o: $(F).asm loader.asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm 


### Standalone-Version

# Format CBM und Ausgabedatei explizit setzen ...
$(FS).o: $(F).asm
	acme -Dstartaddress='$$$(START)' -v3 -l $(FS).l -r $(FS).r -f cbm -o $(FS).o $(F).asm


clean:
	rm $(F).o $(FS).o *.l *.r


