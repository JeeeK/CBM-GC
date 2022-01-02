# Overview
Garbcol is a Garbage Collection implemenation for the C64 similar to the one from CBM BASIC 4.0. It is indented to replace the "old" routine in the BASIC and KERNAL ROM. It has been published October 1988 in the german magazine 64'er.
There are several versions starting at the original and evolving to the bugfixed and optimized one.
- orig: original, tranformed from the disassembly into assembler source
- bugfixed: Just the original version without bugs.
- optimized: Some optimizations.
- optimized-2: Further optimization.
- experimental: An experimental optimiziation, which reaches the break-even-point only if most of the strings are long enough. The string copy routine is faster, but it needs more space which could only be satisfied factoring a part out which leads to a significant JSR/RTS overhead.

See [info.german.txt](GARBCOL/info.german.txt) for a far more detailed description (german only).
