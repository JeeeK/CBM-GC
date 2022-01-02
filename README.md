# CBM-GC

Some alternate, newer and faster Commodore 8-Bit Garbage Collection Implementations in comparison to the standard BASIC V2 implementation.

1. [Back-Link On Demand Garbage Collection](#back-link-on-demand-garbage-collection)
2. [Super Garbage Collection](#super-garbage-collection)
3. [Back-Link Garbcol](#back-link-garbcol)


## Back-Link On Demand Garbage Collection

This version replaces the C64 BASIC V2.0 with a linear run-time behavior. Such an implementation is already known from CBM BASIC 4.0, later part of BASIC 3.5 on the CBM 264 line and of BASIC 7.0 on a C128. But instead of reserving the space for the back-link structure on each string (which costs at least two bytes), it uses a back-link structure with **no additional memory usage**, and which will be built up on demand just right before the garbage collection. The string heap management (disposing memory of a string) needs special handling which is accomplished by hooking into three critical locations in the BASIC interpreter (in addition to the obligatory GC routine hook).
Because the hooks needs patching the interpreter this is easily achieved on C64 by copying the BASIC interpreter into RAM and place the hooks whereever needed. It costs 8 K byte of RAM (underlay of the ROM).

* [Source+Binaries (BLOD-GC)](src/BLOD-GC)
* [D64 image (BLOD-GC)](images/blodgc-2021-distrib.d64)


## Super Garbage Collection

A buffer-based implementation for C64 BASIC V2.0 with nearly linear run-time (in opposite to the standard implemenation which is quadratic according to the active string count).

The original implementation is from 1985, has been optimized 2013 and enriched 2019/2020 with additional variants offering an alternative hook method which is IRQ based and offer more flexibility for the usage of the buffer area. There is no need anymore to copy the BASIC ROM into RAM to patch the legacy garbage collector from BASIC V2.0. This offers the opportunity to place the buffer (8 K in size) in RAM under the BASIC or KERNAL ROM.

* [Source+Binaries (JK-GC)](src/JK-GC)
* [D64 image (JK-GC)](images/supergc-2020-distrib.d64)
* [Tools](tools)

## Back-Link Garbcol

This is a back-link implemention similar to the CBM's implementation found in BASIC 4.0, but which is leaner and will fit info a the C64 ROMs (BASIC and KERNAL) replacing the standard GC routine. It has been published in the magazine 64'er issue from October 1988, but has some severe flaws. A bug-fixed and optimized version will be provided also (from 2013).

* [Source+Binaries (GARBCOL)](src/GARBCOL)
