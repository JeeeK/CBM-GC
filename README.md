CBM-GC
======

Commodore 8-Bit Garbage Collection Implementations

Back-Link On Demand Garbage Collection
--------------------------------------

This version replaces the C64 BASIC V2.0 with a linear run-time behavior. It uses a back-link structure which will be built up on demand just right befor the garbage collection. The string heap management (disposing memory on the) needs special handling which is accomplished by hooking three critical locations in the BASIC interpreter (in addition to the GC routine hook).

* [Source (BLOD-GC)](src/BLOD-GC)
* [D64 image (BLOD-GC)](images/blodgc-2021-distrib.d64)


Super Garbage Collection
------------------------

A buffer-based implementation for C64 BASIC V2.0 with nearly linear run-time (in opposite to the standard implemenation which is quadratic according to the active string count).

The original implementation is from 1985, has been optimized 2013 and enriched 2019/2020 with additional variants offering an alternative hook method which is IRQ based and offer more flexibility for the usage of the buffer area. There is no need anymore to copy the BASIC ROM into RAM to patch the legacy garbage collector from BASIC V2.0. This offers the opportunity to place the buffer (8 K in size) in RAM under the BASIC or KERNAL ROM.

* [Source (JK-GC)](src/JK-GC)
* [D64 image (JK-GC)](images/supergc-2020-distrib.d64)
* [Tools](tools)
