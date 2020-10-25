CBM-GC
======

Commodore 8-Bit Garbage Collection Implementations

Super Garbage Collection
------------------------

A buffer-based implementation for C64 BASIC V2.0 with nearly linear run-time (in opposite to the standard implemenation which is quadratic according to the active string count).

The original implementation is from 1985, has been optimized 2013 and enriched 2019/2020 with additional variants offering an alternative hook method which is IRQ based and offer more flexibility for the usage of the buffer area. There is no need anymore to copy the BASIC ROM into RAM to patch the legacy garbage collector from BASIC V2.0. This offers the opportunity to place the buffer (8 K in size) in RAM under the BASIC or KERNAL ROM.

See [Source (JK-GC)](src/JK-GC).
