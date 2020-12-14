;
; *************************
; *  GARBAGE  COLLECTION  *
; *   from Johann Klasek  *
; *   j AT klasek DOT at  *
; * 1985-12-27 VERS. 1.1  *
; * 2013-11-24 VERS. 2.0  *
; * 2019-02-15 VERS. 2.1  *
; * 2020-12-14 VERS. 2.2  *
; *************************
;
; Collects unused (garbage) strings on the string heap,
; replacing the BASIC 2.0 garbage collector on a C64.
; Only those locations which is used by the legacy garbage
; collector are in use here, some other are restored after
; finishing the run.

; Start of code ...

!ifdef start {
	*=start
} else {
	*= $C500
}

; Options:

; Enable the patch hook (the BASIC ROM copy in RAM from $A000 to $BFFF)
; - otherwise the IRQ hook method is used.
;basic_patch = 1
;
;	Space usage notes:
;	 * The normal hook method into BASIC RAM copy takes  48 bytes.
;	 * The IRQ hook is more complex and consumes 244 bytes,
;	   at least 60 bytes are needed just to provide the
;	   a block-copy routine because the ROM routine is not
;	   available

; If active the RAM area below the BASIC ROM is used as buffer, 
; otherwise the RAM under the KERNAL ROM.
; This opens the possibility to use the RAM area from $E000 to $FFFF,
; e.g. as memory for hires graphic.
;basic_rom_buffer = 1

; Use the original MOVBLOCK routine (a copy from the ROM), which
; is only allowed if option basic_patch is not selected.
; Otherwise an optimized version is used.
;orig_movblock = 1

; Do not display an activiation mark on screen
;no_indicator = 1


; Option dependencies:

; If the BASIC patch hook is selected, the original MOVBLOCK routine
; could be easily used.
!ifdef basic_patch {
orig_movblock = 1
}

; In case of the BASIC patch variant the buffer must not placed in 
; the RAM area below the BASIC ROM!
!ifdef basic_patch {
  !ifdef basic_rom_buffer {
    !error "Invalid option: basic_rom_buffer and basic_patch must no active at the same time!"
  }
}


; BASIC system variables

TOSS     = $19		; Top of String Descriptor Stack
EOSS     = $22		; End of String Descriptor Stack +1
TSSP     = $16		; Current String Descriptor Stack pointer

VARTAB   = $2D		; End of BASIC program = begin of variable area
ARYTAB   = $2F		; End of variables = begin of arrays
STREND   = $31		; End of arrays = lowest possible string heap address
FRETOP   = $33		; Current string heap address
MEMSIZ   = $37		; Highest RAM address for BASIC, start of
			; string heap growing downwards.
MEMBEG   = STREND	; String heap memory begin = STREND
MEMEND   = MEMSIZ	; String heap memory end

V_IRQ    = $0314	; IRQ vektor, 2 bytes

; variables

STRPTR   = FRETOP	; String pointer = FRETOP
STRDP    = $22		; String descriptor address,
			; overwritten by original MOVBLOCK routine!
RNGBEG   = $4C		; Region start
NEWPTR	 = $4E		; New string pointer
PTR      = $50		; Array pointer
LEN      = $52		; string length
; $54-$56 belegt
STAT     = $57		; String status, for values in use,
			; see STAT_* below
; $58-5B is overwritten by MOVBLOCK!
STRADR   = $58		; String address (temp.)
			; (MOVBLOCK: destination end address +1/destination start
RNGEND   = $5D		; Region end
BUFPTR   = $5F		; Buffer pointer
			; (MOVBLOCK: source start!)

CPTR     = $22		; Pointer for installer routine

; Zeropage area to be saved
ZPSTART  = $4C		; First byte to save
ZPEND    = $52		; Last byte to save
ZPLEN    = ZPEND-ZPSTART+1
			; Count of bytes to save

; Constants

; for variable STAT (string status):
STAT_SDS = 5		; Is on String Descriptor Stack
STAT_VAR = 3		; Is a simple variable
STAT_ARY = 1		; Is in a array

; Memory configuration for PROCPORT:
MEMROM   = %00110111	; BASIC+KERNAL ROM, $37
MEMBAS   = %00110110	; BASIC RAM+KERNAL ROM, $34
MEMRAM   = %00110101	; BASIC+KERNAL RAM, $35

; for activity indicator
MARKCHAR = "*"          ; Indicator character
MARKCOL  = 9            ; Indicator color (red)
MARKOFF  = 40*25-1      ; indicator position (lower right corner)
MARKVPOS = VIDBASE+MARKOFF
MARKCPOS = COLBASE+MARKOFF


; Memory locations

GARBCOL  = $B526	; Entry point of the legacy GC

BASIC    = $A000        ; BASIC ROM
KERNAL   = $E000        ; KERNAL ROM
ROMSIZE  = $2000        ; ROM length, 8 Kbyte

			; Buffer:
!ifndef basic_rom_buffer {
BUF	 = KERNAL	; Buffer under KERNAL ROM
} else {
BUF	 = BASIC	; Buffer under BASIC ROM
}
BUFSIZE  = ROMSIZE	; Buffer size

VIDPAGE	 = $288		; Page of video RAM
VIDBASE  = $0400	; Video RAM
COLBASE  = $D800	; Color RAM

PROCPORT = $01		; Processor port


; Debugging

;!set debug=1


; Installer

INSTALL

	!byte $2C	; Opcode BIT absolute, Argument 
			; contains the signature, acts as NOP.
	!text "GC"	; Signature for the loader,
			; the same an on a fixed
			; location for all variants!
!ifdef basic_patch {

	; BASIC-ROM/RAM patch hook

MOVBLOCK = $A3BF	; Move-block in BASIC ROM
			; destroys $58/$59/$5A/$5B/$22

	; copy BASIC into RAM to patch the GC routine
	LDA #MEMROM
	STA PROCPORT	; All ROM (where to copy from)
	LDY #<BASIC	; ROM start
	STY CPTR
	LDA #>BASIC
	STA CPTR+1	; BASIC ROM start
	LDX #>($2000)	; BASIC ROM length in pages
CPYROM	LDA (CPTR),Y	; Read from ROM
	STA (CPTR),Y	; Write to RAM
	INY
	BNE CPYROM
	INC CPTR+1	; Next page
	DEX		; Page counter
	BNE CPYROM
	LDA PROCPORT	; Switch to RAM
	AND #%11111110	; "BASIC off" mask
	STA PROCPORT
	LDA #<COLLECT	; Write "JMP COLLECT"
	STA GARBCOL+1	; patch code.
	LDA #>COLLECT
	STA GARBCOL+2
	LDA #$4C	; The "JMP" opcode
	STA GARBCOL
	RTS
} else {

	; IRQ hook

	SEI
	LDA V_IRQ	; Current IRQ routine
	LDX V_IRQ+1
	CMP #<(IRQ)	; Already hooked in?
	BNE HOOK
	CPX #>(IRQ)
	BEQ INSTEXIT	; Vektor bereits installiert
HOOK
	STA ORIGIRQ	; Keep old vector
	STX ORIGIRQ+1
	LDA #<(IRQ)	; New GC routine ...
	LDX #>(IRQ)
	STA V_IRQ	; hooked in.
	STX V_IRQ+1
INSTEXIT
	CLI
	RTS

; The IRQ routine tests if the old GC is running with following
; conditions:
;   * the stack contains the caller PC for the Open-Space-for-Memory routine ($B62B-1)
;   * on the stack is one of three caller PC for the Search-for-Next-String routine
;   * the PC lies within the range from $B526 to $B63D.
; Particular states have to be handled with a correction of the state
; for these cases:
;   1. If the PC lies within the range from GC_CRIT_START ot GC_CRIT_END
;      the string descriptor is inconsistent.
;      $59 is already incremented if GC_CRIT_59 is reached, otherwise
;      an additional
;         INC $59
;      is needed.
;      Set the high byte:
;         LDY $55
;         INY
;         INY
;         LDA $59
;         STA ($4E),y
;      Point 3 below corrects the whole descriptor already
;      (after tranfering the string), but it doesn't help much
;      - save only one byte, but extents the code complexity.
;   2. If the PC lies within the range from GC_PHP_START to GC_PHP_END
;      the stack is inconsistent -> one byte must be removed from stack.
;   3. If the subroutine "Open Space in Memory" is interrupted
;      (recognised by the calling address), the current transfer
;      of the string has to be finished. This is accomplished by
;      normaly return from interrupt, but the changed caller address
;      for the subroutine's RTS transfers the control to the
;      correction code, which fixes the descriptor and passes
;      the control to the new GC.
;   4. If the subroutine "Search for Next String" is interrupted
;      (recognised by one of the three possible calling addresses)
;      the address of the caller is still on stack. The RTI
;      passes control to a correction routine, which removes
;      two bytes (the caller address) from stack and branches
;      to the new GC.
;
; Otherwise, if the interrupted PC lies within the GC code range
; the new GC can be called directly.
;
; We don't have to bother with the value of Top of String Heap ($33/$34),
; no matter where it is interrupted, because the value is already
; overwritten and will be recalculated by the new GC.
; 

GC_START      = $B526	; PC within this range -> GC active
GC_END        = $B63C
GC_CRIT_START = $B632	; PC within this range -> 
			; descriptor has to be corrected!
GC_CRIT_END   = $B638
GC_CRIT_59    = $B635   ; PC beginning with this addr.: $59 is correct
GC_PHP_START  = $B58A	; PC within this range -> 
			; remove SR (coming from a PHP) from stack!
GC_PHP_END    = $B598	; Location of the PLP instruction

CALLER_OPSP   = $B628+2	; PC-1 of the return address from a JSR $A3BF
			; call (Open Space for Memory) at $B628.
CALLER_SNS1   = $B561+2 ; PC-1 of the return addresses possible
CALLER_SNS2   = $B548+2 ; if in routine "Search for Next String"
CALLER_SNS3   = $B5B8+2

IRQ
	; IRQ stack frame:
	; $104,X status register
	; $105,X low byte PC
	; $106,X high byte PC
	; possibly the calling routine:
	; $107,X low byte of the caller (return-PC)-1
	; $108,X high byte of the caller (return-PC)-1

	LDA $108,X	; Caller's PC high
	TAY
	LDA $107,X	; Caller's PC low
	; callers return-PC-1 in A/Y
			; Are we in "Open Space"?
	CMP #<(CALLER_OPSP)
	BNE CHK_SNS
	CPY #>(CALLER_OPSP)
	BNE CHK_SNS
IN_OPSP
	; Go back with RTI to the interrupted
	; copy-routine to ensure that the string
	; is completely moved. The return address
	; used by the following RTS passes control
	; to the correction routine (for the
	; descriptor) to finally start with the
	; new GC.
	LDA #<(CORR_STR-1)
	STA $107,X	; RTS expects the address-1 on stack
	LDA #>(CORR_STR-1)
	STA $108,X	; Always >0
	BNE CONT	; Always go back by RTI

CORR_STR
	LDY $55		; Descriptor offset
	INY		; String length
	LDA $58		; Put string address low
	STA ($4E),Y	; into the descriptor
	INY
	INC $59		; String address high from MOVBLOCK
	LDA $59		; is one page below, so correct it
	STA ($4E),Y	; and put it into the descriptor
	BNE START_COLLECT
			; Branch always, because always >0

	
CHK_SNS			; We are in "Search for Next String"?
	CPY #>(CALLER_SNS1)
			; High byte is the same for all three addresses!
	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
	  !error "High-Byte of CALLER_SNS* are different. They must be all the same!"
	}
	BNE CHK_PC	; Check only the low byte of these addresses ...
	CMP #<(CALLER_SNS1)
	BEQ IN_SUB
	CMP #<(CALLER_SNS2)
	BEQ IN_SUB
	CMP #<(CALLER_SNS3)
	BNE CHK_PC

IN_SUB
	LDA #<(SKIPSUB) ; Redirect by changing the interruption PC
IN_PHP
	STA $105,X	; Low byte
	LDA #>(SKIPSUB)
	STA $106,X	; High byte
			; The following RTI branches to SKIPSUB
			; where the caller's address is taken from stack.

CONT	JMP (ORIGIRQ)	; Pass control to the pre-hooked code (chain).

CHK_PC
	LDA $106,X	; Check interruption PC
	TAY		; High byte
	LDA $105,X	; Low byte
	CPY #>(GC_START)
	BCC CONT	; Below GC routine
	BNE +		; Past GC beginning
	CMP #<(GC_START)
	BCC CONT	; Below GC routine
+	CPY #>(GC_END+1)
	BCC ++		; In GC routine!
	BNE CONT	; Above GC routine
	CMP #<(GC_END+1)
	BCS CONT	; Above GC routine
++
	; The old GC routine has been interrupted!
	; Are there any special ranges where further action is required?

	; In PHP/PLP section?
	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
	  !error "High byte of GC_PHP_START and GC_PHP_END differs!"
	}
	CPY #>(GC_PHP_START)
	BNE +		; Not in right page, to next range test
	CMP #<(GC_PHP_START)
	BCC +		; Below, no stack correction
	CMP #<(GC_PHP_END+1)
	BCS +		; Above, no stack correction
	LDA #<(SKIPPHP) ; RTI called routine - removes one byte from stack
	BCC IN_PHP	; C always 0, delays SKIPHP to past RTI
+
	; In critical section?
	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
	  !error "High byte of GC_CRIT_START and GC_CRIT_END differs!"
	}
	CPY #>(GC_CRIT_START)
	BNE +		; Not in right page, to next range test
	CMP #<(GC_CRIT_START)
	BCC +		; Below, no stack correction
	CMP #<(GC_CRIT_END+1)
	BCS +		; Above, no stack correction

	; Descriptor correction: set the descriptor's high byte with the
	; string address, the low byte has been already set.
	; Otherwise the address in the descriptor would be incosistent!
	; Caution: The content of $59 is starting with GC_CRIT_59 already
	; right, but otherwise it has to be corrected by adding 1.
 
	CMP #<(GC_CRIT_59)
	BCS ++          ; $59 already incremented,
	INC $59         ; otherwise correct it!
++	LDY $55		; Descriptor offset
	INY		; String length
	INY		; String address low (is already set!)
	LDA $59		; MOVBLOCK high byte, one page to below!
	STA ($4E),Y	; Just set string address high

	; The previous part could theoretically use the descriptor
	; correction code at CORR_STR, but this is normally called
	; in IRQ context which prevents direct usage. It would be
	; possible if RTI calls CORR_STR instead of START_COLLECT.
	; But this would also consume 7 bytes to accomplish, saving
	; only one byte at the expense of readability. Hence, this
	; way is not taken.
+
	; call COLLECT by means of RTI:
TO_COLLECT
	LDA #<(START_COLLECT)
			; Change the return-address on stack for RTI
	STA $0105,X	; Low byte
	LDA #>(START_COLLECT)
	STA $0106,X	; High byte
	BNE CONT	; IRQ continued, RTI starts the new GC

SKIPSUB			; Open-Space or Search-for-Next-String routine aborted:
	PLA		; Called by RTI, remove the caller PC (for RTS)
			; or
SKIPPHP			; remove the PHP
	PLA		; transfer execution directly to the new GC (COLLECT).
START_COLLECT
	LDA #3
	STA $53		; Step size to the next descriptor set to
			; the defined start value (is not initialized
			; by old GC!).
}

; *** Garbage Collector

COLLECT
!ifdef debug {
	JSR gra_on	; Enable graphic (buffer visualization)
}

	; save zero-page
	LDX #ZPLEN	; Counter and index
SAVLOOP	LDA ZPSTART-1,X	; Index runs from count to 1
	STA SAVE-1,X	; Save area
	DEX
	BNE SAVLOOP

!ifndef no_indicator {
			; X is zero from before!
	STX CPTR	; Pointer low byte = 0
	LDX VIDPAGE	; Startpage of video RAM
	!if (>MARKOFF) >= 1 {
	INX
	!if (>MARKOFF) >= 2 {
	INX
	!if (>MARKOFF) >= 3 {
	INX
	} } }
	; X contains now the page plus the offset's high byte
	STX CPTR+1
	LDY #<(MARKOFF)
	LDA (CPTR),Y	; Activity indicator on screen:
	STA ORIGVID	; Save current character
	LDA #MARKCHAR
	STA (CPTR),Y	; Set mark character
	LDA MARKCPOS	; Same for the color information
	STA ORIGCOL	; Save current color
	LDA #MARKCOL
	STA MARKCPOS	; Set mark color
}

	LDA MEMEND	; Set string pointer
	LDX MEMEND+1	; and region start
	STA STRPTR	; to memory end.
	STX STRPTR+1
	STA RNGBEG
	STX RNGBEG+1

; *** The region where the strings are searched

;                        STRADR
;       +-------------------------------------+
;       |                                     |
;       |                                     V
;   +-+-+-+      +-----------------------+----------+------+------------+
;   |L|PTR|      |        not yet        | searched | free |   treated  |
;   | |   |      |   handled strings     | strings  |      |   strings  |
;   +-+-+-+      +-----------------------+----------+------+------------+
;    ^            ^                       ^          ^      ^            ^
;    |            |                       |          |      |            |
;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
;                                                           =FRETOP
;   SDS,VAR,ARY  |<-------------------- string heap -------------------->|
;
; The region RNGBEG to RNGEND (searched strings) has to be reduced by 256 bytes 
; from the buffer size, because a string might start on the end of the region
; and could exceed the region by maximal 254 bytes. This "overflow"
; needs to fit into the buffer and so a page is reserved for this!

NEXTBLOCK
	LDA STRPTR	; NEWPTR pulled along
	STA NEWPTR	; with BUFPTR in parallel.
	LDA STRPTR+1
	STA NEWPTR+1
	LDX RNGBEG	; Region already at end
	LDA RNGBEG+1	; of string heap?
	CPX STREND
	BNE +
	CMP STREND+1
	BEQ EXIT	; Yes -> finished
+
	STX RNGEND	; Move by buffer length - 256
	STA RNGEND+1	; down to lower addresses.
	!if <BUFSIZE > 0 {
	  !error "BUFSIZE is not a multiple of 256 ($100)!"
	}
	SEC		
	SBC #(>BUFSIZE-1)
			; Region length in pages,
			; could be exceeded by max. 254 bytes!
	BCC LASTRANGE	; < 0 = underflow (for sure <STREND)
	STA RNGBEG+1
	CPX STREND	; End of string heap reached?
	SBC STREND+1
	BCS STRINRANGE	; Start of region >= bottom of string heap
LASTRANGE
	LDA STREND	; Start of region = start of free
	LDX STREND+1	; Memory area (bottom of string heap)
	STA RNGBEG	; 
	STX RNGBEG+1	; 
	BNE STRINRANGE	; Always, because high byte >0


; *** Garbage collection finished

EXIT
	; Zero-page registers
	LDX #ZPLEN	; Count and index
RESLOOP	LDA SAVE-1,X	; Index runs from count to 1
	STA ZPSTART-1,X	; Restore from save area
	DEX
	BNE RESLOOP

!ifndef no_indicator {
			; X is zero from before!
	STX CPTR	; Pointer low byte = 0
	LDX VIDPAGE	; Startpage of video RAM
	!if (>MARKOFF) >= 1 {
	INX
	!if (>MARKOFF) >= 2 {
	INX
	!if (>MARKOFF) >= 3 {
	INX
	} } }
	; X contains now the page plus the offset's high byte
	STX CPTR+1
	LDY #<(MARKOFF)
	LDA ORIGVID	; Clear activation indicator:
	STA (CPTR),Y	; restore character on screen
	LDA ORIGCOL	; and its color.
	STA MARKCPOS
}
!ifdef debug {
	JSR gra_off
}
	RTS


; *** Find all strings within the region

STRINRANGE
!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA #>(BUF+BUFSIZE)
	STA BUFPTR+1
	LDA #<(BUF+BUFSIZE)
	STA BUFPTR
} else {
			; Special case: buffer end $FFFF
	LDA #0		; Set buffer pointer start value 
	STA BUFPTR	; to $10000 (65536) = 0
	STA BUFPTR+1
}
	SEC		; Initialize search
	!byte $24	; BIT ZP, skip next instruction
NEXTSTR	
	CLC		; Continue search
NEXTSTR1
	JSR GETSA	; Fetch next string address
	BEQ COPYBACK	; No String found anymore
			; Address in X/Y

	TYA		; High byte
	CPX RNGEND	; X/A >= RNGEND:
	SBC RNGEND+1	; Above region, try
	BCS NEXTSTR	; next string!

	TYA		; High Byte
	CPX RNGBEG	; X/A < RNGBEG:
	SBC RNGBEG+1	; Below the region, so
	BCC NEXTSTR1	; next string!
			; Within the region:
	LDA BUFPTR	; Carry flag always set
	SBC LEN		; Buffer pointer moved
	STA BUFPTR	; down by string length.
	BCS +		; Check high byte overflow
	DEC BUFPTR+1

+	STY STRADR+1	; Save as string address
	STX STRADR	; for copy action.

	LDY LEN		; String length (always > 0)
	BNE NBENTRY	; Always start with decrement
NEXTBYT	LDA (STRADR),Y	; Copy string to buffer,
	STA (BUFPTR),Y	; write through to RAM below ROM.
NBENTRY	DEY		; Index and counter
	BNE NEXTBYT
	LDA (STRADR),Y	; Also the 0th byte,
	STA (BUFPTR),Y	; extra handled

	SEC		; New string address:
	LDA NEWPTR	; Simply pull along
	SBC LEN		; the pointer, by
	STA NEWPTR	; subtract the length.
	BCS +		; 
	DEC NEWPTR+1	; High byte overflow
+
	JSR CORR	; Fix the string address in the
			; descriptor, Z=0 on leave.
	BNE NEXTSTR	; Always, continue with next string


; *** Transfer buffer back to the string heap

; 0 ------------------------------------------- FFFF	
;      destination                  source
;          +--------------------------+
;          |                          |
;          V                         /^\
;     |||||||||||                |||||||||||
;     ^          ^               ^          ^ 
;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)

COPYBACK
!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA BUFPTR	; Buffer is empty ...
	CMP #<(BUF+BUFSIZE)
	BNE +
	LDA BUFPTR+1	; if pointer is still on end
	CMP #>(BUF+BUFSIZE)
	BEQ NOCOPY	; Skip copy if buffer is empty,
+			; to NEXTBLOCK, far branch needed.
} else {
			; Special case: buffer end at $FFFF
	LDA BUFPTR	; Buffer empty
	ORA BUFPTR+1	; if pointer is 0 (at end).
	BEQ NOCOPY	; Skip copy if buffer is empty,
			; to NEXTBLOCK, far branch needed.
}

!ifdef orig_movblock {
	LDA STRPTR	; Original MOVBLOCK needs
	LDX STRPTR+1	; destination block end +1
} else {
	LDA NEWPTR	; Optimized MOVBLOCK needs
	LDX NEWPTR+1	; destination block start
}
	STA $58		; = STRADR,
	STX $59		; Depending on MOVBLOCK variant
			; end+1 or begin of destination block.

!ifdef orig_movblock {
	LDA NEWPTR	; For original MOVBLOCK only,
	LDX NEWPTR+1	; otherwise already in A/X.
}
	STA STRPTR	; New FRETOP so far
	STX STRPTR+1

!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA #<(BUF+BUFSIZE)
	STA $5A		; Source block end+1
	LDA #>(BUF+BUFSIZE)
	STA $5B
} else {
			; Special case buffer end at $FFFF
	LDA #$00	; Source block end+1
	STA $5A
	STA $5B
}
			; Source block begin = BUFPTR

	SEI		; Don't allow interrupts while
	LDA PROCPORT	; KERNAL ROM is switched off
			; to gain access to the RAM under ROM.
	PHA		; Save previous memory configuration
	LDA #MEMRAM	; With KERNAL also BASIC ROM is off!
			; Both buffer $F000 as well as $A000
			; will have both ROMs switched off.
	STA PROCPORT

	JSR MOVBLOCK	; BASIC's routine to move a block,
			; with IRQ hook a own routine must be used
			; because the BASIC ROM isn't switched in!
			; Otherwise we have the ROM copy in RAM
			; Z=1
	PLA		; Restore previous memory configuration
	STA PROCPORT	; KERNAL ROM should be active again
	CLI
NOCOPY
	JMP NEXTBLOCK	; next region


;
; *** Get String - fetch next string with length > 0
;
; ( C-flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-flag )
;
; If C=1 start from the beginning at SDS, otherwise
; continue with position STRDP and string status STAT.
; If the Z-Flag is set no string is available,
; otherwise X/Y contains the address and LEN
; the length of the string.

GETSA	BCC CHECKTYPE	; C=0 -> continue with string according to STAT
			; otherwise start with at SDS ...

; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
;
;    +-------------+
;    |             V
;    |    belegt->|<-frei
;   +-+     +-----+-----+-----+
;   | |     |S|L|H|S|L|H|S|L|H|
;   +-+     +-----+-----+-----+
;    ^       ^     ^     ^     ^
;    $16     $19   $1C   $1F   $22
;    TSSP    TOSS

DESCSTACK
	LDY #0
	STY STRDP+1	; Zero descriptor pointer high
	LDA #STAT_SDS	; Set status to SDS
	STA STAT
	LDX #TOSS	; Start of SDS
	BNE ISDSTEND	; branch always
DSTACK	LDX STRDP
NEXTDST	INX		; next descriptor
	INX
	INX
ISDSTEND
	CPX TSSP	; SDS finished?
	BEQ VARS
	LDA 0,X		; Check string length
	BEQ NEXTDST
	STA LEN		; Return variables:
	STX STRDP	; length, descriptor address
	LDA 2,X		; String address high
	TAY
	LDA 1,X		; String address low
	TAX
	TYA		; Always not zero, Z=0
	RTS		; Returns address in X/Y

; *** Look up simple variables: VARTAB to ARYTAB

VARS	LDA VARTAB	; Begin of variables
	LDX VARTAB+1
	STA STRDP
	STX STRDP+1
	LDY #STAT_VAR	; Set status to variables
	STY STAT
	BNE ISVAREND	; Branch always
VAR
NEXTVAR	CLC		; Next variable
	LDA STRDP
	ADC #7		; Advance to next variable
	STA STRDP
	BCC ISVAREND
	INC STRDP+1	; Overflow high byte
ISVAREND
	CMP ARYTAB
	BNE CHECKVAR
	LDX STRDP+1	; Variable end (=array start)?
	CPX ARYTAB+1
	BEQ ARRAYS	; Variable end reached, proceed with arrays
CHECKVAR
	LDY #0		; Variable name
	LDA (STRDP),Y	; 1st character, type in bit 7 
	BMI NEXTVAR	; No string, to next variable
	INY
	LDA (STRDP),Y	; 2nd character, type in bit 7
	BPL NEXTVAR	; No string, to next variable
	INY
	LDA (STRDP),Y	; String length
	BEQ NEXTVAR	; = 0, to next variable
	BNE RETGETSA

CHECKTYPE
	LDA STAT	; GETSA intro with C=0
	CMP #STAT_VAR	; String status?
	BCC ARRAY	; =1 -> arrays
	BEQ VAR		; =3 -> variables
	JMP DSTACK	; =5 -> SDS

; *** Look up arrays: ARYTAB to STREND

ARRAYS	STA PTR		; A/X set from simple variable processing,
	STX PTR+1	; pointing the start of arrays.
	LDY #STAT_ARY
	STY STAT	; Set status to arrays
ISARREND
	LDA PTR
	LDX PTR+1
CHKAEND	CPX STREND+1	; End of array area?
        BNE NEXTARR
	CMP STREND	; High byte matches, low byte is
			; less or equal.
	BEQ NOSTRING	; Arrays finished -> no string
NEXTARR
			; Carry always cleared because of CPX/CMP
	STA STRDP	; Start of an array
	STX STRDP+1
	LDY #0
	LDA (STRDP),Y	; Array name
	TAX		; Array type, keep it for later
	INY
	LDA (STRDP),Y
	PHP		; Array type 2nd part, keep also
	INY
	LDA (STRDP),Y	; Offset to next array
	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
	STA PTR		; Save start of following array
	INY
	LDA (STRDP),Y
	ADC PTR+1
	STA PTR+1
	PLP		; Fetch array type
	BPL ISARREND	; Not a string array
	TXA		; Fetch array type 2nd part
	BMI ISARREND	; Not string array
	INY
	LDA (STRDP),Y	; Number of dimensions
	ASL		; *2
	ADC #5		; Offset = dimensions*2+5
			; C=0 as long as dim.. <= 125
	BNE ADVDESC	; Branch always
ARRAY			; Entry on continuation
NEXTASTR
	CLC
	LDA #3		; String descriptor length
ADVDESC	ADC STRDP	; Advance to next string
	STA STRDP
	BCC +
	INC STRDP+1	; Overflow high byte
+	CMP PTR		; All array elements processed?
	BNE IS0ASTR
	LDX STRDP+1
	CPX PTR+1
	BEQ CHKAEND	; A/X = PTR, check for end of  array area
IS0ASTR
	LDY #0
	LDA (STRDP),Y	; String length
	BEQ NEXTASTR	; Next array element
RETGETSA
	STA LEN		; Return value: length
	INY
	LDA (STRDP),Y	; String address low
	TAX
	INY
	LDA (STRDP),Y	; String address high
	TAY		; Always not zero, Z=0
	RTS		; Return address in X/Y
NOSTRING
	LDA #0		; Length 0 
	STA LEN		; No string found
	RTS		; Z=1

;
; *** Correct string Address in descriptor
;
; ( NEWPTR, STRDP, STAT -> )
;
CORR	LDA STAT	; String status
	AND #%011	; Just 2 bits, giving the
	TAY		; offset to the descriptor ...
	LDA NEWPTR	;
	STA (STRDP),Y	; which differs for SDS
	INY		; and array elements!
	LDA NEWPTR+1
	STA (STRDP),Y
	RTS


;
; ***  MOVBLOCK routines (needed for IRQ hook method)
;

!ifndef basic_patch {

  !ifndef orig_movblock {
	; The optimized implementation:
	;
	; The "Open Space" routine from the BASIC ROM, $A3BF
	; isn't available while switching off the KERNAL ROM, which
	; switches the BASIC ROM off also. In this case we have to redefine
	; this task with a separate routine which is shorter and slightly
	; faster than the original from the ROM.

	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
	; Overlapping works as long as the destination address is below the
	; source block.
	; Input: $5F/$60 source start address
	;	 $5A/$5B source end address+1
	;	 $58/$59 destination start address
	; Destroyed: A, X, Y
	; Output: $58/$59 has the value destination end address +1
	;          X = 0
	;	   Y = 0 (if the block length is greater 0)
	;	   Z-flag = 1
MOVBLOCK
        SEC
        LDA $5A       ; Source end address low
        SBC $5F       ; Minus source begin address low
        TAY           ; Block length low
        LDA $5B       ; Source end address high
        SBC $60       ; Minus source begin address high
        TAX           ; Block length high, usage as DEC-DEC counter
        TYA           ; Length low
        BEQ copy      ; Nothing more to do if 0
        CLC           ; Length in A
        ADC $5F       ; Source begin address corrected by 
        STA $5F       ; low-byte offset: -(-length) -> +length
        BCS +         ; Because this is like a subtraction
        DEC $60       ; C=0 means to correct begin address high.
+
        TYA           ; Length low
        CLC
        ADC $58       ; Destination begin address corrected by
        STA $58       ; low-byte offset: -(-length) -> +length
        BCS +         ; Because this is like a subtraction
        DEC $59       ; C=0 means to correct begin address high.
+
	INX           ; Page counter (all full and one partial)
	TYA           ; Length low
	EOR #$FF      ; Negate (two's complement):
	TAY           ; NOT(X)+1
	INY
copy    LDA ($5F),Y   ; Copy source 
        STA ($58),Y   ; to destination, with increasing addresses
        INY
        BNE copy
        INC $60       ; Source high
        INC $59       ; Destination high
        DEX           ; Block counter
        BNE copy
        RTS
	; Takes 6 bytes less compared to the original routine.

  } else {

	; The original routine taken from the ROM:
	;
	; The "Open Space" routine from the BASIC ROM, $A3BF
	; isn't available while switching off the KERNAL ROM, which
	; switches the BASIC ROM off also. In this case we have to redefine
	; this task with a separate routine which is shorter and slightly
	; faster than the original from the ROM.

	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
	; Overlapping works as long as the destination's end address is 
	; above the source block.
	; Input: $5F/$60 source start address
	;	 $5A/$5B source end address+1
	;	 $58/$59 destination end address+1
	; Destroyed: A, X, Y
	; Output: $58/$59 has the value destination end address+1-256
	;          X = 0
	;	   Y = 0 (if the block length is greater 0)
	;	   Z-flag = 1

MOVBLOCK
        SEC
        LDA $5A       ; End address low
        SBC $5F       ; Minus begin address low
        STA $22       ; Length low
        TAY
        LDA $5B       ; End address high
        SBC $60       ; Minus begin address high
        TAX           ; Length high
        INX           ; Length as DEC-DEC counter
        TYA           ; Length low
        BEQ +         ; If not zero, then correct
        LDA $5A       ; the end address by low-byte offset
        SEC           ; from length.
        SBC $22       ; Length low
        STA $5A       ;
        BCS ++
        DEC $5B       ; Overflow high byte 
        SEC
++      LDA $58       ; Correct the destination end address
        SBC $22       ; by low-byte offset (length low).
        STA $58
        BCS +++	      ; Overflow high byte
        DEC $59       ; 
        BCC +++
-       LDA ($5A),Y   ; Copy source to destination
        STA ($58),Y   ; from higher to lower addresses.
+++     DEY
        BNE -
        LDA ($5A),Y   ; Source
        STA ($58),Y   ; Destination
+       DEC $5B       ; Source high
        DEC $59       ; Destination high
        DEX           ; Block counter
        BNE --
        RTS
  }
}


!ifdef debug {
!source "debug.asm"
}

!ifndef no_indicator {
ORIGVID !byte 0		; Original character of marker position
ORIGCOL !byte 0		; Original color of marker position
}
!ifndef basic_patch {
ORIGIRQ !byte 0,0	; Original IRQ vector
}
SAVE	!byte 0		; Saved zero-page variables
*=*+ZPLEN-1


