;
; **********************************
; *  BACKLINK GARBAGE  COLLECTION  *
; *        from Johann Klasek      *
; *        j AT klasek DOT at      *
; *       2021-03-30 VERS. 1.1     *
; **********************************
;
; Collects unused (garbage) strings on the string heap,
; replacing the BASIC 2.0 garbage collector on a C64.
; Only those locations which is used by the legacy garbage
; collector are in use here.

; Start of code ...

!ifdef start {
	*=start
} else {
	*= $C500
}

; Options:

; Do not display an activiation mark on screen
;no_indicator = 1



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


; variables

HEAP     = FRETOP	; String pointer = FRETOP
STRDP    = $22		; String descriptor address (used in stage 1+3: GETSA in/out)
CPTR     = $22		; Pointer for installer routine (used in installer)
NEWHEAP  = $22		; New heap pointer (used in stage 2)

STAT     = $57		; String status, for values in use,
			; see STAT_* below (GETSA in/out)
DESC     = $58		; String descriptor address (temp.)
STR      = $5A		; Points to a string
LEN      = $5D		; String length (GETSA out)
PTR      = $5F		; Array pointer (GETSA in/out)



; Constants

; for variable STAT (string status):
STAT_SDS = 0		; Is on String Descriptor Stack
STAT_VAR = 4		; Is a simple variable
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
PATCH1   = $AA6C	; Overwrite string to variable
PATCH2   = $B66A	; String concatenation: 2nd argument handling
PATCH3   = $B726	; LEFT$() copy string

BASIC    = $A000        ; BASIC ROM
KERNAL   = $E000        ; KERNAL ROM
ROMSIZE  = $2000        ; ROM length, 8 Kbyte

VIDPAGE	 = $288		; Page of video RAM
VIDBASE  = $0400	; Video RAM
COLBASE  = $D800	; Color RAM

PROCPORT = $01		; Processor port



; Installer

INSTALL

	!byte $2C	; Opcode BIT absolute, Argument 
			; contains the signature, acts as NOP.
	!text "GC"	; Signature for the loader,
			; the same an on a fixed
			; location for all variants!

	; BASIC-ROM/RAM patch hook

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

	LDA #<HANDLE1	; let JSR in place!
	STA PATCH1+1
	LDA #>HANDLE1
	STA PATCH1+2

	LDA #<HANDLE2	; let JSR in place!
	STA PATCH2+1
	LDA #>HANDLE2
	STA PATCH2+2

	LDA #<HANDLE3	; let JSR in place!
	STA PATCH3+1
	LDA #>HANDLE3
	STA PATCH3+2

	LDA #<COLLECT	; Write "JMP COLLECT"
	STA GARBCOL+1	; patch code.
	LDA #>COLLECT
	STA GARBCOL+2

	LDA #$4C	; The "JMP" opcode
	STA GARBCOL
	RTS


; *** Handle Patch 1: LET variable overwrite
;
; Hooks at AA6C
; Replacing:
;	JSR $B6DB	; Remove only from SDS, but keep string on heap!
; Continues at AA6F:
;	LDY #$00
;	LDA ($50),Y
;	STA ($49),Y

HANDLE1
	CPY $18		; Descriptor on top of SDS?
	BNE +
	CMP $17
	BNE +
	STA $16		; Yes, remove it from SDS
	SBC #3
	STA $17
	
	; If destination variable points to string on the heap, free it.

+	LDY #0
	; $49 points to variable descriptor (in LET's destination variable)
	LDA ($49),Y	; Get string length
	BEQ LEAVE	; Variable contains no string
	TAX		; > 0, save it for later
	INY
	LDA ($49),Y	; String address low
	STA STR
	INY
	LDA ($49),Y	; String address high
	STA STR+1

	; Free STR if on heap and return

FREE
	LDA STR+1	; String address high
	CMP FRETOP+1	; Heap top high
	BCC LEAVE	; String below heap (on on heap)
	BNE ++		; String on heap
	LDA STR		; String address low
	CMP FRETOP	; Heap top low
	BCC LEAVE	; Leave when not on heap!

	LDA STR+1	; String address greater or equal FRETOP

++	CMP MEMEND+1	; String above string memory?
	BCC +++		; no
	BNE LEAVE	; yes
	LDA STR		; High byte equal, compare low byte
	CMP MEMEND
	BCS LEAVE	; Above heap
	
	; String on heap: mark it as free

+++	TXA		; Restore length
	CMP #1		; String of length 1?
	BNE ++

	LDY #0
	STA (STR),Y	; Single byte on heap contains 1
	BEQ LEAVE	; leave, always (Z=1)

++	TAY		; Length to Y (> 1!)
	DEY
	DEY		; Y: Length - 2
	STA (STR),Y	; Pre-last byte of string has length
	INY
	LDA #$FF
	STA (STR),Y	; Last byte of string with gap-marker
LEAVE	RTS



; String concatenation: free 2nd argument after copying!

;.,B65D 20 75 B4 JSR $B475       copy descriptor pointer and make string space A bytes long
;.,B660 20 7A B6 JSR $B67A       copy string from descriptor to utility pointer
;.,B663 A5 50    LDA $50         get descriptor pointer low byte
;.,B665 A4 51    LDY $51         get descriptor pointer high byte
;.,B667 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
;                                returns with A = length, X = pointer low byte,
;                                Y = pointer high byte
;.,B66A 20 8C B6 JSR $B68C       store string from pointer to utility pointer
;.,B66D A5 6F    LDA $6F         get descriptor pointer low byte
;.,B66F A4 70    LDY $70         get descriptor pointer high byte
;.,B671 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
;                                returns with A = length, X = pointer low byte,
;                                Y = pointer high byte
;.,B674 20 CA B4 JSR $B4CA       check space on descriptor stack then put string address
;                                and length on descriptor stack and update stack pointers
;.,B677 4C B8 AD JMP $ADB8       continue evaluation

; -> 

;.,B66A 20 8C B6 JSR HANDLE2     store string from pointer to utility pointer

; Only both or none of the arguments are the SDS. If the 2nd (later pushed)
; element has been poped, the first argument will be on the SDS also.

HANDLE2
	JSR $B68C	; Copy string to utility pointer's location
	LDA $50		; Descriptor address of 2nd argument
	LDY $51		; It is never top on heap, so just mark it as free
	CMP $16		; Previously popped element
	BNE LEAVE
	CPY $18		; High byte (normally 0)
	BNE LEAVE
	JSR FREESDS	; Mark already remove element from SDS as free
	LDA $6F		; Descriptor address of the first argument
	LDY $70
	JMP POPSDS	; Remove element from SDS and mark as free
	

; LEFT$(), RIGHT$(), MID$(): Free input string

;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put string address
;                                and length on descriptor stack and update stack pointers
; -> 
;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer


HANDLE3
	; A: length, copy from ($22) to ($35)
	JSR $B68C	; Copy string part into allocated space
	LDA $50
	LDY $51

	; the string itself is not top of heap, just mark as free and remove from SDS

POPSDS
	CPY $18		; Descriptor on top of SDS?
	BNE LEAVE	; RTS
	CMP $17
	BNE LEAVE	; RTS
	; free memory and pull from SDS
	JSR FREESDS	; Carry = 1 CMP above, left untouched after JSR
	LDA $17		; Top elememt on SDS
	JMP $B6E3	; Remove from SDS (A has low byte to SDS element)
			; Carry flag must be set on entry.
FREESDS
	; A/Y is pointer to string descriptor on the SDS!
	TAX		; Index in zero-page
	LDA 1,X		; String address low
	STA STR
	LDA 2,X		; String address high
	STA STR+1
	LDA 0,X		; String length
	TAX
	BNE FREE	; Length X, address STR/STR+1
	RTS		; No free if length = 0!



; *** Garbage Collector

COLLECT

!ifndef no_indicator {
	LDX #0
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



; walk through all strings and reorganize them

STAGE1
        SEC             ; Initialize search
NEXTSTR
	JSR GETSA
	BEQ STAGE2      ; No String found anymore
			; Address in X/Y, descr. at STRDP + STAT-offset

	CPY FRETOP+1	; String on heap?
	BCC NEXTSTR	; No, C=0 for GETSA continuation
	BNE +
	CPX FRETOP
	BCC NEXTSTR	; No, C=0 for GETSA continuation

+	STX STR		; Start of string which is on heap
	STY STR+1
	LDA LEN
	CMP #1		; String length 1?
	BNE ++

	; LEN 1: 
	;	copy string value into descriptor
	;	overwrite string on heap with value 1

	LDY #0
	LDA (STR),Y	; String value
	TAX
	LDA #1		; Marker for string with length 1
	STA (STR),Y	; Store marker on heap
	LDA STAT
	LSR		; Shift right gives offset, which
	TAY		; refers to STRDP leading to the descriptor
	INY		; Position string address low byte
	TXA		; String value
	STA (STRDP),Y	; Store value in descriptor (low address byte)
	LDA #0		; Mark for high byte, means on heap string
	INY		; String address high byte
	STA (STRDP),Y	; Set to zero
	CLC		; Continuation mode for GETSA
	BCC NEXTSTR	; Always

	; LEN >1:
	;	copy backlink bytes to descriptor
	;	store descriptor pointer to backlink

++	TAY		; Length
	DEY		; Index to last byte
	LDA (STR),Y
	PHA		; Last byte of string
	LDX STRDP+1
	LDA STAT
	LSR		; Shift right gives offset to the descriptor
	CLC
	ADC STRDP
	BCC +
	INX
+	PHA		; STRDP + offset low
	TXA		; X STRDP + offset high
	STA (STR),Y	; Back-link high
	DEY
	LDA (STR),Y	; Pre-last byte string
	TAX
	PLA		; STRDP + offset low
	STA (STR),Y	; Back-link low
	LDA STAT
	LSR		; Shift right gives offset, which
	TAY		; refers to STRDP leading to the descriptor
	INY		; Skip length byte
	PLA		; Last byte of string
	STA (STRDP),Y	; Store into descriptor address low byte
	TXA		; Pre-last byte of string
	INY		; =2
	STA (STRDP),Y	; Store into descriptor address high byte
	CLC		; Continuation mode for GETSA
	BCC NEXTSTR	; Always
	



; walk through heap, remove gaps and move strings

STAGE2
	LDY MEMEND+1	; Top of memory.
	LDX MEMEND	; Set new heap top
	STX NEWHEAP	; to memory end.
	STY NEWHEAP+1
			; Entry point from no-gap part
LOOP2R	STY PTR+1	; PTR comes from X
	LDY #0
LOOP2
	TXA		; PTR minus 1
	BNE +
	DEC PTR+1
+	DEX
-	STX PTR

	CPX HEAP	; PTR blow top of heap?
	LDA PTR+1
	SBC HEAP+1
	BCS +		; PTR >= HEAP
	JMP EXIT2
+
	LDA (PTR),Y	; Get back-link high
	CMP #1		; 1-byte gap
	BEQ LOOP2	; Skip it, covered later in stage 3.

	INX		; Decrement PTR, but leaving A untouched
	DEX		; PTR low = 0?
	BNE +
	DEC PTR+1
+	DEX		; PTR low
	STX PTR

	CMP #$FF	; Gap marker? (length >1)
	BNE NOGAP
			; Skip gap of a certain length ...
	LDA (PTR),Y	; Gap length
	EOR #$FF	; A is > 1
			; Carry set from CMP above!
	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
			; Never 0 because gap length > 1
	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
	TAX		; PTR low byte
	BCS -		; Position on last string byte
	DEC PTR+1	; PTR high byte, always >0
	BNE -		; Always, PTR has string address,
			; pointing to last string byte

; We have a backlink to the string:
NOGAP	STA DESC+1	; Backlink high and
	LDA (PTR),Y	; backlink low is the
	STA DESC	; descriptor address.

	LDA (DESC),Y	; Length from descriptor
	EOR #$FF
	PHA		; Needed for heap later
	LDX PTR+1	; Transfer to STR ...
			; Carry clear from CMP #$FF
	ADC #3		; -(LEN-2) + PTR -> PTR
	BNE +		; PTR already in position
			; Special case length = 2:
	INX		; compensate for the high byte decrement
	CLC		; Adding 0 with carry cleared, leaves PTR unchanged.
+	ADC PTR		; Accumulator before add. was in range 0 to FC
			; which never sets the carry!
	BCS +
	DEX		; In case of adding 0 X is already compensated.
+	STX STR+1	; STR points to string start.
	STA STR
	
	; make space on heap vor LEN bytes
	PLA		; LEN, but only complemented
	SEC		; Finalize two's complement (+1 from carry)
	ADC NEWHEAP	; HEAP - LEN -> HEAP
	STA NEWHEAP
	BCS +
	DEC NEWHEAP+1
+	
	; copy LEN bytes from STR to HEAP
	LDA (DESC),Y	; length from descriptor
	TAY		; as index
	DEY		; index = length - 2
	DEY
	BEQ +		; 0, nothing to copy
	DEY		; -1, index of last byte
	BEQ ++		; No loop if index is 0.
-	LDA (STR),Y	; Transfer byte 1 to len-1
	STA (NEWHEAP),Y
	DEY
	BNE -
++	LDA (STR),Y	; transfer byte 0
	STA (NEWHEAP),Y
+	
	; correct descriptor
	LDY #2		; Offset in descriptor
	LDA (DESC),Y	; pre-last string byte 
	PHA		; Save
	LDA NEWHEAP+1
	STA (DESC),Y	; Restore string address low
	DEY
	LDA (DESC),Y	; last string byte
	PHA		; Save
	LDA NEWHEAP	; Restore string address high
	STA (DESC),Y	; Backlink high

	DEY		; Y=0
	; Restore string bytes to backlink
	LDA (DESC),Y	; Length byte
	TAY
	DEY		; Index of last string byte
	PLA
	STA (NEWHEAP),Y	; last byte
	DEY
	PLA		
	STA (NEWHEAP),Y	; pre-last byte

	LDX STR		; PTR low byte in X
	LDY STR+1	; always >0
	JMP LOOP2R	; Loop with set PTR and reset Y
	
EXIT2
	LDA NEWHEAP	; Set rebuilt, compacted heap
	STA HEAP	; as new heap.
	LDA NEWHEAP+1
	STA HEAP+1




; Put strings with length 1 (stored in the descriptor) back on heap

STAGE3
        SEC             ; Initialize search for GETSA
        !byte $24       ; BIT ZP, skip next instruction
NEXT1STR
	CLC
	JSR GETSA
	BEQ EXIT        ; No String found anymore
			; Address in X/Y, descr. at STRDP + STAT-offset
	DEC LEN
	BNE NEXT1STR	; Loop if not length 1
	TYA		; Check string address high byte
	BNE NEXT1STR	; If not zero, string is not on heap!
			; Y is always 0.	
	TXA		; String addr low is the string byte!
	LDX HEAP
	BNE +		; Heap pointer - 1
	DEC HEAP+1
+	DEX		; Low byte used later
	STX HEAP
	STA (HEAP),Y	; stored string byte back to heap

	LDA STAT
	LSR		; Shift right gives offset, which
	TAY		; refers to STRDP leading to the descriptor
	INY		; Low byte address in descriptor
	TXA		; Heap pointer low
	STA (STRDP),Y	; stored back into descriptor
	INY
	LDA HEAP+1	; Heap pointer high
	STA (STRDP),Y	; stored back into descriptor
	BNE NEXT1STR	; Branch always, because high byte >0
	

; *** Garbage collection finished

EXIT

!ifndef no_indicator {
	LDX #0
        STX CPTR        ; Pointer low byte = 0
        LDX VIDPAGE     ; Startpage of video RAM
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
        LDA ORIGVID     ; Clear activation indicator:
        STA (CPTR),Y    ; restore character on screen
        LDA ORIGCOL     ; and its color.
        STA MARKCPOS
}
	RTS


;
; *** Get String - fetch next string with length > 0
;
; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
; 
; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
;
; If C=1 start from the beginning at SDS, otherwise
; continue with position STRDP and string status STAT.
; If the Z-Flag is set no string is available,
; otherwise X/Y contains the address and LEN
; the length of the string.

GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
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
	LDA LEN		; Always not zero, Z=0
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
	CMP #STAT_ARY	; String status?
	BEQ ARRAY	; =1 -> arrays
	BCS VAR		; =4 -> variables
	JMP DSTACK	; =0 -> SDS

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
	TAY
	LDA LEN		; Always not zero, Z=0
	RTS		; Return address in X/Y
NOSTRING
	LDA #0		; Length 0 
	STA LEN		; No string found
	RTS		; Z=1




!ifndef no_indicator {
ORIGVID !byte 0		; Original character of marker position
ORIGCOL !byte 0		; Original color of marker position
}


