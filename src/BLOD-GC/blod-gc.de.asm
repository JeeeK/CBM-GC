;
; **********************************
; *  BACKLINK GARBAGE  COLLECTION  *
; *        von  Johann Klasek      *
; *        j AT klasek DOT at      *
; *       2021-03-30 VERS. 1.1     *
; **********************************
;
; Räumt Stringspeicher auf, indem Lücken von unbenutzten String
; entfernt werden und ersetzt die C64 BASIC V2.0 Garbage Collection.
; Es werden nur jene Speicherstellen benutzt, die auch die normale
; GC verwendet.

; Start des Codes ...

!ifdef start {
	*=start
} else {
	*= $C500
}


; Optionen:

; Bildschirmmarkierung nicht verwenden
;no_indicator = 1


; BASIC Systemvariablen

TOSS     = $19		; Top of Stringstack
EOSS     = $22		; End of Stringstack +1
TSSP     = $16		; Temp. Stringstack Pointer

VARTAB   = $2D		; BASIC-Programmende = Variablenanfang
ARYTAB   = $2F		; Variablenende = Arraybereichanfang
STREND   = $31		; Arraybereichende = unterste String-Heap-Adresse
FRETOP   = $33		; aktuelle String-Heap-Adresse
MEMSIZ   = $37		; höchste RAM-Adresse für BASIC, Start
			; des nach unten wachsenden String-Heaps
MEMBEG   = STREND	; Memory begin = STREND
MEMEND   = MEMSIZ	; Memory end


; Variablen

HEAP     = FRETOP	; String-Pointer = FRETOP
STRDP    = $22		; String-Descriptor-Address (Stage 1+3: GETSA in/out)
CPTR     = $22		; Pointer für Install-Routine
NEWHEAP  = $22		; Neuer Heap-Pointer (Stage 2)

STAT     = $57		; String-Status, Werte siehe
			; STAT_* weiter unten (GETSA in/out).
DESC     = $58		; String-Address (temp.)
STR      = $5A		; Points to a string
LEN      = $5D		; String length (GETSA out)
PTR      = $5F		; Array pointer (GETSA in/out)


; Konstanten

; für Variabe STAT (String-Status):
; for variable STAT (string status):
STAT_SDS = 0		; String-Descriptor-Stack
STAT_VAR = 4		; einfache Variablen
STAT_ARY = 1		; Array

; für PROCPORT, Speicherkonfigurationen:
MEMROM   = %00110111	; Basic+Kernal ROM, $37
MEMBAS   = %00110110	; Basic RAM+Kernal ROM, $34
MEMRAM   = %00110101	; Basic+Kernal RAM, $35

; für Aktivitätsanzeige
MARKCHAR = "*"		; Markierungszeichen
MARKCOL  = 9		; Markierungsfarbe (rot)
MARKOFF  = 40*25-1	; Markenposition (Ecke rechts unten)
MARKVPOS = VIDBASE+MARKOFF
MARKCPOS = COLBASE+MARKOFF


; Speicherorte

GARBCOL  = $B526	; Einsprungpunkt der GC
PATCH1   = $AA6C	; String-Variable überschreiben
PATCH2   = $B66A	; String-Addition: Behandlung des 2. Arguments
PATCH3   = $B726	; LEFT$() String kopieren

BASIC    = $A000	; BASIC-ROM
KERNAL   = $E000	; KERNAL-ROM
ROMSIZE  = $2000	; ROM-Länge, 8 KByte

VIDPAGE  = $288		; Video RAM Page
VIDBASE  = $0400	; Video RAM
COLBASE  = $D800	; Color RAM

PROCPORT = $01		; Prozessorport


; Installer


INSTALL

	!byte $2C	; Opcode von BIT absolute, Argument
			; enthält die Signature, wirkt als NOP
	!text "GC"	; Signatur für den BASIC-Loader,
			; immer an gleicher Stelle für
			; alle Varianten!


	; BASIC ins RAM kopieren, um die GC-Routine
	; zu patchen ...
	LDA #MEMROM
	STA PROCPORT	; alles ROM (also vom ROM kopieren)
	LDY #<BASIC	; ROM-Beginn
	STY CPTR
	LDA #>BASIC
	STA CPTR+1	; BASIC-ROM Anfang
	LDX #>($2000)	; BASIC-ROM Länge in Pages
CPYROM	LDA (CPTR),Y	; ROM lesen
	STA (CPTR),Y	; RAM schreiben
	INY
	BNE CPYROM
	INC CPTR+1	; nächste Page
	DEX		; Page-Zähler
	BNE CPYROM

	LDA PROCPORT	; auf RAM umschalten
	AND #%11111110	; "BASIC-ROM aus"-Maske
	STA PROCPORT

	LDA #<HANDLE1	; JSR bereits vorhanden!
	STA PATCH1+1
	LDA #>HANDLE1
	STA PATCH1+2

	LDA #<HANDLE2	; JSR bereits vorhanden!
	STA PATCH2+1
	LDA #>HANDLE2
	STA PATCH2+2

	LDA #<HANDLE3	; JSR bereits vorhanden!
	STA PATCH3+1
	LDA #>HANDLE3
	STA PATCH3+2

	LDA #<COLLECT	; "JMP COLLECT"
	STA GARBCOL+1	; patchen ...
	LDA #>COLLECT
	STA GARBCOL+2

	LDA #$4C	; JMP-Opcode
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
;
HANDLE2
	JSR $B68C	; Copy string to utility pointer's location
	LDA $50		; Descriptor address of 2nd argument
	LDY $51		; It is never top on heap, so just mark it as free
	CMP $16		; Previously popped element
	BNE LEAVE
	CPY $18		; High byte (normally 0)
	BNE LEAVE
	JSR FREESDS	; mark already remove element from SDS as free
	LDA $6F
	LDY $70
	JMP POPSDS	; remove element from SDS and mark as free
	

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
	JSR FREESDS
	LDA $17		; Top elememt on SDS
	JMP $B6E3	; remove from SDS (A low byte to SDS element)
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
	STX CPTR	; Zeiger Low-Byte = 0
	LDX VIDPAGE	; Start-Page des Video-RAMs
	!if (>MARKOFF) >= 1 {
	INX
	!if (>MARKOFF) >= 2 {
	INX
	!if (>MARKOFF) >= 3 {
	INX
	} } }
	; X enthält die Basis-Page und die Pages des Offsets
	STX CPTR+1
	LDY #<(MARKOFF)
	LDA (CPTR),Y	; Kontrollanzeige Bildschirm
	STA ORIGVID	; altes Zeichen sichern
	LDA #MARKCHAR
	STA (CPTR),Y	; Zeichen der Marke setzen
	LDA MARKCPOS	; gleiches für die Farbe
	STA ORIGCOL	; Farbe sichern
	LDA #MARKCOL
	STA MARKCPOS	; Farbe der Marke setzen
}



; walk through all strings and reorganize them

STAGE1
        SEC             ; Initialize search
NEXTSTR
	JSR GETSA
	BEQ STAGE2      ; No String found anymore
			; Adresse in X/Y, Descr. STRDP mit Offset von STAT

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
	LDA #0		; 0-Byte Markierung,
	INY		; für Strings, die am Heap liegen.
	STA (STRDP),Y	; Stringadresse High-Byte
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
			; Adresse in X/Y, Descr. STRDP mit Offset von STAT
	DEC LEN
	BNE NEXT1STR	; Loop if not length 1
	TYA		; High-Byte Stringadresse nicht 0 -> nicht am Heap
	BNE NEXT1STR
			; Y ist bereits 0
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
	STX CPTR	; Zeiger Low-Byte = 0
	LDX VIDPAGE	; Start-Page des Video-RAMs
	!if (>MARKOFF) >= 1 {
	INX
	!if (>MARKOFF) >= 2 {
	INX
	!if (>MARKOFF) >= 3 {
	INX
	} } }
	; X enthält die Basis-Page und die Pages des Offsets
	STX CPTR+1
	LDY #<(MARKOFF)
	LDA ORIGVID	; Kontrollanzeige löschen
	STA (CPTR),Y	; und alten Zustand wieder
	LDA ORIGCOL	; herstellen.
	STA MARKCPOS
}
	RTS


;
; *** Get String - nächsten String mit Länge ungleich 0
;
; ( C-Flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-Flag )
;
; STAT >> 1 -> Versatz zum Descriptor relativ zu Zeiger STRDP.
;
; Bei C=1 wird beim SDS gestartet, sonst von der letzten
; Position gemäß STRDP und String-Status STAT.
; Das Z-Flag ist gesetzt, wenn kein String mehr
; vorhanden ist, sonst in X/Y die Adresse und in LEN
; die Stringlänge.

GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
			; sonst Start bei SDS ...

; *** Suche in String-Descriptor-Stack (SDS): TOSS bis TSSP
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
	STY STRDP+1	; Descriptor High-Byte auf 0
	LDA #STAT_SDS	; Status: SDS
	STA STAT
	LDX #TOSS	; SDS-Start
	BNE ISDSTEND	; immer verzweigen
DSTACK	LDX STRDP
NEXTDST	INX		; nächster Descriptor
	INX
	INX
ISDSTEND
	CPX TSSP	; fertig mit SDS?
	BEQ VARS
	LDA 0,X		; prüfe String-Länge
	BEQ NEXTDST
	STA LEN		; Rückgabevariablen:
	STX STRDP	; Länge und Descriptor-Adresse
	LDA 2,X		; String-Adr. high
	TAY
	LDA 1,X		; String-Adr. low
	TAX
	LDA LEN		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour

; *** Suche in einfachen Variablen: VARTAB bis ARYTAB

VARS	LDA VARTAB	; Variablenanfang
	LDX VARTAB+1
	STA STRDP
	STX STRDP+1
	LDY #STAT_VAR	; Status: einfache Variablen
	STY STAT
	BNE ISVAREND	; immer verzweigen
VAR
NEXTVAR	CLC		; nächste Variable
	LDA STRDP
	ADC #7		; Variablenlänge
	STA STRDP
	BCC ISVAREND
	INC STRDP+1	; Überlauf High-Byte
ISVAREND
	CMP ARYTAB
	BNE CHECKVAR
	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
	CPX ARYTAB+1
	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
CHECKVAR
	LDY #0		; Variablenname
	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
	BMI NEXTVAR	; kein String, nächste V.
	INY
	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
	BPL NEXTVAR	; kein String, nächste V.
	INY
	LDA (STRDP),Y	; String-Länge
	BEQ NEXTVAR	; = 0, nächste Variable
	BNE RETGETSA

CHECKTYPE
	LDA STAT	; GETSA-Einstieg mit C=0
	CMP #STAT_ARY	; String-Status?
	BEQ ARRAY	; =1 -> Arrays
	BCS VAR		; =3 -> Variablen
	JMP DSTACK	; =5 -> String-Desc.-Stack

; *** Suche in Arrays: ARYTAB bis STREND

ARRAYS	STA PTR		; A/X von Variablendurchlauf
	STX PTR+1	; Start Array-Array-Bereich
	LDY #STAT_ARY
	STY STAT	; Status: Arrays
ISARREND
	LDA PTR
	LDX PTR+1
CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
        BNE NEXTARR	; erreicht?
	CMP STREND	; High-Byte gleich, nur kleiner
			; oder gleich möglich!
	BEQ NOSTRING	; Arrays fertig -> kein String
NEXTARR
			; immer C=0, wegen CPX/CMP
	STA STRDP	; Start eines Arrays
	STX STRDP+1
	LDY #0
	LDA (STRDP),Y	; Array-Name
	TAX		; Array-Typ merken
	INY
	LDA (STRDP),Y
	PHP		; Array-Typ Teil 2 merken
	INY
	LDA (STRDP),Y	; Offset nächstes Array
	ADC PTR		; C-Flag ist bereits 0 (wegen CMP/CPX oben)
	STA PTR		; Start des Folge-Arrays
	INY
	LDA (STRDP),Y
	ADC PTR+1
	STA PTR+1
	PLP		; Array-Typ holen
	BPL ISARREND	; kein String-Array
	TXA		; Array-Typ Teil 2 holen
	BMI ISARREND	; kein String-Array
	INY
	LDA (STRDP),Y	; Anzahl der Dimensionen
	ASL		; *2
	ADC #5		; Offset = Dimensionen*2+5
			; C=0 solange Dim.. <= 125
	BNE ADVDESC	; immer verzweigen
ARRAY			; Einstieg bei Fortsetzung
NEXTASTR
	CLC
	LDA #3		; String-Descriptor-Länge
ADVDESC	ADC STRDP	; zum nächsten String
	STA STRDP
	BCC +
	INC STRDP+1	; Überlauf High-Byte
+	CMP PTR		; Alle Array-Elemente durch?
	BNE IS0ASTR
	LDX STRDP+1
	CPX PTR+1
	BEQ CHKAEND	; A/X = PTR, Array-Bereichsende prüfen
IS0ASTR
	LDY #0
	LDA (STRDP),Y	; String-Länge
	BEQ NEXTASTR	; weiter im Array
RETGETSA
	STA LEN		; Rückgabevariable
	INY
	LDA (STRDP),Y	; String-Adresse low
	TAX
	INY
	LDA (STRDP),Y	; String-Adresse high
	TAY
	LDA LEN		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour
NOSTRING
	LDA #0		; Länge 0 
	STA LEN		; kein String gefunden
	RTS		; Z=1




!ifndef no_indicator {
ORIGVID !byte 0		; originales Zeichen an der Markenposition
ORIGCOL !byte 0		; originale Farbe an der Markenposition
}


