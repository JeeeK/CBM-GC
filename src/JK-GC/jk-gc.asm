*= $C500
!to "jk-gc.o", cbm	; set output file and format
;
;.OPT LIST,NOSYM
;
; *************************
; *  GARBAGE  COLLECTION  *
; *   von Johann Klasek   *
; * 1985-12-27 VERS. 1.1  *
; * 2013-11-24 VERS. 2.0  *
; * 2019-02-15 VERS. 2.1  *
; *************************
;
; Aufruf: SYS ...
; Räumt Stringspeicher auf
; Es werden nur jene Speicherstellen
; benutzt, die auch die normale
; GC verwendet; alle anderen
; werden wieder restauriert.

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

STRPTR   = FRETOP	; String-Pointer = FRETOP
STRDP    = $22		; String-Descriptor-Address
BERANF   = $4C		; Bereichsanfang
NEWPTR	 = $4E		; Neuer String-Pointer
PTR      = $50		; Array-Pointer
LEN      = $52		; String-Length
; $54-$56 belegt
STAT     = $57		; String-State
; $58-5B wird von MOVBLOCK zerstört!
STRADR   = $58		; String-Address (temp.)
BEREND   = $5D		; Bereichsende
BUFPTR   = $5F		; Buffer-Pointer
			; (MOVBLOCK: Quellblockanfang!)

CPTR     = $22		; Pointer für Install-Routine

; zu rettender Zeropage-Bereich
ZPSTART  = $4C		; 1. zu rettende
ZPEND    = $52		; letzte zu rettende Stelle
ZPLEN    = ZPEND-ZPSTART+1
			; Anzahl zu rettenden Bytes

; Konstanten

; für Variabe STAT (String State):
STAT_SDS = 5		; String-Descriptor-Stack
STAT_VAR = 3		; einfache Variablen
STAT_ARY = 1		; Array


; Speicherorte

GARBCOL  = $B526	; Einsprungpunkt der GC

MOVBLOCK = $A3BF	; Block verschieben
			; zerstört $58/$59/$5A/$5B/$22

BASIC    = $A000        ; BASIC-ROM
KERNAL   = $E000        ; KERNAL-ROM
ROMSIZE  = $2000        ; ROM-Länge 8 KByte8
BUF	 = KERNAL	; Puffer unter KERNAL
BUFSIZE  = ROMSIZE	; Puffergröße

; I/O-BEREICHE/-ADRESSEN

VIDBASE  = $0400	; Video RAM
COLBASE  = $D800	; Color RAM

MARKE    = "*"		; Aktivitätsanzeige
MARKEFARBE = 9		; rot
MARKEOFF = 40*25-1	; Markenposition
MARKEVID = VIDBASE+MARKEOFF
MARKECOL = COLBASE+MARKEOFF

PROZPORT = $01		; Prozessorport
MEMROM   = %00110111	; Basic+Kernal ROM, $37
MEMBAS   = %00110110	; Basic RAM+Kernal ROM, $34
MEMRAM   = %00110101	; Basic+Kernal RAM, $35


; Debugging

;!set debug=1


; Installer

INSTALL
	; BASIC ins RAM kopieren, um die GC-Routine
	; zu patchen ...
	LDA #MEMROM
	STA PROZPORT	; alles ROM (also vom ROM kopieren)
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
	LDA PROZPORT	; auf RAM umschalten
	AND #%11111110	; "BASIC-ROM aus"-Maske
	STA PROZPORT
	LDA #<COLLECT	; "JMP COLLECT"
	STA GARBCOL+1	; patchen ...
	LDA #>COLLECT
	STA GARBCOL+2
	LDA #$4C	; JMP-Opcode
	STA GARBCOL
	RTS

!ifdef debug {
!source "debug.asm"
}

; *** Garbage Collector

COLLECT
!ifdef debug {
	JSR gra_on
}
	LDA MARKEVID	; Kontrollanzeige Bildschirm
	STA ORIGVID
	LDA #MARKE
	STA MARKEVID	; Marke: Zeichen
	LDA MARKECOL	; sichern
	STA ORIGCOL
	LDA #MARKEFARBE
	STA MARKECOL	; Marke: Farbe sichern

	LDX #ZPLEN	; Zeropage-Reg.
SAVLOOP	LDA ZPSTART-1,X	; retten
	STA SAVE-1,X
	DEX
	BNE SAVLOOP

	LDA MEMEND	; String-Pointer
	LDX MEMEND+1	; und Bereichanfang 
	STA STRPTR	; auf Speicherende
	STX STRPTR+1	; setzen.
	STA BERANF
	STX BERANF+1

; *** Nächster zu betrachtender Bereich am String-Heap

;                        STRADR
;       +-------------------------------------+
;       |                                     |
;       |                                     V
;   +-+-+-+      +-----------------------+----------+------+------------+
;   |L|PTR|      |      noch nicht       | gesuchte | frei | behandelte |
;   | |   |      |  behandelte Strings   | Strings  |      |   Strings  |
;   +-+-+-+      +-----------------------+----------+------+------------+
;    ^            ^                       ^          ^      ^            ^
;    |            |                       |          |      |            |
;    STRDP        STREND                  BERANF     BEREND STRPTR       MEMSIZ
;                                                           =FRETOP
;   SDS,VAR,ARY  |<-------------------- String-Heap -------------------->|
;
; Der Bereich BERANF bis BEREND (gesuchte Strings) ist immer um 256 Bytes 
; kleiner als der Pufferbereich, da am Ende des Bereichs ein String beginnen
; könnte, der max. 254 Bytes das Bereichsende überragen könnte. Dieser 
; "Überhang" muss im Puffer Platz haben und dort reserviert sein!

NEXTBLOCK
	LDA STRPTR	; NEWPTR parallel mit
	STA NEWPTR	; BUFPTR mitziehen ...
	LDA STRPTR+1
	STA NEWPTR+1
	LDX BERANF	; Bereich war zuletzt
	LDA BERANF+1	; String-Heap-Ende?
	CPX STREND
	BNE +
	CMP STREND+1
	BEQ EXIT	; ja -> fertig

+	STX BEREND	; um Pufferlänge - 256
	STA BEREND+1	; nach unten verlegen.
	SEC		
	SBC #(>BUFSIZE-1) ; Bereichslänge in Pages,
			; kann um 254 Bytes überragt werden!
	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
	STA BERANF+1
	CPX STREND	; Ende des String-Heaps erreicht?
	SBC STREND+1
	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
LASTRANGE
	LDA STREND	; Bereichanfang =
	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
	STA BERANF	; 
	STX BERANF+1	; 
	BNE STRINRANGE	; immer, weil High-Byte >0


; *** Ende der Garbage Collection

EXIT
	LDX #ZPLEN
RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
	STA ZPSTART-1,X	; restaurieren.
	DEX
	BNE RESLOOP

	LDA ORIGVID	; Kontrollanzeige löschen
	STA MARKEVID	; und alten Zustand wieder
	LDA ORIGCOL	; herstellen.
	STA MARKECOL
!ifdef debug {
	JSR gra_off
}
	RTS


; *** Bereich durchgehen

STRINRANGE
!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA #>(BUF+BUFSIZE)
	STA BUFPTR+1
	LDA #<(BUF+BUFSIZE)
	STA BUFPTR
} else {
			; Sonderfall Pufferende bei $FFFF
	LDA #0		; Buffer-Pointer auf
	STA BUFPTR	; $10000 (65536) = 0
	STA BUFPTR+1	; setzen.
}
	SEC
	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
NEXTSTR	
	CLC
NEXTSTR1
	JSR GETSA	; Nächste String-Adresse holen.
	BEQ COPYBACK	; keinen String mehr gefunden!

	TYA		; high Byte
	CPX BEREND	; X/A >= BEREND:
	SBC BEREND+1	; oberhalb des Bereichs, dann
	BCS NEXTSTR	; nächster String!

	TYA		; high Byte
	CPX BERANF	; X/A < BERANF:
	SBC BERANF+1	; unterhalb des Bereichs, dann
	BCC NEXTSTR1	; nächster String!
			; Innerhalb des Bereichs:
	LDA BUFPTR	; Pufferzeiger um
	SBC LEN		; String-Länge nach unten
	STA BUFPTR	; setzen.
	BCS +
	DEC BUFPTR+1	; Überlauf High-Byte

+	STY STRADR+1	; String-Adresse abspeichern
	STX STRADR	; für Kopieraktion.

	LDY LEN		; String-Länge (> 0)
	BNE NBENTRY	; immer, mit Dekrement beginnen!
NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
NBENTRY	DEY		; schreibt ins RAM unters ROM!
	BNE NEXTBYT
LEN1
	LDA (STRADR),Y	; Das 0. Byte extra
	STA (BUFPTR),Y	; übertragen

	SEC		; Neue String-Adresse:
	LDA NEWPTR	; Einfach den Pointer
	SBC LEN		; mitziehen, ebenso um
	STA NEWPTR	; String-Länge nach unten
	BCS +		; setzen.
	DEC NEWPTR+1	; Überlauf High-Byte
+
	JSR CORR	; String-Adresse in Descriptor ändern.
			; Immmer Z=0,
	BNE NEXTSTR	; zum nächsten String.


; *** Pufferinhalt wieder zurück auf String-Heap

; 0 ------------------------------------------- FFFF	
;        Ziel                        Quelle
;          +--------------------------+
;          |                          |
;          V                         /^\
;     |||||||||||                |||||||||||
;     ^          ^               ^          ^ 
;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)

COPYBACK
!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA BUFPTR	; Puffer leer ...
	CMP #<(BUF+BUFSIZE)
	BNE +
	LDA BUFPTR+1	; Wenn Pointer am Ende ...
	CMP #>(BUF+BUFSIZE)
	BEQ NOCOPY	; ist der Puffer leer, ev. nächster
+			; Bereich ...
} else {
			; Sonderfall: Pufferende bei $FFFF
	LDA BUFPTR	; Puffer leer,
	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
	BEQ NOCOPY	; War es letzter Bereich?
}

	LDA STRPTR
	STA $58		; Zielblockende+1
	LDA STRPTR+1
	STA $59
	LDA NEWPTR
	STA STRPTR	; neues FRETOP
	LDA NEWPTR+1	
	STA STRPTR+1

!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA #<(BUF+BUFSIZE)
	STA $5A
	LDA #>(BUF+BUFSIZE)
	STA $5B
} else {
			; Sonderfall Pufferende bei $FFFF
	LDA #$00	; Quellblockende+1
	STA $5A
	STA $5B
}
			; Quellbockanfang = BUFPTR

	SEI		; keine Interrupts zulassen, wegen
	LDA PROZPORT	; KERNAL-ROM wegblenden
	PHA		; damit das Puffer-RAM zugänglich
	LDA #MEMRAM	; wird!
	STA PROZPORT

	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
			; Z=1
	PLA		; ursprünglicher Zustand
	STA PROZPORT	; KERNAL-ROM wieder aktivieren
	CLI
NOCOPY
	JMP NEXTBLOCK	; nächsten Bereich


;
; *** Get String - nächsten String mit Länge ungleich 0
;
; ( C-Flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-Flag )
;
; Bei C=1 wird beim SDS gestartet, sonst von der letzten
; Position gemäß STRDP und String-Status STAT.
; Das Z-Flag ist gesetzt, wenn kein String mehr
; vorhanden ist, sonst in X/Y die Adresse und in LEN
; die Stringlänge.

GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
			; sonst Start bei SDS ...

; *** String-Descriptor-Stack (SDS): TOSS bis TSSP
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
	STY STRDP+1	; Descriptor auf
	LDA #STAT_SDS	; Status: SDS
	STA STAT
	LDX #TOSS	; SDS Start
	BNE ISDSTEND	; immer verzweigen
DSTACK	LDX STRDP
NEXTDST	INX		; nächster Descriptor
	INX
	INX
ISDSTEND
	CPX TSSP	; Stack durch?
	BEQ VARS
	LDA 0,X		; String-Länge
	BEQ NEXTDST
	STA LEN		; Rückgabevariable
	STX STRDP	; festhalten
	LDA 2,X		; String-Adr. high
	TAY
	LDA 1,X		; String-Adr. low
	TAX
	TYA		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour

; *** Variablen: VARTAB bis ARYTAB

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
	BEQ NEXTVAR	; = 0, Nächste Variable
	STA LEN		; Rückgabevariable
	INY
	LDA (STRDP),Y	; String-Adresse low
	TAX
	INY
	LDA (STRDP),Y	; String-Adresse high
	TAY		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour

CHECKTYPE
	LDA STAT	; GETSA-Einstieg mit C=0
	CMP #STAT_VAR	; String-Status?
	BCC ARRAY	; =1 -> Arrays
	BEQ VAR		; =3 -> Variablen
	JMP DSTACK	; =5 -> String-Desc.-Stack

; *** Arrays: ARYTAB bis STREND

ARRAYS	STA PTR		; A/X von Variablendurchlauf
	STX PTR+1	; Start Array-Array-Bereich
	LDY #STAT_ARY
	STY STAT	; Status: Arrays
ISARREND
	LDA PTR
	LDX PTR+1
CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
        BNE NEXTARR	; erreicht?
	CMP STREND
	BEQ NOSTRING	; Arrays fertig -> kein String
NEXTARR
	STA STRDP	; immer C=0
	STX STRDP+1
	LDY #0
	LDA (STRDP),Y	; Array-Name
	TAX		; Array-Typ merken
	INY
	LDA (STRDP),Y
	PHP		; Array-Typ merken
	INY
	LDA (STRDP),Y	; Offset nächstes Array
	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
	STA PTR		; Start Folge-Array
	INY
	LDA (STRDP),Y
	ADC PTR+1
	STA PTR+1
	PLP		; Var.-Typ holen
	BPL ISARREND	; kein String-Array
	TXA		; Var.-Typ holen
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
ADVDESC	ADC STRDP	; nächten String
	STA STRDP
	BCC +
	INC STRDP+1	; Überlauf High-Byte
+	CMP PTR		; Array durch?
	BNE IS0ASTR
	LDX STRDP+1
	CPX PTR+1
	BEQ CHKAEND	; A/X = PTR, Array-Ende prüfen
IS0ASTR
	LDY #0
	LDA (STRDP),Y	; String-Länge
	BEQ NEXTASTR	; weiter im Array
	STA LEN		; Rückgabevariable
	INY
	LDA (STRDP),Y	; String-Adresse low
	TAX
	INY
	LDA (STRDP),Y	; String-Adresse high
	TAY		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour

NOSTRING
	LDA #0		; Länge 0 
	STA LEN		; kein String gefunden
	RTS		; Z=1

;
; CORR - String-Adresse im Descriptor korrigieren
;
; ( STRADR, STAT -> )
;
CORR	LDA STAT	; String-Status
	AND #%011	; nur 2 Bits
	TAY		; Lage des Descriptors
	LDA NEWPTR	;
	STA (STRDP),Y	; ... bei SDS
	INY		; ... und Array verschieden!
	LDA NEWPTR+1
	STA (STRDP),Y
	RTS

ORIGVID !byte 0		; original Video der Markenposition
ORIGCOL !byte 0		; original Farbe der Markenposition
SAVE	!byte 0		; gesicherte ZP-Variablen
*=*+ZPLEN-1


