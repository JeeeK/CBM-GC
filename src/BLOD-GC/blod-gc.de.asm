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
DESC     = $58		; String-Adresse (temp.)
STR      = $5A		; Zeigt auf einen String
LEN      = $5D		; String-Länge (GETSA out)
PTR      = $5F		; Array-Zeiger (GETSA in/out)


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
	CPY $18		; Descriptor auf SDS-Top?
	BNE +
	CMP $17
	BNE +
	STA $16		; Ja, dann vom SDS entfernen
	SBC #3
	STA $17
	
	; Wenn die Zielvariable auf einen String am Heap zeigt, dann freigeben.

+	LDY #0
	; $49 zeigt auf den Variablen-Descriptor (in der LET-Zielvariable)
	LDA ($49),Y	; String-Länge
	BEQ LEAVE	; Variable enthält keinen String-Wert
	TAX		; > 0, für später speichern
	INY
	LDA ($49),Y	; Stringadresse Low-Byte
	STA STR
	INY
	LDA ($49),Y	; Stringadresse High-Byte
	STA STR+1

	; STR freigeben, wenn am Heap und zurück

FREE
	LDA STR+1	; Stringadress High-Byte
	CMP FRETOP+1	; Heap-Top High-Byte
	BCC LEAVE	; String unterhalb des Heaps
	BNE ++		; String könnte am Heap sein
	LDA STR		; Stringadresse Low-Byte
	CMP FRETOP	; Heap-Top Low-Byte
	BCC LEAVE	; Nicht am Heap!

	LDA STR+1	; Stringadresse über oder gleich FRETOP

++	CMP MEMEND+1	; String oberhalb Sring-Speicher?
	BCC +++		; Nein
	BNE LEAVE	; Ja
	LDA STR		; High-Byte gleich, Low-Byte vergleichen
	CMP MEMEND
	BCS LEAVE	; Oberhalb, also nicht am Heap
	
	; String am Heap: als frei markieren

+++	TXA		; Länge wiederherstellen
	CMP #1		; String-Länge 1?
	BNE ++

	LDY #0
	STA (STR),Y	; Einzelnes Byte am Heap enthält nur 1
	BEQ LEAVE	; Immer (Z=1)

++	TAY		; Länge nach Y (> 1!)
	DEY
	DEY		; Y: Länge - 2
	STA (STR),Y	; Vorletztes Byte des Strings enthält die Länge
	INY
	LDA #$FF
	STA (STR),Y	; Letztes Byte des Strings mit Lückenmarkierung
LEAVE	RTS



; String-Addition: Zweites Argument nach dem Kopieren!

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

; Entweder beide Argumente oder keines sind auf dem SDS. Wenn das zweite Argument
; (das später auf dem SDS gelangte) am SDS ist, dann ist das erste auch dort.

HANDLE2
	JSR $B68C	; Kopiere String zur Hilfszeigerposition
	LDA $50		; Descriptor-Adresse des zweiten Arguments
	LDY $51		; Kann nicht mehr am Heap sein, also als frei markieren,
	CMP $16		; wenn es das zuvor entfernte Element war
	BNE LEAVE
	CPY $18		; High-Byte (immer 0)
	BNE LEAVE
	JSR FREESDS	; Das bereits am SDS entfernte Element als frei markieren
	LDA $6F		; Descriptor-Adresse des ersten Arguments
	LDY $70
	JMP POPSDS	; Elemente entfernen und als frei markieren
	

; LEFT$(), RIGHT$(), MID$(): Eingabe-String freigeben

;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put string address
;                                and length on descriptor stack and update stack pointers
; -> 
;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer

HANDLE3
	; A: Länge, kopiere von ($22) nach ($35)
	JSR $B68C	; Kopiert den String-Teil in den angeforderten Neubereich
	LDA $50
	LDY $51

	; Der String selbst ist nicht Top-of-heap, nur als frei markieren und vom
	; SDS entfernen.

POPSDS
	CPY $18		; Descriptor zuoberst auf SDS?
	BNE LEAVE	; RTS
	CMP $17
	BNE LEAVE	; RTS
	; Speicher als frei markieren und vom SDS entfernen
	JSR FREESDS	; C=1 von CMP zuvor, im Aufruf nicht verändert
	LDA $17		; Zuoberstes Element on SDS
	JMP $B6E3	; Vom SDS entfernen (A Low-Byte des SDS-Elements)
			; Muss beim Eintritt C=1 haben
FREESDS
	; A/Y ist Zeiger auf String-Descriptor am SDS!
	TAX		; Index in der Zero-Page
	LDA 1,X		; String-Adresse Low-Byte
	STA STR
	LDA 2,X		; String-Adresse High-Byte
	STA STR+1
	LDA 0,X		; String-Länge
	TAX
	BNE FREE	; Länge in X, Adresse in STR/STR+1
	RTS		; Kein Freigeben, wenn Länge 0!



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



; Gehe über alle String und reorganisiere sie

STAGE1
        SEC             ; Suche von Beginn an
NEXTSTR
	JSR GETSA
	BEQ STAGE2      ; Kein String mehr gefunden
			; Adresse in X/Y, Descr. STRDP mit Offset von STAT

	CPY FRETOP+1	; String am Heap?
	BCC NEXTSTR	; Nein, C=0 für GETSA-Fortsetzung
	BNE +
	CPX FRETOP
	BCC NEXTSTR	; Nein, C=0 für GETSA-Fortsetzung

+	STX STR		; String-Start, der am Heap liegt
	STY STR+1
	LDA LEN
	CMP #1		; String-Länge 1?
	BNE ++

	; LEN 1: 
	;	Kopiert den String-Wert in den Descriptor.
	;	Überschreibt den String am Heap mit dem Wert 1.

	LDY #0
	LDA (STR),Y	; String-Inhalt/Wert
	TAX
	LDA #1		; Markierung für String mit Länge 1
	STA (STR),Y	; Markierung am Heap speichern
	LDA STAT
	LSR		; Rechtsschieben ergibt Offset, welcher sich auf
	TAY		; auf STRDP bezieht und zum Descriptor positioniert
	INY		; String-Position Adresse Low-Byte
	TXA		; String-Wert
	STA (STRDP),Y	; Wert in den Descriptor speichern (Adresse Low-Byte)
	LDA #0		; 0-Byte Markierung,
	INY		; für Strings, die am Heap liegen.
	STA (STRDP),Y	; String-Adresse High-Byte
	CLC		; Fortsetzungsmodus für GETSA
	BCC NEXTSTR	; Immer

	; LEN >1:
	;	Kopiere Back-link-Bytes in den Descriptor,
	;	speichere Descriptor-Zeiger als Back-link

++	TAY		; Länge
	DEY		; Index auf letztes Byte
	LDA (STR),Y
	PHA		; Letztes Byte des Strings
	LDX STRDP+1
	LDA STAT
	LSR		; Rechtsschieben ergbit Offset auf den Descriptor
	CLC
	ADC STRDP
	BCC +
	INX
+	PHA		; STRDP + Offset Low-Byte
	TXA		; X STRDP + Offset High-Byte
	STA (STR),Y	; Back-link High-Byte
	DEY
	LDA (STR),Y	; Vorletztes Byte im String
	TAX
	PLA		; STRDP + Offset Low-Byte
	STA (STR),Y	; Back-link Low-Byte
	LDA STAT
	LSR		; Rechtsschieben ergibt Offset, welcher sich auf
	TAY		; STRDP bezieht und auf den Descriptor positioniert
	INY		; Längen-Byte übergehen
	PLA		; Letztes Byte des Strings
	STA (STRDP),Y	; In den Descriptor Adressse Low-Byte speichern
	TXA		; Vorletztes Byte im String
	INY		; =2
	STA (STRDP),Y	; In den Descriptor Adresse High-Byte speichern
	CLC		; Forsetzungsmodus for GETSA
	BCC NEXTSTR	; Immer
	



; Gehe durch den Heap, entferne die Lücken und verschiebe die Strings

STAGE2
	LDY MEMEND+1	; Ende des String-Speichers
	LDX MEMEND	; ist neuer Heap-Anfang.
	STX NEWHEAP
	STY NEWHEAP+1
			; Einstiegspunkt vom Keine-Lücke-Teil
LOOP2R	STY PTR+1	; PTR is in X
	LDY #0
LOOP2
	TXA		; PTR minus 1
	BNE +
	DEC PTR+1
+	DEX
-	STX PTR

	CPX HEAP	; PTR unterhalb des Heaps?
	LDA PTR+1
	SBC HEAP+1
	BCS +		; PTR >= HEAP
	JMP EXIT2
+
	LDA (PTR),Y	; Hole Back-link High-Byte
	CMP #1		; 1-Byte-Lücke
	BEQ LOOP2	; überspringen, wird in Stage 3 behandelt

	INX		; PTR dekrementieren, aber A unberührt lassen
	DEX		; PTR Low-Byte = 0?
	BNE +
	DEC PTR+1
+	DEX		; PTR Low-Byte
	STX PTR

	CMP #$FF	; Lückenmarkierung? (Länge >1)
	BNE NOGAP
			; Lücke einer bestimmten Länge übergehen ...
	LDA (PTR),Y	; Lückenlänge
	EOR #$FF	; A is > 1
			; Carry-Flag von CMP oben gesetzt!
	ADC #1		; Zweierkomplement +1 und +1, -(LEN-1) + PTR -> PTR
			; wird nie 0 weil Lückenlänge > 1
	ADC PTR		; C=0 immer, weil -(LEN-1) nie $FF überschreiten kann
	TAX		; PTR Low-Byte
	BCS -		; Position letztes String-Byte
	DEC PTR+1	; PTR High-Byte, immer >0
	BNE -		; Immer, PTR hat String-Adresse, die auf
			; das letzte String-Byte zeigt

	; Wir haben einen Back-link auf den String:
NOGAP	STA DESC+1	; Back-link High-Byte und
	LDA (PTR),Y	; Back-link Low-Byte ist die
	STA DESC	; Descriptor-Adresse.

	LDA (DESC),Y	; Länge aus dem ~escriptor
	EOR #$FF
	PHA		; Später für den Heap gebraucht
	LDX PTR+1	; Transfer zu STR ...
			; Carry-Flag gelöscht vom CMP #$FF!
	ADC #3		; -(LEN-2) + PTR -> PTR
	BNE +		; PTR bereits auf Position
			; Spezialfall Länge = 2:
	INX		; Kompensiere das High-Byte-Dekrement
	CLC		; Addiere 0 mit Carry-Fläg gelöscht, PTR bleibt unverändert
+	ADC PTR		; Akku war vorher im Bereich von 0 bis FC
			; was nie das Carry setzen kann!
	BCS +
	DEX		; Im Fall des Addierens von 0 ist X bereits kompensiert
+	STX STR+1	; STR zeigt auf String-Start
	STA STR
	
	; Am Heap LEN Bytes reservieren
	PLA		; LEN, aber bereits komplementiert
	SEC		; Finalisiere das Zweierkomplement (+1 vom Carry-Flag)
	ADC NEWHEAP	; HEAP - LEN -> HEAP
	STA NEWHEAP
	BCS +
	DEC NEWHEAP+1
+	
	; Kopiere LEN Bytes von STR zu HEAP
	LDA (DESC),Y	; Länge aus dem Descriptor
	TAY		; als Index
	DEY		; index = length - 2
	DEY
	BEQ +		; 0, nichts zu kopieren
	DEY		; -1, Index auf das letzte Byte
	BEQ ++		; Keine Iteration, wenn Index 0 ist.
-	LDA (STR),Y	; Transferiere Byte 1 bis len-1
	STA (NEWHEAP),Y
	DEY
	BNE -
++	LDA (STR),Y	; Transfereriere Byte 0
	STA (NEWHEAP),Y
+	
	; Descriptor korrigieren
	LDY #2		; Offset im Descriptor
	LDA (DESC),Y	; Vorletztes String-Byte
	PHA		; Zwischenspeichern
	LDA NEWHEAP+1
	STA (DESC),Y	; String-Adresse Low-Byte setzen
	DEY
	LDA (DESC),Y	; Letztes String-Byte
	PHA		; Für später retten
	LDA NEWHEAP	; String-Adresse High-Byte wiederherstellen
	STA (DESC),Y	; Back-link High-Byte

	DEY		; Y=0
	; Wiederherstellung der String-Bytes im Back-link
	LDA (DESC),Y	; Längen-Byte
	TAY
	DEY		; Index auf letztes String-Byte
	PLA
	STA (NEWHEAP),Y	; Letztes Byte
	DEY
	PLA		
	STA (NEWHEAP),Y	; Vorletztes Byte

	LDX STR		; PTR Low-Byte nach X
	LDY STR+1	; Immer >0
	JMP LOOP2R	; Wiederholen mit Setzen von PTR und Setzen von Y=0
	
EXIT2
	LDA NEWHEAP	; Der neue, kompaktierte Heap
	STA HEAP	; nun als neuen Heap setzen
	LDA NEWHEAP+1
	STA HEAP+1




; Alle Strings des Heaps mit Länge wieder zurück auf den Heap
; kopieren. Diese haben eine spezielle Markierung im Descriptor.

STAGE3
        SEC             ; Suche mittels GETSA von Beginn an
        !byte $24       ; BIT ZP, nächste Instruktion übergehen
NEXT1STR
	CLC		; GETSA an letzter Position fortsetzen
	JSR GETSA
	BEQ EXIT        ; Alle Strings durch
			; Adresse in X/Y, Descr. STRDP mit Offset von STAT
	DEC LEN
	BNE NEXT1STR	; Nächster Durchlauf, wenn Länge nicht 1
	TYA		; High-Byte Stringadresse nicht 0 -> nicht am Heap
	BNE NEXT1STR
			; Y ist bereits 0
	TXA		; String-Adr. Low-Byte ist das String-Byte!
	LDX HEAP
	BNE +		; Heap-Zeiger - 1
	DEC HEAP+1
+	DEX		; Low-Byte verwendet später
	STX HEAP
	STA (HEAP),Y	; String-Byte wieder am Heap speichern

	LDA STAT
	LSR		; Rechtsschieben ergibt Offset, welcher sich auf
	TAY		; STRDP bezieht und auf den Descriptor positioniert
	INY		; Low-Byte der Adresse im Descriptor
	TXA		; Heap-Zeiger Low-Byte
	STA (STRDP),Y	; wieder in den Descriptor speichern
	INY
	LDA HEAP+1	; Heap-Zeiger High-Byte
	STA (STRDP),Y	; wieder in den Descriptor speichern
	BNE NEXT1STR	; Immer, weil High-Byte >0
	

; *** Garbage collection fertig

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


