;
; *************************
; *  GARBAGE  COLLECTION  *
; *   von Johann Klasek   *
; *   j AT klasek DOT at  *
; * 1985-12-27 VERS. 1.1  *
; * 2013-11-24 VERS. 2.0  *
; * 2019-02-15 VERS. 2.1  *
; * 2020-12-12 VERS. 2.2  *
; *************************
;
; Räumt Stringspeicher auf, indem Lücken von unbenutzten String
; entfernt werden und ersetzt die C64 BASIC V2.0 Garbage Collection.
; Es werden nur jene Speicherstellen benutzt, die auch die normale
; GC verwendet; alle anderen werden wieder gesichert und restauriert.

; Start des Codes ...

!ifdef start {
	*=start
} else {
	*= $C500
}

; Optionen:

; Entweder als Patch des BASIC-ROMs in der RAM-Kopie von $A000 bis $BFFF
; oder GC via IRQ anspringen (auskommentieren):
;basic_patch = 1
;
;	Ad Platzverbrauch: 
;	 * Der normal Hook in BASIC-RAM-Kopie braucht 48 Bytes.
;	 * Der IRQ-Hook ist sehr aufwendig und braucht 244 Bytes, weil
;	   hier auch noch 60 Bytes für die Blockkopierroutine
;	   benötigt wird, da hier die ROM-Variante nicht genutzt
; 	   werden kann!

; Wenn aktiv, wird als Puffer das RAM unter dem BASIC-ROM verwendet,
; sonst jenes unter dem KERNAL. So kann etwa das RAM unter dem KERNAL
; für Grafikzwecke verwendet werden ...
;basic_rom_buffer = 1

; Original MOVBLOCK-Routine (Kopie aus dem ROM) verwenden (nur
; im Fall, wenn basic_patch nicht aktiviert ist!).
; Sonst wird ein optimierte Variante verwendet.
;orig_movblock = 1

; Bildschirmmarkierung nicht verwenden
;no_indicator = 1


; Optionsabhängigkeiten

; Wenn BASIC im RAM gepatcht wird, immer die originale MOVBLOCK-Routine
; verwenden!
!ifdef basic_patch {
orig_movblock = 1
}

; In der BASIC-Patch-Variante darf der Puffer nicht auch im RAM unter
; dem BASIC-ROM sein!
!ifdef basic_patch {
  !ifdef basic_rom_buffer {
    !error "Ungültige Option: basic_rom_buffer und basic_patch dürfen nicht gleichzeitig aktiv sein!"
  }
}


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

V_IRQ    = $0314	; IRQ-Vektor, 2 Bytes

; Variablen

STRPTR   = FRETOP	; String-Pointer = FRETOP
STRDP    = $22		; String-Descriptor-Address
			; $22 wird von originalem MOVBLOCK zerstört!
RNGBEG   = $4C		; Bereichsanfang
NEWPTR	 = $4E		; Neuer String-Pointer
PTR      = $50		; Array-Pointer
LEN      = $52		; String-Length
; $54-$56 belegt
STAT     = $57		; String-Status, Werte siehe
			; STAT_* weiter unten.
; $58-5B wird von MOVBLOCK zerstört!
STRADR   = $58		; String-Address (temp.)
			; (MOVBLOCK: Zielblockende+1 bzw. Zielblockanfang)
RNGEND   = $5D		; Bereichsende
BUFPTR   = $5F		; Buffer-Pointer
			; (MOVBLOCK: Quellblockanfang!)

CPTR     = $22		; Pointer für Install-Routine

; zu rettender Zeropage-Bereich
ZPSTART  = $4C		; 1. zu rettende
ZPEND    = $52		; letzte zu rettende Stelle
ZPLEN    = ZPEND-ZPSTART+1
			; Anzahl zu rettenden Bytes

; Konstanten

; für Variabe STAT (String-Status):
STAT_SDS = 5		; String-Descriptor-Stack
STAT_VAR = 3		; einfache Variablen
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

BASIC    = $A000	; BASIC-ROM
KERNAL   = $E000	; KERNAL-ROM
ROMSIZE  = $2000	; ROM-Länge, 8 KByte

			; Puffer:
!ifndef basic_rom_buffer {
BUF	 = KERNAL	; Puffer unter KERNAL-ROM
} else {
BUF	 = BASIC	; Puffer unter BASIC-ROM
}
BUFSIZE  = ROMSIZE	; Puffergröße

VIDPAGE  = $288		; Video RAM Page
VIDBASE  = $0400	; Video RAM
COLBASE  = $D800	; Color RAM

PROCPORT = $01		; Prozessorport


; Debugging

;!set debug=1


; Installer

INSTALL

	!byte $2C	; Opcode von BIT absolute, Argument
			; enthält die Signature, wirkt als NOP
	!text "GC"	; Signatur für den BASIC-Loader,
			; immer an gleicher Stelle für
			; alle Varianten!

!ifdef basic_patch {

	; BASIC-ROM/RAM-Patch-Einbindung

MOVBLOCK = $A3BF	; Block verschieben aus BASIC-ROM
			; zerstört $58/$59/$5A/$5B/$22

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
	LDA #<COLLECT	; "JMP COLLECT"
	STA GARBCOL+1	; patchen ...
	LDA #>COLLECT
	STA GARBCOL+2
	LDA #$4C	; JMP-Opcode
	STA GARBCOL
	RTS
} else {

	; IRQ-Einbindung

	SEI
	LDA V_IRQ	; bisherige IRQ-Routine aufheben
	LDX V_IRQ+1
	CMP #<(IRQ)
	BNE HOOK
	CPX #>(IRQ)
	BEQ INSTEXIT	; Vektor bereits installiert
HOOK
	STA ORIGIRQ
	STX ORIGIRQ+1
	LDA #<(IRQ)	; GC-Routine einhängen ...
	LDX #>(IRQ)
	STA V_IRQ
	STX V_IRQ+1
INSTEXIT
	CLI
	RTS

; In der IRQ-Routine wird festgestellt, ob die alte GC
; gerade aktiv ist, was dann der Fall ist, wenn
;   * am Stack der Caller-PC der Open-Space-for-Memory-Routine ($B62B-1) ist,
;   * am Stack einer der drei Caller-PC der Search-for-Next-String-Routine
;     ist,
;   * der PC im Bereich von $B526 bis $B63D liegt.
; Gewisse Zustände müssen dabei allerings korrigiert werden.
; Korrekturfälle:
;   1. Wenn der PC im Bereich von GC_CRIT_START bis GC_CRIT_END liegt,
;      ist der Descriptor inkonsistent.
;      $59 ist already incremented if GC_CRIT_59 is reached, otherwise
;      an additional
;         INC $59
;      is needed.
;      High-Byte setzen:
;         LDY $55
;         INY
;         INY
;         LDA $59
;         STA ($4E),y
;      Bei Punkt 3 bereits der gesamte Descriptor
;      korrigiert (nach dem Verschieben des Strings),
;      aber es bringt faktisch keine Ersparnis diese Routine
;      mitzuverwenden.
;   2. Wenn der PC im Bereich von GC_PHP_START bis GC_PHP_END liegt,
;      ist der Stack inkonsistent -> ein Byte vom Stack nehmen.
;   3. Wenn die Subroutine "Open Space in Memory" unterbrochen
;      wurde (an der Aufruferadresse erkannt), dann muss der
;      gerade kopierte String fertig übertragen werden, daher
;      kehrt das RTI wieder zur Routine zurück und erst mit
;      dem RTS wird über die manipulierte Adresse am Stack
;      erst zur Korrektur des Descriptors und dann zur neuen GC
;      gesprungen.
;   4. Wenn in Subroutine "Search for Next String" unterbrochen
;      wurde (an den drei möglichen Aufrufadressen erkannt),
;      befindet sich am Stack die Adresse des Aufrufers, wobei
;      am Stack die RTI-Rückkehradresse auf eine Korrektur-
;      routine gelenkt wird, die 2 Bytes vom Stack nimmt und
;      zur neuen GC verzweigt.
;
; Sonst kann, wenn der Unterbrechungs-PC im GC-Code-Bereich liegt,
; direkt zu neuen GC verzweigt werden.
;
; Egal wo die GC unterbrochen wird, Top of String-Heap ($33/$34)
; ist zwar bereits zerstört, aber das ist egal, weil
; die neue GC diesen ohnehin neu ermittelt.
; 

GC_START      = $B526	; PC in diesem Bereich -> GC aktiv
GC_END        = $B63C
GC_CRIT_START = $B632	; PC in diesem Bereich -> 
			; Descriptor muss korrigiert werden!
GC_CRIT_END   = $B638
GC_CRIT_59    = $B635	; PC ab dieser Adresse: $59 ist korrekt 
GC_PHP_START  = $B58A	; PC in diesem Bereich -> 
			; SR (von PHP) vom Stack nehmen!
GC_PHP_END    = $B598	; hier ist das PLP

CALLER_OPSP   = $B628+2	; PC-1 der Return-Adresse des JSR $A3BF
			; Aufrufs (Open Space for Memory) bei $B628.
CALLER_SNS1   = $B561+2 ; PC-1 Return-Adressen der Aufrufstellen
CALLER_SNS2   = $B548+2 ; der "Search for Next String"-Routine
CALLER_SNS3   = $B5B8+2

IRQ
	SEI

	; IRQ-Stackframe:
	; $104,X Status-Reg
	; $105,X Low-PC
	; $106,X High-PC
	; ev. aufrufene Routine, falls in einer Subroutine:
	; $107,X Low-Byte des (Aufruf-PC)-1
	; $108,X High-Byte des (Aufruf-PC)-1

	LDA $108,X	; Aufrufer PC high
	TAY
	LDA $107,X	; Aufrufer PC low
	; Aufruf-PC-1 in A/Y
			; sind wir in "Open Space"?
	CMP #<(CALLER_OPSP)
	BNE CHK_SNS
	CPY #>(CALLER_OPSP)
	BNE CHK_SNS
IN_OPSP
	; mit RTI zurück in die unterbrochene
	; Kopierroutine, damit der String
	; fertig übertragen wird, aber mit
	; dem RTS dort, geht es dann zu
	; Descriptor-Korrekturroutine
	; die dann endlich die neue CG aufrufen
	; darf.
	LDA #<(CORR_STR-1)
	STA $107,X	; wegen RTS minus 1
	LDA #>(CORR_STR-1)
	STA $108,X	; immer >0
	BNE CONT	; immer, mit RTI zurück

CORR_STR
	LDY $55		; Offset des Descriptors
	INY		; String-Länge
	LDA $58		; String-Adresse low
	STA ($4E),Y	; in den Descriptor
	INY
	INC $59		; String-Adresse high von MOVBLOCK
	LDA $59		; ist eine Page darunter, also korrigieren!
	STA ($4E),Y	; nun String-Adresse high setzen
	BNE START_COLLECT
			; immer, weil immer >0

	
CHK_SNS			; sind wir in "Search for Next String"?
	CPY #>(CALLER_SNS1)
			; High-Byte ist für alle drei Adressen gleich!
	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
	  !error "High-Byte von CALLER_SNS* sind verschieden. Sie müssen gleich sein!"
	}
	BNE CHK_PC	; nur die Low-Bytes vergleichen ...
	CMP #<(CALLER_SNS1)
	BEQ IN_SUB
	CMP #<(CALLER_SNS2)
	BEQ IN_SUB
	CMP #<(CALLER_SNS3)
	BNE CHK_PC

IN_SUB
	LDA #<(SKIPSUB) ; Unterbrechungs-PC umbiegen ...
	STA $105,X	; Low-Byte
	LDA #>(SKIPSUB)
	STA $106,X	; High-Byte
			; Mit dem RTI geht es zu SKIPSUB, wo
			; der Caller vom Stack genommen wird.

CONT	JMP (ORIGIRQ)	; weiter zum Original-Code (Aufrufkette).

CHK_PC
	LDA $106,X	; Unterbrechungs-PC
	TAY		; High-Byte
	LDA $105,X	; Low-Byte
	CPY #>(GC_START)
	BCC CONT	; vor der GC
	BNE +		; über GC-Anfang
	CMP #<(GC_START)
	BCC CONT	; unterhalb GC
+	CPY #>(GC_END+1)
	BCC ++		; in der GC!
	BNE CONT	; über der GC
	CMP #<(GC_END+1)
	BCS CONT	; über der GC
++
	; Die GC wurde unterbrochen!
	; In irgendwelchen Spezialbereichen, wo wir mehr machen müssen?

	; in PHP/PLP Sektion?
	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
	  !error "High-Byte von GC_PHP_START und GC_PHP_END sind nicht gleich!"
	}
	CPY #>(GC_PHP_START)
	BNE +		; nicht in richter Page, nächster Bereichstest
	CMP #<(GC_PHP_START)
	BCC +		; unterhalb, keine Stack-Korrektur
	CMP #<(GC_PHP_END+1)
	BCS +		; darüber, keine Stack-Korrektur
	; Stack-Korrektur:
	PLA		; SR vom Stack nehmen, setzt ev. Flags N,Z aber nicht C
	BCC TO_COLLECT	; C immer 0, mittels RTI zu COLLECT
+
	; in kritischer Sektion?
	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
	  !error "High-Byte von GC_CRIT_START und GC_CRIT_END sind nicht gleich!"
	}
	CPY #>(GC_CRIT_START)
	BNE +		; nicht in richtiger Page, dann normal weiter
	CMP #<(GC_CRIT_START)
	BCC +		; unterhalb, keine Korrektur
	CMP #<(GC_CRIT_END+1)
	BCS +		; darüber, keine Korrektur

	; Korrektur, High Byte der String-Adresse in Descriptor setzen,
	; weil bereits das Low-Byte gesetzt wurde. Die Adresse im Descriptor
	; wäre sonst inkonsistent! Aber aufpassen: Der Inhalt von $59
	; ist ab GC_CRIT_59 schon richtig, sonst muss er korrigiert, also
	; also um 1 erhöht werden!

	CMP #<(GC_CRIT_59)
	BCS ++		; $59 bereits erhöht,
	INC $59		; sonst korrigieren!
++	LDY $55		; Offset des Descriptors
	INY		; String-Länge
	INY		; String-Adresse-Low (ist schon gesetzt!)
	LDA $59		; String-Adresse-High von MOVBLOCK
	STA ($4E),Y	; nun String-Adresse-High setzen

	; Der obige Teil könnte theoretisch auch die Descriptorkorrektur
	; bei CORR_STR nutzen (die aber nicht im IRQ-Kontext läuft und
	; deswegen hier nicht direkt angesprungen werden kann). Statt
	; über RTI mit Fortsetzung bei START_COLLECT, müsste 
	; alternativ CORR_STR verwendet werden. Doch eine entsprechende
	; Umsetzung würde auch 7 Bytes erfordern, die gegenüber dem 
	; obigen Code nur 1 Byte spart. Der Klarheit wegen wird hier auf
	; diesen Variante verzichtet.
+
	; mittels RTI COLLECT aufrufen:
TO_COLLECT
	LDA #<(START_COLLECT)
			; Rückkehradresse für RTI am Stack umbiegen ...
	STA $0105,X	; Low-Byte
	LDA #>(START_COLLECT)
	STA $0106,X	; High-Byte
	BNE CONT	; IRQ fortsetzen, RTI startet dann die neue GC ...

SKIPSUB			; Open-Space- oder Search-for-Next-String-Routine
	PLA		; abgebrochen: kommt direkt von RTI, Aufruf-PC
	PLA		; (für RTS) in alter GC verwerfen und die neue GC
			; übernimmt, geht direkt zu COLLECT.
START_COLLECT
	LDA #3
	STA $53		; Step-Size für nächsten Descriptor auf
			; Ausgangswert setzen. (Wird von alter GC
			; nicht initialisiert!)
}

; *** Garbage Collector

COLLECT
!ifdef debug {
	JSR gra_on	; Grafik (Puffervisualisierung) aktivieren
}

	; Zeropage-Register
	LDX #ZPLEN	; Anzahl und Index
SAVLOOP	LDA ZPSTART-1,X	; Index geht von Anzahl bis 1
	STA SAVE-1,X	; retten
	DEX
	BNE SAVLOOP

!ifndef no_indicator {
			; X ist 0 wegen Code davor!
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

	LDA MEMEND	; String-Pointer
	LDX MEMEND+1	; und Bereichanfang 
	STA STRPTR	; auf Speicherende
	STX STRPTR+1	; setzen.
	STA RNGBEG
	STX RNGBEG+1

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
;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
;                                                           =FRETOP
;   SDS,VAR,ARY  |<-------------------- String-Heap -------------------->|
;
; Der Bereich RNGBEG bis RNGEND (gesuchte Strings) ist immer um 256 Bytes 
; kleiner als der Pufferbereich, da am Ende des Bereichs ein String beginnen
; könnte, der max. 254 Bytes das Bereichsende überragen könnte. Dieser 
; "Überhang" muss im Puffer Platz haben und dort reserviert sein!

NEXTBLOCK
	LDA STRPTR	; NEWPTR parallel mit
	STA NEWPTR	; BUFPTR mitziehen ...
	LDA STRPTR+1
	STA NEWPTR+1
	LDX RNGBEG	; Bereich war zuletzt
	LDA RNGBEG+1	; String-Heap-Ende?
	CPX STREND
	BNE +
	CMP STREND+1
	BEQ EXIT	; ja -> fertig
+
	STX RNGEND	; um Pufferlänge - 256
	STA RNGEND+1	; nach unten verlegen.
	!if <BUFSIZE > 0 {
	  !error "BUFSIZE ist nicht ein Vielfaches von 256 ($100)!"
	}
	SEC		
	SBC #(>BUFSIZE-1)
			; Bereichslänge in Pages,
			; kann um 254 Bytes überragt werden!
	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
	STA RNGBEG+1
	CPX STREND	; Ende des String-Heaps erreicht?
	SBC STREND+1
	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
LASTRANGE
	LDA STREND	; Bereichanfang =
	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
	STA RNGBEG	; 
	STX RNGBEG+1	; 
	BNE STRINRANGE	; immer, weil High-Byte >0


; *** Ende der Garbage Collection

EXIT
	; Zeropage-Register
	LDX #ZPLEN	; Anzahl und Index
RESLOOP	LDA SAVE-1,X	; Index geht von Anzahl bis 1
	STA ZPSTART-1,X	; restaurieren.
	DEX
	BNE RESLOOP

!ifndef no_indicator {
			; X ist 0 wegen Code davor!
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
!ifdef debug {
	JSR gra_off	; Grafik (Visualisierung des Puffers)
			; deaktivieren.
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
	SEC		; suche von Beginn an ...
	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
NEXTSTR	
	CLC		; Suche fortsetzen
NEXTSTR1
	JSR GETSA	; Nächste String-Adresse holen.
	BEQ COPYBACK	; keinen String mehr gefunden!
			; Adresse in X/Y

	TYA		; high Byte
	CPX RNGEND	; X/A >= RNGEND:
	SBC RNGEND+1	; oberhalb des Bereichs, dann
	BCS NEXTSTR	; nächster String!

	TYA		; high Byte
	CPX RNGBEG	; X/A < RNGBEG:
	SBC RNGBEG+1	; unterhalb des Bereichs, dann
	BCC NEXTSTR1	; nächster String!
			; Innerhalb des Bereichs:
	LDA BUFPTR	; C immer gesetzt!
	SBC LEN		; Pufferzeiger um
	STA BUFPTR	; String-Länge nach unten 
	BCS +
	DEC BUFPTR+1	; Überlauf High-Byte

+	STY STRADR+1	; String-Adresse abspeichern
	STX STRADR	; für Kopieraktion.

	LDY LEN		; String-Länge (immer > 0)
	BNE NBENTRY	; immer, mit Dekrement beginnen!
NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
NBENTRY	DEY		; schreibt ins RAM unters ROM!
	BNE NEXTBYT
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
	LDA BUFPTR+1	; Wenn Pointer am Ende, ist der Puffer ...
	CMP #>(BUF+BUFSIZE)
	BEQ NOCOPY	; leer, dann nächster Bereich.
+			; Weil NEXTBLOCK zu weit
} else {
			; Sonderfall: Pufferende bei $FFFF
	LDA BUFPTR	; Puffer leer,
	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
	BEQ NOCOPY	; War es letzter Bereich?
			; Weil NEXTBLOCK zu weit
}

!ifdef orig_movblock {
	LDA STRPTR	; Original MOVBLOCK braucht
	LDX STRPTR+1	; Zielblockende+1
} else {
	LDA NEWPTR	; Optimiertes MOVBLOCK braucht
	LDX NEWPTR+1	; Zielblockanfang	
}
	STA $58		; = STRADR
	STX $59		; je nach MOVBLOCK-Variante
			; Ende+1 oder Anfang des Zielblocks

!ifdef orig_movblock {
	LDA NEWPTR	; nur für originales MOVBLOCK,
	LDX NEWPTR+1	; sonst ist die Adresse in A/Y
}
	STA STRPTR	; vorerst neues FRETOP
	STX STRPTR+1

!if ((BUF+BUFSIZE) and $FFFF) != 0  {
	LDA #<(BUF+BUFSIZE)
	STA $5A		; Quellblockende+1
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
	LDA PROCPORT	; KERNAL-ROM wegblenden
	PHA		; damit das Puffer-RAM zugänglich
	LDA #MEMRAM	; wird. Auch BASIC-ROM ist damit weg!
			; Sowohl bei Puffer $F000 als auch $A000
			; werden beide ROMs ausgeblendet.
	STA PROCPORT

	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
			; bei IRQ-Anbindung eigene Kopie verwenden,
			; da das BASIC-ROM ausgeblendet ist!
			; Andernfalls ist ja die ROM-Kopie im RAM da.
			; Z=1
	PLA		; ursprünglicher Zustand
	STA PROCPORT	; KERNAL-ROM wieder aktivieren
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
	TYA		; immer ungleich 0, Z=0
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
	CMP #STAT_VAR	; String-Status?
	BCC ARRAY	; =1 -> Arrays
	BEQ VAR		; =3 -> Variablen
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
	TAY		; immer ungleich 0, Z=0
	RTS		; Adresse in X/Y retour

NOSTRING
	LDA #0		; Länge 0 
	STA LEN		; kein String gefunden
	RTS		; Z=1

;
; *** String-Adresse im Descriptor korrigieren
;
; ( NEWPTR, STRDP, STAT -> )
;
CORR	LDA STAT	; String-Status
	AND #%011	; nur 2 Bits, ergibt
	TAY		; Lage des Descriptors, welche ...
	LDA NEWPTR	;
	STA (STRDP),Y	; bei SDS- und
	INY		; Array-Elementen unterschiedlich ist.
	LDA NEWPTR+1
	STA (STRDP),Y
	RTS


; *** MOVBLOCK-Routinen (für IRQ-Hook-Methode)

!ifndef basic_patch {

  !ifndef orig_movblock {
	; Die optimierte Routine.
	;
	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
	; ist beim Ausblenden des KERNALs, auch immer BASIC
	; ausgeblendet und damit nicht verfügbar. Daher eine
	; extra Routine, die aber etwas kürzer und schneller ist.

	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
	; Für das Kopieren zu niedrigen Adressen ist es überlappungsrobust.
	; Eingabe: $5F/$60 Startadresse Quelle
	;	   $5A/$5B Endadresse+1 der Quelle
	;	   $58/$59 Startadresse Ziel
	; Zerstört: A, X, Y
	; Ausgabe: $58/$59 hat den Wert Zielendadresse+1-256
	;          X = 0
	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
	;	   Z-Flag = 1
MOVBLOCK
        SEC
        LDA $5A       ; Quelle Endadresse low
        SBC $5F       ; minus Quelle Startadresse low
        TAY           ; Länge low
        LDA $5B       ; Endadresse high
        SBC $60       ; minus Startadresse high
        TAX           ; Länge high, als DEC-DEC-Counter
        TYA           ; Länge low
        BEQ copy      ; wenn 0, dann geht es einfach ...
        CLC           ; Länge in A
        ADC $5F       ; Quellstartadresse um Low-Byte-Offset
        STA $5F       ; korrigieren: -(-Länge) -> +Länge
        BCS +         ; da eigentlich Subtraktion, Überlauf
        DEC $60       ; entspr. in Startadresse high korr.
+
        TYA           ; Länge low
        CLC
        ADC $58       ; Zielstartadresse um Low-Bye-Offset
        STA $58       ; korrigieren: -(-Länge) -> +Länge
        BCS +         ; da eigentlich Subtraktion, Überlauf
        DEC $59       ; entspr. in Startadresse high korr.
+
	INX           ; Page-Zähler (alle ganzen und einen teilweise)
	TYA           ; Länge low
	EOR #$FF      ; negieren (2er-Komplement):
	TAY           ; NOT(X)+1
	INY
copy    LDA ($5F),Y   ; Quelle auf
        STA ($58),Y   ; Ziel kopieren, aufsteigend
        INY
        BNE copy
        INC $60       ; Quelle high
        INC $59       ; Ziel high
        DEX           ; Blockzähler 
        BNE copy
        RTS
	; braucht 6 Bytes weniger als die Originalroutine.

  } else {

	; Die originale Routine aus dem ROM:
	;
	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
	; Da beim Ausblenden des KERNALs auch immer BASIC
	; ausgeblendet ist, kann die Routine nicht im ROM aufgerufen
	; werden!

	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
	; Für das Kopieren zu höheren Adressen ist es Überlappungsrobust.
	; Eingabe: $5F/$60 Startadresse Quelle
	;	   $5A/$5B Endadresse+1 der Quelle
	;	   $58/$59 Endadresse+1 des Ziels
	; Zerstört: $22, A, X, Y
	; Ausgabe: $58/$59 hat den Wert des 1. Bytes des Zielbereichs.
	;          X = 0
	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
	;	   Z-Flag = 1
MOVBLOCK
        SEC
        LDA $5A       ; Endadresse low
        SBC $5F       ; minus Startadresse low
        STA $22       ; Länge low
        TAY
        LDA $5B       ; Endadresse high
        SBC $60       ; minus Startadresse high
        TAX           ; Länge high
        INX           ; Länge als DEC-DEC-Counter
        TYA           ; Länge low
        BEQ +         ; wenn nicht 0, dann 
        LDA $5A       ; Endadresse um Low-Byte-Offset
        SEC           ; der Länge korrigieren
        SBC $22       ; Länge low
        STA $5A       ;
        BCS ++
        DEC $5B       ; Endadresse high korr.
        SEC
++      LDA $58       ; Zieladresse um Low-Bye-Offset
        SBC $22       ; der Länge korrigieren
        STA $58
        BCS +++	      ; High byte
        DEC $59       ; 
        BCC +++
-       LDA ($5A),Y   ; Quelle auf
        STA ($58),Y   ; Ziel kopieren von höhere
--		      ; auf niedrigere Adressen
+++     DEY
        BNE -
        LDA ($5A),Y   ; Quelle
        STA ($58),Y   ; Ziel
+       DEC $5B       ; Quelle high
        DEC $59       ; Ziel high
        DEX           ; Blockzähler 
        BNE --
        RTS
  }
}


!ifdef debug {
!source "debug.asm"
}

!ifndef no_indicator {
ORIGVID !byte 0		; originales Zeichen an der Markenposition
ORIGCOL !byte 0		; originale Farbe an der Markenposition
}
!ifndef basic_patch {
ORIGIRQ !byte 0,0	; originaler IRQ-Vektor
}
SAVE	!byte 0		; gesicherte ZP-Variablen
*=*+ZPLEN-1

