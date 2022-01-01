!to "garbcol.o",cbm	
;
;  **** Garbage Collection ****
;
; 64'er, Oct. 1988

;
; Überarbeitetet und korrigiert:
;	2013-11-15 Johann E. Klasek, johann at klasek at
;
; Bugfixes:
;	1) in backlinkarr:
;	   C-Flag ist beim Errechnen des Folge-Arrays
;	   definiert gelöscht. 
;          Sonst werden ev. nicht alle Elemente
;          aller Arrays mit einem korrekten
;          Backlink versehen und der
;          String-Heap wird durch die GC
;          korrumpiert!
;	2) in backlinkarr bei blanext:
;	   Muss zum Aufrufer immer mit Z=0
;	   rückkehren, und erkennt
;	   sonst immer nur das 1. Array!
;	   Damit liegen die anderen Strings
;	   dann im freien Bereich und
;	   werden nach und nach überschrieben!
;
; Optimierungen:
;	Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
;	Wegen des längeren Codes und und einem deswegen länger werdenden
;	Branch-Offset, wurde der Code ab cfinish zurückversetzt.
;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
;	noch an anderer Stelle eingespart werden und zwar durch
; 	Faktorisieren in die Routine incptr. Dies ergibt exakt
;	die Ersparnis von 2 Bytes. Die Optimierung wirkt sich
;	allerdings immer nur dann positiv aus, wenn ein String
;	mehr als 12 Zeichen aufweist, sonst aber bei einer Länge
;	weniger als 12 Zeichen *negativ* aus, weil der Aufruf 
;	des faktorisieren Codes per JSR/RTS immer 24 Takte kostet!
;	Es hängt also vom konkreten Anwendungsfall ab, ob
;	sie ein besseres oder schlechteres Laufzeitverhalten ergibt.
;	Hier ist wohl jene Variante ohne dieser speziellen "Optimierung"
;	im Allgemeinen die bessere Wahl.

!set use_fast_copy=1



; Basic-Zeiger und -konstanten

collected = $0f

sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
			; wächst nach oben, max. 3 Elemente
			; zu je 3 Bytes.
sdsptr   = $16		; Zeiger auf nächstes freie Element
			; des String-Descriptor-Stacks (SDS)

vartab   = $2d		; Basicprogrammende = Variablenanfang
arytab   = $2f		; Variablenende = Array-Bereichanfang
strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
fretop   = $33		; aktuelle String-Heap-Adresse
strptr	 = $35		; temporärer Stringzeiger
memsiz   = $37		; höchste RAM-Adresse für Basic, Start
			; des nach unten wachsenden String-Heaps
; Hilfsvariablen

ptr	 = $22		; Arbeitszeiger
newptr	 = $4e		; Neuer Stringzeiger
desclen	 = $53		; akt. Länge eines Stringdescriptors
aryptr	 = $58		; Array-Zeiger
descptr	 = $5f		; Descriptor-Zeiger

garcoll  = $b526

; Vorbelegung der Speicherplätze

romsize  = $2000	; ROM Länge 8K

prozport = $01		; Prozessorport
memrom = %00110111	; Basic+Kernal ROM
membas = %00110110	; Basic RAM+kernal ROM
memram = %00110101	; Basic+Kernal RAM


; Datenstrukturen
;
; String am Heap:
;
;   +--------------------------------------+
;   |       +--------------+               |
;   V       |              V               |
;   +---+---+---+          +-----------+---+---+
;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
;   +---+---+---+          +-----------+---+---+
;   ^    *******           ^            *******
;   |       String.adr.    |               Descriptor-Adr.
;   +-Descriptor-Adresse   +-Stringadresse
;
; Lücken am Heap:
;                      
;   +-------------+ +----------------+
;   V             | V                |
;    +-----------+---+---+---------+---+---+
;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
;    +-----------+---+---+---------+---+---+
;                  ^  ***            ^  ***
;                  |   Lückenmark.   |   Lückenmarkierung
;                  Backlink-Adresse  Backlink-Adresse



!source "loader.asm"

;
; Patch-Liste für "loader"
;

patchlist:

!wo part1_real,part1_real_end-part1_real,part1
!wo part2_real,part2_real_end-part2_real,part2
!wo part3_real,part3_real_end-part3_real,part3
!wo part4_real,part4_real_end-part4_real,part4
!wo 0  ; Endemarkierung


; ******************************* part 1 *************************************

part1_real:

!pseudopc $b4f4 {

part1:

;***** Speicher von String-Heap anfordern
;
;	in:	A		; Länge anforderung
;		fretop
;	mod:	collected	; "GC aufgerufen" Flag
;		strptr		; temp. Zeiger
;	out:	fretop		; Adresse auf String
;		X,Y		; Adresse auf String
;
; Der String wird im Backlink stets als ungebrauchte Lücke
; markiert! Dann muss die GC nur noch die Backlinks
; der aktiven Strings setzen und kann die ungebrauchten
; Strings überspringen.


basicerror = $b4d2		; Basic-Fehlermeldung

allocate:
	lsr collected		; Flag löschen
retry	pha			; Länge der Anforderung,
				; für 2. Teil
				; Länge 0 möglich, verbraucht aber 2 Bytes
	eor #$ff		; negieren
	sec
	adc fretop		; A/X = fretop; A/X -= Länge
	ldx fretop+1
	bcs l1
	dex
	sec
l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
	bcs l2
	dex
l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
	bcc checkcollect
	bne alloc		; nein, Bereich anfordern
	cmp strend 
	bcs alloc		; nein, Bereich anfordern
checkcollect
	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
	lda collected
	bmi basicerror		; Collection schon gelaufen?
	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
	ror collected		; Flag setzen (Bit 7) setzen
	pla			; Länge angeforderter Bereich
	jmp retry		; nochmal versuchen (ob durch GC Platz frei wurde)

alloc	jsr setfretop		; FRETOP = A/X
	jmp stralloc		; zum 2. Teil: Allokation abschließen


;***** garbage collection

;	in:	-
;	mod:	ptr		; Zeiger auf alten String-Heap
;		newptr		; Zeiger auf neuen String-Heap
;		descptr		; Zeiger auf Descriptor
;		desclen		; Descriptor-Schrittweite
;	out:	fretop		; Neue String-Heap-Position
;		C=1

docollect


; Backlink aller temporären Strings am String-Descriptor-Stack setzen

sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
	ldx #>sdsbase		; da in 0-Page, immer 0
	jsr setptr		; damit ptr setzen

sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
	beq vars		; Ja, SDS durch, weiter mit Variablen
	jsr backlink		; sonst Backlink setzen
	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descriptor

; Backlink aller String-Variablen setzen

vars:	lda #5			; Descriptor-Schritt für Variablen
	sta desclen
	lda vartab		; Variablenbeginn
	ldx vartab+1
	jsr setptr		; ptr = A/X

varnext	cpx arytab+1		; Variablenende?
	bne varbl
	cmp arytab
	beq arrays		; ja, weiter mit Arrays
varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
	bne varnext		; immer; nächsten Var.-Descriptor

; Backlink bei allen String-Arrays setzen

arrays:
	sta aryptr		; Variablenbereichende = Array-Bereichanfang
	stx aryptr+1 
	ldy #3			; Descriptor-Schritt bei String-Arrays
	sty desclen

arrnext	cpx strend+1		; Array-Bereichende?
	bne arrbl
	cmp strend
	beq cleanwalk
arrbl	jsr backlinkarr		; Backlinks für nächstes String-Array setzen -> Z=0!
	bne arrnext		; immer; nächstes Array-Element


; Ende, Zeiger zum neuen String-Heap übernehmen

cfinish
	lda newptr		; Aufgeräumtzeiger ist ..
	ldx newptr+1
setfretop
	sta fretop		; neues FRETOP
	stx fretop+1 
	rts			; fertig!

; Nachdem nun alle Backlinks gesetzt sind
; den String-Heap von oben nach unten durchgehen
; und zusammenschieben ...

cleanwalk:
	lda memsiz		; beim Basic-Speicherende
	ldx memsiz+1
	sta newptr		; ... beginnen
	stx newptr+1 

; Aufräumschleife

cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
	bne cwclean		; dann Heap durch und fertig.
	cmp fretop		; andernfalls aufräumen ...
	beq cfinish		; fertig, weil A/X = FRETOP

; nächsten String "aufräumen" ...

cwclean	sec			; Aufräumtzeiger auf backlink
	sbc #2
	bcs cw1
	dex			; A/X -> Backlink

cw1	jsr setptr		; A/X -> ptr (Arbeitszeiger)

	ldy #0
	lda (ptr),y		; Backlink low oder Lückenlänge
	iny			; Y=1
	tax			; -> X
	lda (ptr),y		; Backlink high
	cmp #$ff		; String "nicht gebraucht" Markierung
	bcc cwactive		; aktiver String

	txa			; Lückenlänge
	eor #$ff		; negieren
	adc ptr			; (ptr - Lückenlänge)
	ldx ptr+1 
	bcs cwnext		; weiter ...
	dex			; High Byte

cw2	bne cwnext		; immer (Heap ist nie in Page 1)

; einen aktiven String nach oben schieben

cwactive			; immer mit Y=1 angesprungen
	sta descptr+1		; Descriptor-Adresse
	stx descptr 

	lda newptr		; Aufgeräumtzeiger -= 2
	sbc #1			; weil bereits C=0!
	sta newptr		; newptr -= 2
	bcs cw3
	dec newptr+1
	sec			; für SBC unten

cw3	lda #$ff		; Backlink h: als Lücke markieren
	sta (newptr),y		; Y=1
	dey			; Y=0
	lda (descptr),y		; Descriptor: String-länge
	sta (newptr),y		; Backlink l: Lückenlänge

	lda newptr		; Aufgeräumtzeiger -= String-Länge
	sbc (descptr),y		; immer C=1
	sta newptr
	bcs cw4
	dec newptr+1
	sec			; für SBC unten

cw4	iny			; Y=1
	sta (descptr),y		; String-Adresse L: neue Adresse
	iny			; Y=2
	lda newptr+1
	sta (descptr),y		; String-Adresse H: neue Adresse
	ldy #0
	lda ptr
	sbc (descptr),y		; immer C=1
	sta ptr			; Arbeitszeiger = alte String-Adresse
	bcs cw5
	dec ptr+1
cw5
	lda (descptr),y		; String-Länge=0?
	beq cwnocopy		; ja, dann nicht kopieren
	tay			; Länge

!ifndef use_fast_copy {

cwloop	dey			; -> Startindex fürs Kopieren
	lda (ptr),y		; Arbeitszeiger mit altem String
	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
	tya			; Test auf Z-Flag!
	bne cwloop		; Index = 0 -> fertig kopiert

} else {

				; + 5 Byte, -2 T/Zeichen 
	bne cwentry		; immer, da Länge in Y>0
cwloop				; -> Startindex fürs Kopieren
	lda (ptr),y		; Arbeitszeiger mit altem String
	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
cwentry	dey			; Test auf Z-Flag!
	bne cwloop		; Index = 0 -> fertig kopiert
cwone	lda (ptr),y		; Arbeitszeiger mit altem String
	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort

}

cwnocopy
	lda ptr
	ldx ptr+1		; High-Byte immer !=0
	bne cwnext		; immer; weiter in Schleife


;**** Backlink setzen
;
; 	in:		ptr	Descriptor-Adresse
; 	out:		ptr	Descriptor-Adresse
;			A/X
;			Z=0	wenn nicht am SDS
;			Z=1	wenn am SDS
;	destroy:	newptr
;	called:		blaset, backlinkvar

backlink:
	ldy #0
	lda (ptr),y		; String-Länge
	beq blnext		; fertig, wenn =0
	iny
	clc
	adc (ptr),y		; Backlink-Position (am String-Ende)
	sta newptr		; Backlink-Zeiger L
	tax
	iny
	lda (ptr),y
	adc #0
	sta newptr+1		; Backlink-Zeiger H
	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
	bcc blnext		; ja, denn nächsten String
	bne blsetdesc
	cpx strend 
	bcc blnext		; < Array-Bereichende (außerhalb Heap)?

blsetdesc
	ldy #1
	lda ptr+1
	sta (newptr),y		; Descriptor-Adresse ...
	dey
	lda ptr
	sta (newptr),y		; in den Backlink übertragen

blnext	lda desclen		; nächster String/nächste Variable
	jsr incptr		; Schrittweite zum nächsten Descriptor
	ldx ptr+1		; immer != 0 -> Z=0 (außer bei SDS, Z=1)
	rts

;**** Nächste String-Variable und Backlink setzen
;
; 	in:		ptr	Variablenadresse
; 	out:		ptr	Variablenadresse
;			A/X
;			Z=0
;	destroy:	newptr
;	called:		varbl (vars)

backlinkvar:
	ldy #0
	lda (ptr),y		; Variablenname 1. Zeichen
	tax			; Typstatus merken
	iny
	lda (ptr),y		; Variablenname 2. Zeichen
	tay			; Typstatus merken

	lda #2			; Descriptor-Adresse (in Variable)
	jsr incptr		; errechnen

	txa			; Variablentyp prüfen
	bmi blnext		; keine String, nächste Variable
	tya
	bmi backlink		; Backlink setzen
	bpl blnext		; keine String-Var., nächste Variable

; **** ptr inkrementieren

incptr
	clc
	adc ptr			; ptr += A
	sta ptr
	bcc +
	inc ptr+1
+	rts

}
part1_real_end

; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!

!set part1_end = (part1_real_end-part1_real)+part1
!if ( part1_end > $B63D ) {
	!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
}


; ******************************* part 4 *************************************

part4_real
!pseudopc $b6c1 {

part4:

part4_continue = $b6d6
	jmp part4_continue

;**** Nächste Array-Variable und Backlink setzen
;
; 	in: 		ptr	Arrayadresse
; 	out:		ptr	Adresse Folge-array
;			aryptr	Adresse Folge-array
;			A/X	Adresse Folge-array
;			Z=0
;	destroy:	newptr
;	called:		arrbl (arrays)

backlinkarr:
	ldy #0
	lda (ptr),y		; Variablenname 1. Zeichen
	php			; für später
	iny
	lda (ptr),y		; Variablenname 2. Zeichen
	tax			; für später

	iny
	lda (ptr),y		; Offset nächstes Array
	clc			; Bugfix 1: C=0 definiert setzen
	adc aryptr
	jmp backlinkarr2
				; weiter an anderer Stelle!
blapast
!if blapast > part4_continue {
	!error "part4 ist zu lang!"
}
}
part4_real_end


; ******************************* part 3 *************************************

part3_real
!pseudopc $e474 {

part3:

	!byte  0 		; Einschaltmeldung kürzen

backlinkarr2:
	sta aryptr		; Folge-Array L
	iny
	lda (ptr),y
	adc aryptr+1 
	sta aryptr+1		; Folge-Array H

	plp			; Arraytyp:
	bmi blaskip		; kein String-Array
	txa
	bpl blaskip		; kein String-Array

	iny			; Y=4
	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
	asl 			; *2
	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
	adc ptr			; auf 1. Element ...
	sta ptr 
	bcc bla1
	inc ptr+1 
bla1	ldx ptr+1		; positionieren

blanext	cpx aryptr+1		; Array-Ende erreicht?
	bne blaset		; nein, Backlink setzen
	cmp aryptr
	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
blaset
	jsr backlink		; Backlink setzen
	bne blanext		; immer (High-Byte != 0)

blaskip
	lda aryptr		; Zeiger auf Folge-Array
blafinish
	ldx aryptr+1 		; Z=0 sicherstellen

setptr	sta ptr			; Arbeitszeiger setzen
	stx ptr+1
	rts			; immer Z=0

;--- $e4b7 - $e4d2 unused ($aa)
;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
;----              sonst Patch für andere Zwecke

}
part3_real_end


; ******************************* part 2 *************************************

part2_real
!pseudopc $e4ba {

part2:

;**** String Allocation (Fortsetzung)
;
;	in: 	TOS		; Länge
;		fretop		; String-Adresse
;	out:	fretop		; String-Adresse
;		strptr		; String-Adresse (wird nicht verwendet)
;		A		; Länge
;		X,Y		; String-Adresse
;	called:	allocate

stralloc:
	sta strptr		; strptr = A/X = FRETOP
	stx strptr+1
	tax			; A in X aufheben
	pla			; Länge temp. vom Stack 
	pha			; wieder auf Stack, nun auch in A
	tay			; Index=Länge (Backlink-position)
	sta (fretop),y		; Backlink L = String-/Lückenlänge
	iny			; Y=Länge+1
	bne sa1			; wenn Länge=255, dann
	inc fretop+1		; Überlauf, aber nur temporär!

sa1	lda #$ff		; Backlink H = Markierung "Lücke"
	sta (fretop),y
	ldy strptr+1
	sty fretop+1		; Überlaufkorr. rückgängig
	pla			; Länge vom Stack nehmen
	rts

}
part2_real_end


; Einsprungspunkt an korrekter Position?

; Kann erst nach dem Label docollect gemacht werden!

!if (garcoll != docollect) {
	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
}
