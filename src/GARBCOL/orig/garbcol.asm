!to "garbcol.o",cbm	
;
;  **** garbage collection ****
;
; 64er, oct 1988

;
; ueberarbeitetet und korrigiert:
;	2013 11 10 johann e. klasek, johann at klasek at
;
; bugfixes:
;


; basic-zeiger und -konstanten

sdsbase  = $0019	; 1. element stringdescriptorstack
			; waechst nach oben, max. 3 elemente
			; zu je 3 bytes.
sdsptr   = $16		; zeiger auf naechstes freie element
			; des stringdescriptorstacks

vartab   = $2d		; basicprogrammende = variablenanfang
arytab   = $2f		; variablenende = arraybereichanfang
strend   = $31		; arraybereichende = unterste stringheap adresse 
fretop   = $33		; aktuelle stringheap adresse
memsiz   = $37		; hoechste ram-adresse fuer basic, start
			; des nach unten wachsenden stringheaps

garcoll  = $b526

; vorbelegung der speicherplaetze

romsize  = $2000	; rom laenge 8k

prozport = $01		; prozessorport
memrom = %00110111	; basic+kernal rom
membas = %00110110	; basic ram+kernal rom
memram = %00110101	; basic+kernal ram


!source "loader.asm"


;
; Patch-Liste
;

patchlist:

!wo part1_real,part1_real_end-part1_real,part1
!wo part2_real,part2_real_end-part2_real,part2
!wo part3_real,part3_real_end-part3_real,part3
!wo part4_real,part4_real_end-part4_real,part4
!wo 0  ; Endemarkierung



part1_real:

!pseudopc $b4f4 {

part1:

;***** speicher von stringheap anfordern
;
;	in:	a			; länge anforderung
;		$33/34			; fretop
;	mod:	$0f			; gc aufgerufen flag
;		$35/36			; temp. zeiger
;	out:	$33/34			; fretop

        lsr $0f		; set not collected state
        pha		; länge der anforderung,
			; für 2. teil
        eor #$ff		; negieren
        sec
        adc $33
        ldx $34
        bcs $b502
        dex
        sec
        sbc #$02		; platz für backlink einrechnen
        bcs $b507
        dex
        cpx $32		; stringheap voll (arraybereich ende)?
        bcc $b511
        bne $b520		; nein, bereich anfordern
        cmp $31
        bcs $b520		; nein, bereich anfordern
        ldx #$10
        lda $0f
        bmi $b4d2		; collection schon gelaufen?
        jsr $b526		; nein, dann garbage collection, c=1 (immer!)
        ror $0f		; mark collected state, bit7 gesetzt
        pla		; länge angeforderter bereich
        jmp $b4f6		; nochmal versuchen

        jsr $b577		; fretop = a/x
        jmp $e4ba		; allocate final

;***** garbage collection

; backlink aller stringdescriptorstack strings setzen

        lda #$19		; start adr. string descriptor stack
        ldx #$00
        jsr $e4a5		; set $22/$23

        cmp $16		; at 1. free sds element?
        beq $b536		; sds done
        jsr $b5e8		; backlink setzen
        beq $b52d		; unbedingt

; backlink aller stringvariablen setzen

        lda #$05		; descriptor-schritt bei variablen
        sta $53
        lda $2d		; variablenbeginn
        ldx $2e
        jsr $e4a5		; 22/23 = a/x

        cpx $30		; variablenende?
        bne $b549
        cmp $2f
        beq $b54e		; ja, weiter mit arrays
        jsr $b61f		; backlink für nächste stringvariable setzen
        bne $b541		; unbedingt

; backlink aller stringarrays setzen

        sta $58		; variablenbereichende = arraybereichanfang
        stx $59
        ldy #$03		; descriptor-schritt bei stringarrays
        sty $53

        cpx $32		; arraybereichende?
        bne $b55e
        cmp $31
        beq $b563
        jsr $b6c4		; backlinks für nächstes stringarray setzen
        bne $b556		; unbedingt.

        lda $37		; memtop
        ldx $38
        sta $4e		; -> aufgeräumtzeiger
        stx $4f

; aufräumschleife

        cpx $34		; a/x: altes fretop erreicht,
        bne $b57c		; dann heap durch und fertig.
        cmp $33		; sonst aufräumen ...
        bne $b57c
        lda $4e		; aufgeräumtzeiger ist
        ldx $4f
        sta $33		; neues fretop
        stx $34
        rts		; fertig!

; nächsten string "aufräumen" ...

        sec		; aufräumtzeiger auf backlink
        sbc #$02
        bcs $b582
        dex		; a/x -> backlink
        jsr $e4a5		; a/x -> 22/23 (arbeitszeiger)
        ldy #$00
        lda ($22),y	; backlink low oder lückenlänge
        iny
        tax		; -> x
        lda ($22),y	; backlink high
        cmp #$ff		; string "nicht gebraucht" markierung
        bcc $b59e		; aktiver string
        txa		; lückenlänge
        eor #$ff		; negieren
        adc $22		; 22/23 - lückenlänge
        ldx $23
        bcs $b59b		; korr: geht auch gleich nach $b56b
        dex		; 
        jmp $b56b		; korr: bne $b56b (sollte nie 0 werden können!)

; aktiven string nach oben schieben

        sta $60		; descriptor-adresse
        stx $5f
        lda $4e		; aufgeräumtzeiger -= 2
        sbc #$01		; weil c=0!
        sta $4e
        bcs $b5ad
        dec $4f
        sec		; y=1

        lda #$ff		; backlink h: als lücke markieren
        sta ($4e),y	; 
        dey		; y=0
        lda ($5f),y	; descriptor: stringlänge
        sta ($4e),y	; backlink l: lückenlänge

        lda $4e		; aufgeräumtzeiger -= stringlänge
        sbc ($5f),y	; immer c=1
        sta $4e
        bcs $b5c1
        dec $4f
        sec

        iny		; y=1
        sta ($5f),y	; stringadresse l: neue adresse
        iny		; y=2
        lda $4f
        sta ($5f),y	; stringadresse h: neue adresse
        ldy #$00
        lda $22
        sbc ($5f),y	; immer c=1
        sta $22		; arbeitszeiger = alte stringadresse
        bcs $b5d5
        dec $23

        lda ($5f),y	; stringlänge=0?
        beq $b5e2		; ja, dann nicht kopieren
        tay		; länge-1

        dey		; -> startindex fürs kopieren
        lda ($22),y	; arbeitszeiger mit altem string
        sta ($4e),y	; aufgeräumtzeiger mit neuem stringort
        tya		; z-flag!
        bne $b5da		; index 0 -> fertig kopiert

        lda $22
        ldx $23
        bne $b56b		; unbedingt, weiter in schleife


; backlink setzen
;
; 	in:	22/23	descriptoradresse
; 	out:	22/23	descriptoradresse
;		a/x
;	destroy: 4e/4f
;	called:	$b531, $b637

        ldy #$00
        lda ($22),y	; stringlänge
        beq $b611		; fertig, wenn 0
        iny
        clc
        adc ($22),y	; backlinkposition (am stringende)
        sta $4e		; backlink-zeiger l
        tax
        iny
        lda ($22),y
        adc #$00
        sta $4f		; backlink-zeiger h
        cmp $32		; < arraybereichende (außerhalb heap)?
        bcc $b611		; ja, denn nächsten string
        bne $b606
        cpx $31
        bcc $b611		; < arraybereichende (außerhalb heap)?

        ldy #$01
        lda $23
        sta ($4e),y	; descriptoradresse ...
        dey
        lda $22
        sta ($4e),y	; in den backlink übertragen

        clc		; nächster string/nächste variable
        lda $53		; schrittweite zum nächsten
        adc $22		; descriptor ...
        sta $22
        bcc $b61c
        inc $23
        ldx $23
        rts

; nächste stringvariable und backlink setzen
;
; 	in: 22/23	variablenadresse
; 	out:	22/23	variablenadresse
;		a/x
;	destroy: 4e/4f
;	called: $b549

        ldy #$00
        lda ($22),y	; variablenname 1. zeichen
        tax
        iny
        lda ($22),y	; variablenname 2. zeichen
        tay
        clc
        lda $22		; descriptoradresse (in variable)
        adc #$02
        sta $22
        bcc $b633
        inc $23
        txa		; variablen typ prüfen
        bmi $b611		; keine string, nächste variable
        tya
        bmi $b5e8		; backlink setzen
        bpl $b611		; keine stringvar., nächste variable

}
part1_real_end

part4_real
!pseudopc $b6c1 {

part4:

        jmp $b6d6

; nächste array variable und backlink setzen
;
; 	in: 22/23	arrayadresse
; 	out:	22/23	adresse folge-array
;		58/59	adresse folge-array
;		a/x	adresse folge-array
;	destroy: 4e/4f
;	called: $b55e

        ldy #$00
        lda ($22),y	; variablenname 1. zeichen
        php		; für später
        iny
        lda ($22),y	; variablenname 2. zeichen
        tax		; für später
        iny
        lda ($22),y	; offset nächstes array
					; bug: clc fehlt!
        adc $58
        jmp $e475
}
part4_real_end


part3_real
!pseudopc $e474 {

part3:

        brk		; einschaltmeldung kürzen

        sta $58		; folge-array l
        iny
        lda ($22),y
        adc $59
        sta $59		; folge-array h
        plp		; arraytyp:
        bmi $e4a1		; kein stringarray
        txa
        bpl $e4a1		; kein stringarray
        iny		; y=4
        lda ($22),y	; anzahl der dimensionen
        asl 		; *2
        adc #$05		; + 5 (var.name+offset+dimensionen)
        adc $22		; auf 1. element ...
        sta $22
        bcc $e492
        inc $23
        ldx $23		; positionieren

        cpx $59		; arrayende erreicht?
        bne $e49c		; nein, backlink setzen
        cmp $58
        beq $e4a5		; array durch

        jsr $b5e8		; backlink setzen
        bne $e494		; unbedingt

        lda $58		; arrayzeiger
        ldx $59
        sta $22		; arbeitszeiger
        stx $23
        rts

;--- $e4b7 - $e4d2 unused ($aa)
;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
;----              patch for rs232-routines

}
part3_real_end


part2_real
!pseudopc $e4ba {

part2:

;**** string allocation (fortsetzung)
;
;	in: 	tos			; länge
;		33/34			; stringadresse
;	out:	a			; länge
;		35/36			; stringadresse
;	called:	$b523

        sta $35		; 35/36 = a/x = 33/34
        stx $36
        tax		; a in x aufheben
        pla		; länge
        pha		; wieder auf stack
        tay		; index=länge (backlink-position)
        sta ($33),y	; backlink l = string-/lückenlänge
        iny		; y=len+1
        bne $e4c9		; wenn länge=255, dann
        inc $34		; Überlauf, aber nur temporär!

        lda #$ff		; backlink h = markierung "lücke"
        sta ($33),y
        ldy $36
        sty $34		; Überlaufkorr. rückgänig
        pla		; länge vom stack
        rts

}
part2_real_end

