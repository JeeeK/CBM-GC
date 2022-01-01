;
; *********** Loader
;
;       2013 11 10 johann e. klasek, johann at klasek at
;

; --- Temporäre Variablen:

ptr		= $22		; Zeropage, frei
ptr_l		= ptr
ptr_h		= ptr+1

len		= $4f		; Zeropage, temp. frei
len_l		= len
len_h		= len+1

dest		= $51		; Zeropage, temp. frei
dest_l		= dest
dest_h		= dest+1


; --- Konstanten:

basicrom	= $a000		; Startadresse
kernalrom	= $e000		; Startadresse

; --- Ein-/Ausgabe:

prozport 	= $01		; Prozessorport

memrom		= %00110111	; Basic+Kernal ROM
membas		= %00110110	; Basic ram+Kernal ROM
memram		= %00110101	; Basic+Kernal ROM


*= $0801
basic_start
;       2013 sys2061
	!by <EOP,>EOP,<(2013),>(2013),$9E
	!tx "2061"
	!by 0 			; End of Line
EOP	!by 0, 0		; Basic-Programmende

loader
!if loader != 2061 {
	!error "Loader-Adresse stimmt nicht mit SYS-Adresse überein!"
}
	ldy #0
	lda #memrom
	sta prozport		; ROM einblenden
	sty ptr_l

	; Basic und Kernal ins RAM kopieren

	lda #>basicrom		; Basic ROM Start
	jsr copyram
	lda #>kernalrom		; Kernal ROM Start
	jsr copyram

	; Patchliste abarbeiten (Basic und Kernal betreffend)

	ldx #0
nextp	lda patchlist,x
	sta ptr
       	tay

	lda patchlist+1,x
	sta ptr_h
	bne patch
	tya
	beq pend

patch	
	lda patchlist+3,x
	sta len_h
	lda patchlist+2,x
	sta len_l
	beq nohighcorr		; dec 0/0 Korrektur
	inc len_h
nohighcorr

	lda patchlist+4,x
	sta dest_l

	lda patchlist+5,x
	sta dest_h

	ldy #0
ploop	lda (ptr),y		; Patch an richtige Adresse
	sta (dest),y		; übertragen
	iny
	bne nohigh
	inc ptr_h		; High Byte bei Überlauf
	inc dest_h
nohigh
	dec len_l		; Länge herunter
	bne ploop		; zählen nach
	dec len_h		; dec 0/0 Methode
	bne ploop
	txa			; Index auf nächsten Patch
	clc			; positionieren ...
	adc #6
	tax
	bne nextp		; immer

pend
	lda #memram		; BASIC, KERNAL RAM aktivieren
	sta prozport
	rts


; 8kByte Block an geleiche Stelle kopieren

copyram
	sta ptr_h		; Startadresse
	ldx #$20		; Pages: 8K
toram	lda (ptr),y		; ROM lesen
	sta (ptr),y		; RAM schreiben
	iny
	bne toram
	inc ptr_h		; nächste "Page"
	dex
	bne toram
	rts

