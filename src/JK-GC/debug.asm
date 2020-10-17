

vic_cr	= $D011		; VIC control register
vic_mcr	= $D018		; VIC memory control register
cia_pra	= $DD00		; CIA 2 port register A

cram	= $CC00		; start of color ram

gram	= $e000		; start of graphic bitmap ram
gramp	= gram >> 8	; start page of bitmap

gra_off
	LDX #0		; let see what's on screen ...
	LDY #0
delay	NOP		; burn some cycles
	NOP
	NOP
	DEX
	BNE delay
	DEY
	BNE delay

        LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
        STA cia_pra
        LDA #((1 <<4) + (2 <<1) + 1)
			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
			; char addr $1000/4096 = char. ROM
        STA vic_mcr	; VIC memory control
        LDA vic_cr	; VIC control register
        AND #%11011111	; Hires mode off
        STA vic_cr
        RTS

gra_on

	LDA #$D0	; light green on black
	LDY #0		; start index
cram_loop
        STA cram,Y	; fill color RAM
        STA cram+$100,Y
        STA cram+$200,Y
        STA cram+$300-24,Y
        INY
        BNE cram_loop

	LDX #$20	; erase bitmap
	LDA #<(gram)	; (192 bytes more
	STA $22		; as necessary)
	LDA #>(gram)
	STA $23
	TYA
gram_loop
	STA ($22),y
	INY
	BNE gram_loop
	INC $23
	DEX
	BNE gram_loop

        LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
        STA cia_pra
        LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
        STA vic_mcr	; VIC memory control
        LDA vic_cr	; VIC control register
        ORA #%00100000	; Bit 5 = 1: Hires on
        STA vic_cr

        RTS

