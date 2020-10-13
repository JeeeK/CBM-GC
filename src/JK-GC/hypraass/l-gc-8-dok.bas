

;l-gc-8-dok 

  hypra-ass  assemblerlisting:

               1    -.li 3,8,3,"l-gc-8-dok,w,p"
  ; *************************
  ; *  garbage  collection  *
  ; *   von johann klasek   *
  ; * 1985-12-27 vers. 1.1  *
  ; *************************
  ;
  ; aufruf: sys ...
  ; raeumt stringspeicher auf
  ;
  ; es werden nur jene speicherstellen
  ; benuetzt, die auch die normale
  ; gc verwendet; alle anderen
  ; werden wieder restauriert.
  ;
               100  -.eq strptr = $33  ; stringpointer
               110  -.eq bufptr = $5f  ; bufferpointer
               120  -.eq stradr = $58  ; stringadresse
               130  -.eq strdp  = $22  ; stringdescriptoradresse
               140  -.eq membeg = $31  ; speicher-anfang
               150  -.eq memend = $37  ; speicher-ende
               160  -.eq beranf = $fb  ; bereichsanfang u. -ende
               170  -.eq berend = $fd
               180  -.eq flag   = $53  ; ende-flag
               190  -.eq len    = $4e  ; stringlaenge
               200  -.eq stat   = $4f  ; string-type
               210  -.eq ptr    = $50  ; array-pointer
               990  -.ba $c500
  c500 78     :1000 -          sei            ; betriebssys.-rom
  c501 a501   :1010 -          lda 1          ; wegblenden
  c503 48     :1020 -          pha            ; damit ram zugaenglich
  c504 a935   :1030 -          lda #$35       ; wird
  c506 8501   :1040 -          sta 1
  c508 a537   :1050 -          lda memend     ; stringpointer
  c50a 8533   :1060 -          sta strptr     ; und bereichanfang
  c50c a638   :1070 -          ldx memend+1   ; auf speicherende
  c50e 8634   :1080 -          stx strptr+1   ; setzen
  c510 85fb   :1090 -          sta beranf
  c512 86fc   :1100 -          stx beranf+1
  c514 a550   :1101 -          lda ptr        ; ptr-register retten
  c516 a651   :1102 -          ldx ptr+1
  c518 8ddcc6 :1103 -          sta sptr
  c51b 8eddc6 :1104 -          stx sptr+1
  c51e a003   :1105 -          ldy #3
  c520 b9fb00 :1106 -l10       lda $fb,y      ; zero-page-reg.
  c523 99dec6 :1107 -          sta sptr+2,y   ; retten
  c526 88     :1108 -          dey 
  c527 10f7   :1109 -          bpl l10
  c529 c8     :1110 -          iny 
  c52a 8453   :1120 -          sty flag       ; flag=0
  c52c a6fb   :1130 -begin1    ldx beranf     ; *** hauptschleife ***
  c52e a5fc   :1140 -          lda beranf+1   ; bereich
  c530 86fd   :1150 -          stx berend     ; genau um 8k
  c532 85fe   :1160 -          sta berend+1   ; nach unten verlegen
  c534 38     :1170 -          sec 
  c535 e920   :1180 -          sbc #$20
  c537 9008   :1185 -          bcc l2
  c539 85fc   :1190 -          sta beranf+1
  c53b e431   :1200 -          cpx membeg     ; speicheranfang
  c53d e532   :1210 -          sbc membeg+1   ; erreicht?
  c53f b012   :1220 -          bcs l1
  c541 e653   :1230 -l2        inc flag       ; ja, endeflag setzen
  c543 a531   :1240 -          lda membeg     ; bereichanfang =
  c545 a632   :1250 -          ldx membeg+1   ; speicheranfang
  c547 85fb   :1260 -          sta beranf     ; bereichanfang = be-
  c549 86fc   :1270 -          stx beranf+1   ; reichende (sonderfall)
  c54b c5fd   :1271 -          cmp berend     ; => bereich ist 0 byte
  c54d d004   :1272 -          bne l1         ; lang => fertig
  c54f e4fe   :1273 -          cpx berend+1
  c551 f074   :1274 -          beq ende
  c553 a900   :1280 -l1        lda #0         ; bufferpointer auf
  c555 855f   :1290 -          sta bufptr     ; $10000 od. dez.
  c557 8560   :1300 -          sta bufptr+1   ; 65536 setzen
  c559 38     :1360 -          sec            ; c=1: getsa-subr init
  c55a 20e1c5 :1370 -begin2    jsr getsa      ; string-adresse holen
  c55d a54e   :1372 -          lda len        ; stringlaenge = 0 ?
  c55f f03d   :1373 -          beq weiter     ; keinen string mehr
  c561 98     :1380 -          tya            ; gefunden
  c562 e4fb   :1390 -          cpx beranf
  c564 e5fc   :1400 -          sbc beranf+1
  c566 9032   :1410 -          bcc l3         ; liegt stringadr.
  c568 98     :1420 -          tya            ; im bereich ?
  c569 e4fd   :1430 -          cpx berend
  c56b e5fe   :1440 -          sbc berend+1
  c56d b02b   :1450 -          bcs l3         ; nein, naechster str.
  c56f 38     :1460 -          sec            ; ja, bufferpointer um
  c570 a55f   :1470 -          lda bufptr     ; stringlaenge
  c572 e54e   :1480 -          sbc len        ; verschieben
  c574 855f   :1490 -          sta bufptr
  c576 b002   :1500 -          bcs l4
  c578 c660   :1510 -          dec bufptr+1
  c57a 8459   :1512 -l4        sty stradr+1   ; stringadr. abspeicher
  c57c 8658   :1513 -          stx stradr
  c57e a64e   :1520 -          ldx len        ; string in den
  c580 a000   :1530 -          ldy #0         ; bufferbereich
  c582 b158   :1540 -loop1     lda (stradr),y ; uebertragen
  c584 915f   :1550 -          sta (bufptr),y
  c586 c8     :1560 -          iny 
  c587 ca     :1570 -          dex 
  c588 d0f8   :1580 -          bne loop1
  c58a 18     :1590 -          clc 
  c58b a55f   :1600 -          lda bufptr     ; neue stringadresse
  c58d 6533   :1610 -          adc strptr     ; berechnen:
  c58f 8558   :1620 -          sta stradr     ; stradr=bufptr-(65536-
  c591 a560   :1630 -          lda bufptr+1   ; strptr)
  c593 6534   :1640 -          adc strptr+1   ; da 16-bit berechnung
  c595 8559   :1650 -          sta stradr+1   ; => 65536 = 0 => sa=bp+sp
  c597 20cac6 :1660 -          jsr corr       ; stringdescriptor
  c59a 18     :1670 -l3        clc            ; korrigieren
  c59b 4c5ac5 :1680 -          jmp begin2     ; naechsten string
  c59e a55f   :1750 -weiter    lda bufptr     ; buffer leer ?
  c5a0 0560   :1760 -          ora bufptr+1
  c5a2 f01c   :1770 -          beq l6         ; ja, dann braucht
  c5a4 18     :1780 -          clc            ; nichts verschoben
  c5a5 a533   :1790 -          lda strptr     ; werden
  c5a7 aa     :1795 -          tax 
  c5a8 655f   :1800 -          adc bufptr     ; verschiebe parameter
  c5aa 8533   :1810 -          sta strptr     ; ermitteln
  c5ac a534   :1820 -          lda strptr+1
  c5ae a8     :1825 -          tay 
  c5af 6560   :1830 -          adc bufptr+1   ; bufptr=quellblockanfang
  c5b1 8534   :1840 -          sta strptr+1
  c5b3 a900   :1850 -          lda #$00       ; quellblockende+1
  c5b5 855a   :1860 -          sta $5a        ; (=65536)
  c5b7 855b   :1870 -          sta $5b
  c5b9 8658   :1900 -          stx $58        ; zielblockende+1
  c5bb 8459   :1910 -          sty $59        ; basic-interpreter-
  c5bd 20bfa3 :1920 -          jsr $a3bf      ; routine:blockverschieben
  c5c0 a553   :1930 -l6        lda flag       ; endeflag gesetzt?
  c5c2 d003   :1950 -          bne ende
  c5c4 4c2cc5 :1960 -          jmp begin1     ; naechsten bereich be-
  c5c7 68     :1970 -ende      pla            ; arbeiten
  c5c8 8501   :1980 -          sta 1          ; auf betriebssys.-rom
  c5ca 58     :1990 -          cli            ; schalten
  c5cb addcc6 :1991 -          lda sptr       ; ptr-reg. in zero-page
  c5ce aeddc6 :1992 -          ldx sptr+1     ; wiederherstellen
  c5d1 8550   :1993 -          sta ptr
  c5d3 8651   :1994 -          stx ptr+1
  c5d5 a003   :1995 -          ldy #3
  c5d7 b9dec6 :1996 -l11       lda sptr+2,y   ; zero-page-reg. zu-
  c5da 99fb00 :1997 -          sta $fb,y      ; ruecksetzen
  c5dd 88     :1998 -          dey 
  c5de 10f7   :1999 -          bpl l11
  c5e0 60     :2000 -          rts            ; gc-routine ende
  ;
  ;
  c5e1 906c   :5000 -getsa     bcc g1         ; c=0 => naechste stradr
  c5e3 a900   :5010 -          lda #0         ; holen(zum normalen
  c5e5 8523   :5020 -          sta strdp+1    ; einsprung)
  c5e7 a216   :5030 -          ldx #$16       ; c=1:status u. str.-
  c5e9 8622   :5040 -          stx strdp      ; descriptorpointer init
  c5eb a903   :5050 -          lda #3         ;
  c5ed 854f   :5065 -          sta stat       ; status=3:descr.-stack
  c5ef a522   :5110 -dstck     lda strdp      ; stringdescriptor vom
  c5f1 18     :5120 -          clc            ; string descriptor-
  c5f2 6903   :5130 -          adc #3         ; stack holen
  c5f4 8522   :5140 -          sta strdp
  c5f6 a000   :5150 -          ldy #0
  c5f8 b122   :5160 -          lda (strdp),y
  c5fa 854e   :5170 -          sta len
  c5fc c8     :5180 -          iny 
  c5fd b122   :5190 -          lda (strdp),y
  c5ff aa     :5200 -          tax 
  c600 c8     :5210 -          iny 
  c601 b122   :5220 -          lda (strdp),y
  c603 a8     :5230 -          tay 
  c604 a516   :5240 -          lda $16
  c606 c522   :5250 -          cmp strdp
  c608 f005   :5260 -          beq n1         ; stringdescriporstack
  c60a a54e   :5262 -          lda len        ; leer? => variablen
  c60c f0e1   :5263 -          beq dstck
  c60e 60     :5270 -          rts 
  c60f a52d   :5280 -n1        lda $2d        ; var-anfang
  c611 a62e   :5290 -          ldx $2e
  c613 8522   :5300 -          sta strdp
  c615 8623   :5310 -          stx strdp+1
  c617 a522   :5320 -          lda strdp
  c619 a002   :5330 -          ldy #2         ; status=2 => variable
  c61b 844f   :5340 -          sty stat
  c61d d00b   :5351 -          bne v1
  c61f 18     :5352 -var       clc 
  c620 a522   :5353 -          lda strdp
  c622 6907   :5354 -          adc #7         ; variablen-laenge
  c624 8522   :5355 -          sta strdp
  c626 9002   :5356 -          bcc v1
  c628 e623   :5357 -          inc strdp+1
  c62a a623   :5360 -v1        ldx strdp+1
  c62c e430   :5370 -          cpx $30
  c62e d004   :5380 -          bne g2         ; var-ende erreicht
  c630 c52f   :5390 -          cmp $2f        ; var. ende ? ja, arrays
  c632 f026   :5400 -          beq n2         ; absuchen
  c634 a000   :5410 -g2        ldy #0
  c636 b122   :5420 -          lda (strdp),y  ; var-name untersuchen
  c638 30e5   :5430 -          bmi var        ; keine stringvar
  c63a c8     :5440 -          iny 
  c63b b122   :5450 -          lda (strdp),y
  c63d 10e0   :5460 -          bpl var        ; keine stringvariable
  c63f c8     :5470 -          iny 
  c640 b122   :5480 -          lda (strdp),y  ; stringlaenge = 0 ?
  c642 f0db   :5485 -          beq var        ; naechste variable
  c644 854e   :5490 -          sta len
  c646 c8     :5500 -          iny 
  c647 b122   :5510 -          lda (strdp),y  ; stringadr. holen
  c649 aa     :5520 -          tax 
  c64a c8     :5530 -          iny 
  c64b b122   :5540 -          lda (strdp),y
  c64d a8     :5550 -          tay 
  c64e 60     :5560 -          rts 
  c64f a54f   :5561 -g1        lda stat       ; status pruefen
  c651 c902   :5562 -          cmp #2         ; und zur string-typ
  c653 904a   :5563 -          bcc array      ; verzweigen
  c655 f0c8   :5564 -          beq var
  c657 4cefc5 :5565 -          jmp dstck
  c65a 8550   :5570 -n2        sta ptr        ; pointer auf array init
  c65c 8651   :5580 -          stx ptr+1
  c65e a001   :5590 -          ldy #1         ; status=1: array
  c660 844f   :5600 -          sty stat
  c662 a550   :5660 -nextarr   lda ptr        ; array-ende erreicht ?
  c664 a651   :5670 -          ldx ptr+1
  c666 e432   :5680 -          cpx $32
  c668 d004   :5690 -          bne a1
  c66a c531   :5700 -          cmp $31
  c66c f057   :5710 -          beq n3
  c66e 8522   :5720 -a1        sta strdp
  c670 8623   :5730 -          stx strdp+1
  c672 a000   :5740 -          ldy #0
  c674 b122   :5750 -          lda (strdp),y  ; array-name
  c676 aa     :5760 -          tax 
  c677 c8     :5770 -          iny 
  c678 b122   :5780 -          lda (strdp),y
  c67a 08     :5790 -          php 
  c67b c8     :5800 -          iny 
  c67c b122   :5810 -          lda (strdp),y  ; ptr auf naechstes
  c67e 6550   :5820 -          adc ptr        ; array linken
  c680 8550   :5830 -          sta ptr
  c682 c8     :5840 -          iny 
  c683 b122   :5850 -          lda (strdp),y
  c685 6551   :5860 -          adc ptr+1
  c687 8551   :5870 -          sta ptr+1
  c689 28     :5880 -          plp            ; array-typ testen
  c68a 10d6   :5890 -          bpl nextarr    ; kein stringarray
  c68c 8a     :5900 -          txa 
  c68d 30d3   :5910 -          bmi nextarr
  c68f c8     :5920 -          iny 
  c690 b122   :5930 -          lda (strdp),y  ; anzahl der dimensionen
  c692 a000   :5940 -          ldy #0
  c694 0a     :5950 -          asl            ; dimensionenwerte
  c695 6902   :5960 -          adc #2         ; ueberlesen
  c697 6522   :5970 -          adc strdp
  c699 8522   :5980 -          sta strdp
  c69b 9002   :5990 -          bcc array
  c69d e623   :6000 -          inc strdp+1
  c69f 18     :6001 -array     clc 
  c6a0 a522   :6002 -          lda strdp
  c6a2 6903   :6003 -          adc #3         ; stringdescriptorlaenge
  c6a4 8522   :6004 -          sta strdp
  c6a6 9002   :6005 -          bcc a5
  c6a8 e623   :6006 -          inc strdp+1
  c6aa a623   :6010 -a5        ldx strdp+1
  c6ac e451   :6020 -          cpx ptr+1      ; array fertig durch-
  c6ae d004   :6030 -          bne a6         ; sucht?
  c6b0 c550   :6040 -          cmp ptr
  c6b2 f0ae   :6050 -          beq nextarr
  c6b4 a000   :6060 -a6        ldy #0
  c6b6 b122   :6070 -          lda (strdp),y
  c6b8 f0e5   :6075 -          beq array      ; laenge=0 => nachstes
  c6ba 854e   :6080 -          sta len        ; arrayelement
  c6bc c8     :6090 -          iny 
  c6bd b122   :6100 -          lda (strdp),y  ; stringadresse ueber-
  c6bf aa     :6110 -          tax            ; geben
  c6c0 c8     :6120 -          iny 
  c6c1 b122   :6130 -          lda (strdp),y
  c6c3 a8     :6140 -          tay 
  c6c4 60     :6200 -          rts 
  c6c5 a900   :6210 -n3        lda #0         ; keinen string mehr
  c6c7 854e   :6220 -          sta len        ; gefunden
  c6c9 60     :6230 -          rts 
  ;
  ;
  c6ca a54f   :8000 -corr      lda stat       ; stringadr. korrigieren
  c6cc 0a     :8010 -          asl            ; c=0,mal 2
  c6cd 69ff   :8020 -          adc #$ff       ; minus 1
  c6cf 2903   :8030 -          and #3         ; nur 2 bits als index
  c6d1 a8     :8040 -          tay            ; (positionsberechnung,
  c6d2 a558   :8050 -          lda stradr     ; da lage d. stringadr.
  c6d4 9122   :8060 -          sta (strdp),y  ; bei dstack, var u.
  c6d6 c8     :8070 -          iny            ; array verschieden
  c6d7 a559   :8080 -          lda stradr+1   ; ist)
  c6d9 9122   :8090 -          sta (strdp),y
  c6db 60     :8100 -          rts 
               8500 -sptr      .by 0,0,0,0,0  ; 5-bytes zero-page

