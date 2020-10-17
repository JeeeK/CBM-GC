
; ******** Source: jk-gc.asm
     1                          *= $C500
     2                          !to "jk-gc.o", cbm	; set output file and format
     3                          ;
     4                          ;.OPT LIST,NOSYM
     5                          ;
     6                          ; *************************
     7                          ; *  GARBAGE  COLLECTION  *
     8                          ; *   VON JOHANN KLASEK   *
     9                          ; * 1985-12-27 VERS. 1.1  *
    10                          ; * 2013-11-24 VERS. 2.0  *
    11                          ; *************************
    12                          ;
    13                          ; AUFRUF: SYS ...
    14                          ; RÄUMT STRINGSPEICHER AUF
    15                          ; ES WERDEN NUR JENE SPEICHERSTELLEN
    16                          ; BENUTZT, DIE AUCH DIE NORMALE
    17                          ; GC VERWENDET; ALLE ANDEREN
    18                          ; WERDEN WIEDER RESTAURIERT.
    19                          
    20                          ; BASIC SYSTEMVARIABLEN
    21                          
    22                          TOSS     = $19		; TOP OF STRINGSTACK
    23                          EOSS     = $22		; END OF STRINGSTACK +1
    24                          TSSP     = $16		; TEMP. STRINGSTACK POINTER
    25                          
    26                          VARTAB   = $2D		; Basicprogrammende = Variablenanfang
    27                          ARYTAB   = $2F		; Variablenende = Arraybereichanfang
    28                          STREND   = $31		; Arraybereichende = unterste Stringheap-Adresse
    29                          FRETOP   = $33		; aktuelle Stringheap-Adresse
    30                          MEMSIZ   = $37		; höchste RAM-Adresse für Basic, Start
    31                          			; des nach unten wachsenden Stringheaps
    32                          MEMBEG   = STREND	; MEMORY BEGINN = STREND
    33                          MEMEND   = MEMSIZ	; MEMORY END
    34                          
    35                          ; VARIABLEN
    36                          
    37                          STRPTR   = FRETOP	; STRING POINTER = FRETOP
    38                          STRDP    = $22		; STRING DESCRIPTOR ADDRESS
    39                          BERANF   = $4C		; BEREICHSANFANG
    40                          NEWPTR	 = $4E		; NEUER STRING POINTER
    41                          PTR      = $50		; ARRAY POINTER
    42                          LEN      = $52		; STRING LENGTH
    43                          FLAG     = $53		; ENDE-FLAG
    44                          ; $54-$56 BELEGT
    45                          STAT     = $57		; STRING STATE
    46                          ; $58-5B wird von MOVBLOCK zerstört!
    47                          STRADR   = $58		; STRING ADDRESS (TEMP.)
    48                          BEREND   = $5D		; BEREICHSENDE
    49                          BUFPTR   = $5F		; BUFFER POINTER
    50                          			; (MOVBLOCK: QUELLBLOCKANFANG!)
    51                          
    52                          CPTR     = $22		; POINTER FÜR INSTALL-ROUTINE
    53                          
    54                          ; ZU RETTENDER ZEROPAGE-BEREICH
    55                          ZPSTART  = $4C		; 1. zu rettende
    56                          ZPEND    = $53		; letzte zu rettende
    57                          ZPLEN    = ZPEND-ZPSTART+1
    58                          			; Anzahl zu rettende
    59                          
    60                          ; KONSTANTEN
    61                          
    62                          STAT_SDS = 5
    63                          STAT_VAR = 3
    64                          STAT_ARY = 1
    65                          
    66                          
    67                          ; SPEICHERORTE
    68                          
    69                          GARBCOL  = $B526	; Einsprungpunkt GC
    70                          
    71                          MOVBLOCK = $A3BF	; Block verschieben
    72                          			; zerstört $58/$59/$5A/$5B/$22
    73                          
    74                          BASIC    = $A000        ; BASIC-ROM
    75                          KERNAL   = $E000        ; KERNAL-ROM
    76                          ROMSIZE  = $2000        ; ROM-Länge 8k
    77                          BUF	 = KERNAL	; Puffer unter Kernal
    78                          BUFSIZE  = ROMSIZE	; Puffergröße
    79                          
    80                          ; I/O-BEREICHE/-ADRESSEN
    81                          
    82                          VIDBASE  = $0400	; Video RAM
    83                          COLBASE  = $D800	; Color RAM
    84                          
    85                          MARKE    = "*"		; Aktivitätsanzeige
    86                          MARKEFARBE = 9		; rot
    87                          MARKEOFF = 40*25-1	; Markeposition
    88                          MARKEVID = VIDBASE+MARKEOFF
    89                          MARKECOL = COLBASE+MARKEOFF
    90                          
    91                          PROZPORT = $01          ; Prozessorport
    92                          MEMROM = %00110111      ; Basic+Kernal ROM, $37
    93                          MEMBAS = %00110110      ; Basic RAM+Kernal ROM, $34
    94                          MEMRAM = %00110101      ; Basic+Kernal RAM, $35
    95                          
    96                          
    97                          
    98                          ; Installer
    99                          
   100                          INSTALL
   101  c500 a937               	LDA #MEMROM
   102  c502 8501               	STA PROZPORT	; ALLES ROM
   103  c504 a000               	LDY #<BASIC	; ROM-BEGINN
   104  c506 8422               	STY CPTR
   105  c508 a9a0               	LDA #>BASIC
   106  c50a 8523               	STA CPTR+1	; BASIC-ROM ANFANG
   107  c50c a220               	LDX #>($2000)	; BASIC-ROM LÄNGE
   108  c50e b122               CPYROM	LDA (CPTR),Y	; ROM LESEN
   109  c510 9122               	STA (CPTR),Y	; RAM SCHREIBEN
   110  c512 c8                 	INY
   111  c513 d0f9               	BNE CPYROM
   112  c515 e623               	INC CPTR+1	; NÄCHSTE PAGE
   113  c517 ca                 	DEX
   114  c518 d0f4               	BNE CPYROM
   115  c51a a501               	LDA PROZPORT
   116  c51c 29fe               	AND #%11111110	; BASIC-ROM AUS MASKE
   117  c51e 8501               	STA PROZPORT
   118  c520 a930               	LDA #<COLLECT	; JMP COLLECT
   119  c522 8d27b5             	STA GARBCOL+1	; PATCHEN ...
   120  c525 a9c5               	LDA #>COLLECT
   121  c527 8d28b5             	STA GARBCOL+2
   122  c52a a94c               	LDA #$4C	; JMP OPCODE
   123  c52c 8d26b5             	STA GARBCOL
   124  c52f 60                 	RTS
   125                          
   126                          ; *** Garbage Collector
   127                          
   128                          COLLECT
   129  c530 ade707             	LDA MARKEVID	; KONTROLLANZEIGE BILDSCHIRM
   130  c533 8d0dc7             	STA ORIGVID
   131  c536 a92a               	LDA #MARKE
   132  c538 8de707             	STA MARKEVID	; MARKE ZEICHEN
   133  c53b ade7db             	LDA MARKECOL
   134  c53e 8d0ec7             	STA ORIGCOL
   135  c541 a909               	LDA #MARKEFARBE
   136  c543 8de7db             	STA MARKECOL	; MARKE FARBE
   137                          
   138  c546 a208               	LDX #ZPLEN	; ZEROPAGE-REG.
   139  c548 b54b               SAVLOOP	LDA ZPSTART-1,X	; RETTEN
   140  c54a 9d0ec7             	STA SAVE-1,X
   141  c54d ca                 	DEX
   142  c54e d0f8               	BNE SAVLOOP
   143                          
   144  c550 8653               	STX FLAG	; ENDEFLAG=0 (NEIN)
   145  c552 a537               	LDA MEMEND	; STRING POINTER
   146  c554 a638               	LDX MEMEND+1	; UND BEREICHANFANG 
   147  c556 8533               	STA STRPTR	; AUF SPEICHERENDE
   148  c558 8634               	STX STRPTR+1	; SETZEN
   149  c55a 854c               	STA BERANF
   150  c55c 864d               	STX BERANF+1
   151                          
   152                          ; *** Bereiche
   153                          
   154                          NEXTBLOCK
   155  c55e a533               	LDA STRPTR	; NEWPTR PARALLEL MIT
   156  c560 854e               	STA NEWPTR	; BUFPTR MITZIEHEN ...
   157  c562 a534               	LDA STRPTR+1
   158  c564 854f               	STA NEWPTR+1
   159  c566 a64c               	LDX BERANF
   160  c568 a54d               	LDA BERANF+1	; BEREICH
   161  c56a 865d               	STX BEREND	; UM PUFFERLÄNGE
   162  c56c 855e               	STA BEREND+1	; NACH UNTEN VERLEGEN
   163  c56e 38                 	SEC
   164  c56f e920               	SBC #(>BUFSIZE)
   165                          			; -1 PAGE XXX
   166  c571 9008               	BCC LASTRANGE	; <0 (ALSO <STREND)
   167  c573 854d               	STA BERANF+1
   168  c575 e431               	CPX STREND	; STRINGS ENDE
   169  c577 e532               	SBC STREND+1	; ERREICHT?
   170  c579 b02d               	BCS STRINRANGE
   171                          LASTRANGE
   172  c57b e653               	INC FLAG	; JA, ENDEFLAG SETZEN
   173  c57d a531               	LDA STREND	; BEREICHANFANG =
   174  c57f a632               	LDX STREND+1	; SPEICHERANFANG
   175  c581 854c               	STA BERANF	; BEREICHANFANG = BEREICHENDE
   176  c583 864d               	STX BERANF+1	; (SONDERFALL)
   177                          
   178  c585 c55d               	CMP BEREND	; -> BEREICH IST 0 BYTE LANG
   179  c587 d01f               	BNE STRINRANGE	; -> FERTIG
   180  c589 e45e               	CPX BEREND+1
   181  c58b d01b               	BNE STRINRANGE
   182                          
   183                          CHECKEND
   184  c58d a553               	LDA FLAG	; ENDFLAG GESETZT?
   185  c58f f0cd               	BEQ NEXTBLOCK	; NÄCHSTEN BEREICH UNTERSUCHEN
   186                          
   187                          ; *** Ende
   188                          
   189  c591 a208               	LDX #ZPLEN	; ZEROPAGE-REG.
   190  c593 bd0ec7             RESLOOP	LDA SAVE-1,X
   191  c596 954b               	STA ZPSTART-1,X	; RESTAURIEREN
   192  c598 ca                 	DEX
   193  c599 d0f8               	BNE RESLOOP
   194                          
   195  c59b ad0dc7             	LDA ORIGVID	; KONTROLLANZEIGE LÖSCH
   196  c59e 8de707             	STA MARKEVID	; UND ALTEN ZUSTAND WIEDER HERSTELLEN
   197  c5a1 ad0ec7             	LDA ORIGCOL
   198  c5a4 8de7db             	STA MARKECOL
   199                          
   200  c5a7 60                 	RTS
   201                          
   202                          STRINRANGE
   203                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   204                          	LDA #>(BUF+BUFSIZE)
   205                          	STA BUFPTR+1
   206                          	LDA #<(BUF+BUFSIZE)
   207                          	STA BUFPTR
   208                          } else {
   209  c5a8 a900               	LDA #0		; BUFFERPOINTER AUF
   210  c5aa 855f               	STA BUFPTR	; $10000 (65536) = 0
   211  c5ac 8560               	STA BUFPTR+1	; SETZEN.
   212                          }
   213  c5ae 38                 	SEC
   214  c5af 24                 	!byte $24	; BIT ZP, NÄCHSTEN BEFEHL IGNORIEREN
   215                          NEXTSTR	
   216  c5b0 18                 	CLC
   217                          NEXTSTR1
   218  c5b1 2022c6             	JSR GETSA	; NÄCHSTE STRINGADRESSE HOLEN
   219  c5b4 a552               	LDA LEN		; WENN 0, DANN
   220  c5b6 f03c               	BEQ WEITER	; KEINEN STRING MEHR GEFUNDEN!
   221                          
   222  c5b8 98                 	TYA		; HIGH BYTE
   223  c5b9 e45d               	CPX BEREND
   224  c5bb e55e               	SBC BEREND+1	; ÜBER BEREICH
   225  c5bd b0f1               	BCS NEXTSTR	; NÄCHSTER STRING!
   226  c5bf 98                 	TYA		; HIGH BYTE
   227  c5c0 e44c               	CPX BERANF
   228  c5c2 e54d               	SBC BERANF+1	; UNTER BEREICH
   229  c5c4 90eb               	BCC NEXTSTR1	; NÄCHSTER STRING!
   230                          
   231  c5c6 a55f               	LDA BUFPTR	; STRINGLÄNGE VERSCHIEBEN
   232  c5c8 e552               	SBC LEN
   233  c5ca 855f               	STA BUFPTR
   234  c5cc b002               	BCS L4
   235  c5ce c660               	DEC BUFPTR+1
   236                          
   237  c5d0 8459               L4	STY STRADR+1	; STRINGADRESSE ABSPEICHERN
   238  c5d2 8658               	STX STRADR
   239                          
   240  c5d4 a452               	LDY LEN
   241  c5d6 88                 	DEY
   242  c5d7 f007               	BEQ LEN1
   243  c5d9 b158               NEXTBYT	LDA (STRADR),Y	; STRING IN DEN BUFFERBEREICH
   244  c5db 915f               	STA (BUFPTR),Y	; ÜBERTRAGEN
   245  c5dd 88                 	DEY
   246  c5de d0f9               	BNE NEXTBYT
   247                          LEN1
   248  c5e0 b158               	LDA (STRADR),Y	; DAS 0. BYTE EXTRA
   249  c5e2 915f               	STA (BUFPTR),Y	; 
   250                          
   251  c5e4 38                 	SEC		; NEUE STRINGADRESSE BERECHNEN!
   252  c5e5 a54e               	LDA NEWPTR
   253  c5e7 e552               	SBC LEN
   254  c5e9 854e               	STA NEWPTR
   255  c5eb b002               	BCS L5
   256  c5ed c64f               	DEC NEWPTR+1
   257                          L5
   258  c5ef 20fec6             	JSR CORR	; STRINGADRESSE IN DESCRIPTOR ÄNDERN
   259                          			; Z=0
   260  c5f2 d0bc               	BNE NEXTSTR	; UNBEDINGT, NÄCHSTER STRING
   261                          
   262                          WEITER	
   263                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   264                          	LDA BUFPTR	; BUFFER LEER ...
   265                          	CMP #<(BUF+BUFSIZE)
   266                          	BNE WEITER1
   267                          	LDA BUFPTR+1	; WENN PTR AM ENDE
   268                          	CMP #>(BUF+BUFSIZE)
   269                          	BEQ CHECKEND
   270                          WEITER1
   271                          } else {
   272  c5f4 a55f               	LDA BUFPTR	; BUFFER LEER
   273  c5f6 0560               	ORA BUFPTR+1	; WENN PTR =0 (ENDE)
   274  c5f8 f093               	BEQ CHECKEND
   275                          }
   276                          
   277  c5fa a533               	LDA STRPTR
   278  c5fc 8558               	STA $58		; ZIELBLOCKENDE+1
   279  c5fe a534               	LDA STRPTR+1
   280  c600 8559               	STA $59
   281  c602 a54e               	LDA NEWPTR
   282  c604 8533               	STA STRPTR	; NEUES FRETOP
   283  c606 a54f               	LDA NEWPTR+1	
   284  c608 8534               	STA STRPTR+1
   285                          
   286                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   287                          	LDA #<(BUF+BUFSIZE)
   288                          	STA $5A
   289                          	LDA #>(BUF+BUFSIZE)
   290                          	STA $5B
   291                          } else {
   292  c60a a900               	LDA #$00	; QUELLBLOCKENDE+1
   293  c60c 855a               	STA $5A
   294  c60e 855b               	STA $5B
   295                          }
   296                          			; QUELLBOCKANFANG = BUFPTR
   297                          
   298  c610 78                 	SEI		; BETRIEBSSYS.-ROM
   299  c611 a501               	LDA PROZPORT	; WEGBLENDEN
   300  c613 48                 	PHA		; DAMIT RAM ZUGÄNGLICH
   301  c614 a935               	LDA #MEMRAM	; WIRD
   302                          
   303  c616 8501               	STA PROZPORT
   304                          
   305  c618 20bfa3             	JSR MOVBLOCK	; BASIC-ROUTINE BLOCKVERSCHIEBEN
   306                          			; Z=1
   307  c61b 68                 	PLA		; URSPRÜNGLICHER ZUSTAND
   308  c61c 8501               	STA PROZPORT	; KERNAL-ROM WIEDER AKTIVIEREN
   309  c61e 58                 	CLI
   310  c61f 4c8dc5             	JMP CHECKEND	; IMMER
   311                          
   312                          
   313                          ;
   314                          ; GETSA: ( C, STRDP -> STRDP, LEN, X, Y )
   315                          ;
   316                          
   317  c622 9065               GETSA	BCC CHECKTYPE	; C=0 -> NÄCHSTEN STRING
   318                          
   319                          ; *** STRING DESCRIPTOR STACK (SDS)
   320                          
   321                          ;         belegt->|<-frei
   322                          ;    +-------------+
   323                          ;    |             V
   324                          ;   +-+     +-----+-----+-----+
   325                          ;   | |     |S|L|H|S|L|H|S|L|H|
   326                          ;   +-+     +-----+-----+-----+
   327                          ;    ^       ^     ^     ^     ^
   328                          ;    $16     $19   $1C   $1F   $21
   329                          ;    TSSP    TOSS
   330                          ;
   331                          DESCSTACK
   332  c624 a000               	LDY #0
   333  c626 8423               	STY STRDP+1	; DESCRIPTOR AUF
   334  c628 a919               	LDA #TOSS	; SDS
   335  c62a 8522               	STA STRDP
   336  c62c a205               	LDX #STAT_SDS
   337  c62e 8657               	STX STAT
   338  c630 d005               	BNE ISDSTEND	; IMMER
   339  c632 18                 DSTACK	CLC
   340  c633 a522               	LDA STRDP
   341  c635 6903               NEXTDST	ADC #3
   342                          ISDSTEND
   343  c637 c516               	CMP TSSP	; STACK DURCH?
   344  c639 f00e               	BEQ VARS
   345  c63b aa                 	TAX
   346  c63c b400               	LDY 0,X
   347  c63e f0f5               	BEQ NEXTDST
   348  c640 8452               	STY LEN
   349  c642 b502               	LDA 2,X		; STRINGADR. HIGH
   350  c644 a8                 	TAY
   351  c645 b501               	LDA 1,X		; STRINGADR. LOW
   352  c647 aa                 	TAX
   353  c648 60                 	RTS
   354                          
   355                          ; *** VARIABLEN
   356                          
   357  c649 a52d               VARS	LDA VARTAB	; VARIABLENANFANG
   358  c64b a62e               	LDX VARTAB+1
   359  c64d 8522               	STA STRDP
   360  c64f 8623               	STX STRDP+1
   361  c651 a522               	LDA STRDP
   362  c653 a003               	LDY #STAT_VAR	; STATUS: EINFACHE VARIABLEN
   363  c655 8457               	STY STAT
   364  c657 d00b               	BNE ISVAREND
   365                          VAR
   366  c659 18                 NEXTVAR	CLC		; NÄCHSTE VARIABLE
   367  c65a a522               	LDA STRDP
   368  c65c 6907               	ADC #7		; VARIABLENLÄNGE
   369  c65e 8522               	STA STRDP
   370  c660 9002               	BCC ISVAREND
   371  c662 e623               	INC STRDP+1
   372                          ISVAREND
   373  c664 c52f               	CMP ARYTAB
   374  c666 d006               	BNE CHECKVAR
   375  c668 a623               	LDX STRDP+1	; VAR-ENDE (=ARRAY-ANFANG)?
   376  c66a e430               	CPX ARYTAB+1
   377  c66c f026               	BEQ ARRAYS	; VAR.-ENDE, WEITER MIT ARRAYS
   378                          CHECKVAR
   379  c66e a000               	LDY #0		; VARIABLENNAME
   380  c670 b122               	LDA (STRDP),Y	; 1. ZEICHEN
   381  c672 30e5               	BMI NEXTVAR	; KEIN STRING, NÄCHSTE V.
   382  c674 c8                 	INY
   383  c675 b122               	LDA (STRDP),Y
   384  c677 10e0               	BPL NEXTVAR	; KEIN STRING, NÄCHSTE V.
   385  c679 c8                 	INY
   386  c67a b122               	LDA (STRDP),Y	; STRINGLÄNGE
   387  c67c f0db               	BEQ NEXTVAR	; = 0, NÄCHSTE V.
   388  c67e 8552               	STA LEN
   389  c680 c8                 	INY
   390  c681 b122               	LDA (STRDP),Y	; ADRESSE LOW
   391  c683 aa                 	TAX
   392  c684 c8                 	INY
   393  c685 b122               	LDA (STRDP),Y	; ADRESSE HIGH
   394  c687 a8                 	TAY
   395  c688 60                 	RTS
   396                          
   397                          CHECKTYPE
   398  c689 a557               	LDA STAT	; GETSA FORTSETZUNGSEINSTIEG
   399  c68b c903               	CMP #STAT_VAR	; STATUS STRING?
   400  c68d 9044               	BCC ARRAY	; =1 -> ARRAY
   401  c68f f0c8               	BEQ VAR		; =3 -> VARIABLE
   402  c691 4c32c6             	JMP DSTACK	; =5 -> STRING DESC. STACK
   403                          
   404  c694 8550               ARRAYS	STA PTR		; ARRAY POINTER
   405  c696 8651               	STX PTR+1
   406  c698 a001               	LDY #STAT_ARY
   407  c69a 8457               	STY STAT	; ARRAYS STATUS
   408                          ISARREND
   409  c69c a550               	LDA PTR
   410  c69e a651               	LDX PTR+1
   411  c6a0 e432               	CPX STREND+1
   412  c6a2 d004                       BNE NEXTARR
   413  c6a4 c531               	CMP STREND
   414  c6a6 f051               	BEQ NOSTRING	; ARRAYS FERTIG
   415                          NEXTARR
   416  c6a8 8522               	STA STRDP	; IMMER C=0
   417  c6aa 8623               	STX STRDP+1
   418  c6ac a000               	LDY #0
   419  c6ae b122               	LDA (STRDP),Y	; ARRAY-NAME
   420  c6b0 aa                 	TAX		; VAR-TYP MERKEN
   421  c6b1 c8                 	INY
   422  c6b2 b122               	LDA (STRDP),Y
   423  c6b4 08                 	PHP		; VAR-TYP MERKEN
   424  c6b5 c8                 	INY
   425  c6b6 b122               	LDA (STRDP),Y	; OFFSET NÄCHSTES ARRAY
   426  c6b8 6550               	ADC PTR		; C IST BEREITS 0 (CMP/CPX)
   427  c6ba 8550               	STA PTR		; START FOLGEARRAY
   428  c6bc c8                 	INY
   429  c6bd b122               	LDA (STRDP),Y
   430  c6bf 6551               	ADC PTR+1
   431  c6c1 8551               	STA PTR+1
   432  c6c3 28                 	PLP		; VAR-TYP HOLEN
   433  c6c4 10d6               	BPL ISARREND	; KEIN STRINGARRAY
   434  c6c6 8a                 	TXA		; VAR-TYP HOLEN
   435  c6c7 30d3               	BMI ISARREND	; KEIN STRINGARRAY
   436  c6c9 c8                 	INY
   437  c6ca b122               	LDA (STRDP),Y	; ANZAHL DER DIMENSIONEN
   438  c6cc 0a                 	ASL		; *2
   439  c6cd 6905               	ADC #5		; OFFSET = DIM*2+5
   440  c6cf a000               	LDY #0
   441  c6d1 f005               	BEQ ADVDESC
   442                          ARRAY
   443  c6d3 a000               	LDY #0
   444                          NEXTASTR
   445  c6d5 18                 	CLC
   446  c6d6 a903               	LDA #3		; STRING-DESCRIPTOR-LÄNGE
   447                          ADVDESC
   448  c6d8 6522               	ADC STRDP	; STRING WEITER
   449  c6da 8522               	STA STRDP
   450  c6dc 9002               	BCC ISLASTASTR
   451  c6de e623               	INC STRDP+1
   452                          ISLASTASTR
   453  c6e0 c550               	CMP PTR		; ARRAY DURCH?
   454  c6e2 d006               	BNE IS0ASTR
   455  c6e4 a623               	LDX STRDP+1
   456  c6e6 e451               	CPX PTR+1
   457  c6e8 f0b2               	BEQ ISARREND
   458                          IS0ASTR
   459  c6ea b122               	LDA (STRDP),Y	; STR.-LÄNGE
   460  c6ec f0e7               	BEQ NEXTASTR	; WEITER IM ARRAY
   461  c6ee 8552               	STA LEN
   462  c6f0 c8                 	INY
   463  c6f1 b122               	LDA (STRDP),Y	; ADRESSE LOW
   464  c6f3 aa                 	TAX
   465  c6f4 c8                 	INY
   466  c6f5 b122               	LDA (STRDP),Y	; ADRESSE HIGH
   467  c6f7 a8                 	TAY
   468  c6f8 60                 	RTS		; IN X/Y RETOUR
   469                          
   470                          NOSTRING
   471  c6f9 a900               	LDA #0
   472  c6fb 8552               	STA LEN
   473  c6fd 60                 	RTS
   474                          
   475                          ;
   476                          ; CORR ( STRADR, STAT -> )
   477                          ;
   478  c6fe a557               CORR	LDA STAT	; STR.-ADR. KORRIGIEREN
   479  c700 2903               	AND #%011	; NUR 2 BITS
   480  c702 a8                 	TAY		; LAGE DES DESCRIPTORS
   481  c703 a54e               	LDA NEWPTR	;
   482  c705 9122               	STA (STRDP),Y	; ... BEI STR.-STACK
   483  c707 c8                 	INY		; ... UND ARRAY VERSCHIEDEN!
   484  c708 a54f               	LDA NEWPTR+1
   485  c70a 9122               	STA (STRDP),Y
   486  c70c 60                 	RTS
   487                          
   488  c70d 00                 ORIGVID !byte 0		; ORIGINAL VIDEO DER MARKENPOSITION
   489  c70e 00                 ORIGCOL !byte 0		; ORIGINAL FARBE DER MARKENPOSITION
   490  c70f 00                 SAVE	!byte 0		; GESICHERTE ZP-VARIABLEN
   491                          *=*+ZPLEN-1
   492                          
   493                          
