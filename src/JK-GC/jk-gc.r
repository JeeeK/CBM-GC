
; ******** Source: jk-gc.asm
     1                          *= $C500
     2                          !to "jk-gc.o", cbm	; set output file and format
     3                          ;
     4                          ;.OPT LIST,NOSYM
     5                          ;
     6                          ; *************************
     7                          ; *  GARBAGE  COLLECTION  *
     8                          ; *   von Johann Klasek   *
     9                          ; * 1985-12-27 VERS. 1.1  *
    10                          ; * 2013-11-24 VERS. 2.0  *
    11                          ; * 2019-02-15 VERS. 2.1  *
    12                          ; *************************
    13                          ;
    14                          ; Aufruf: SYS ...
    15                          ; Räumt Stringspeicher auf
    16                          ; Es werden nur jene Speicherstellen
    17                          ; benutzt, die auch die normale
    18                          ; GC verwendet; alle anderen
    19                          ; werden wieder restauriert.
    20                          
    21                          ; BASIC Systemvariablen
    22                          
    23                          TOSS     = $19		; Top of Stringstack
    24                          EOSS     = $22		; End of Stringstack +1
    25                          TSSP     = $16		; Temp. Stringstack Pointer
    26                          
    27                          VARTAB   = $2D		; BASIC-Programmende = Variablenanfang
    28                          ARYTAB   = $2F		; Variablenende = Arraybereichanfang
    29                          STREND   = $31		; Arraybereichende = unterste String-Heap-Adresse
    30                          FRETOP   = $33		; aktuelle String-Heap-Adresse
    31                          MEMSIZ   = $37		; höchste RAM-Adresse für BASIC, Start
    32                          			; des nach unten wachsenden String-Heaps
    33                          MEMBEG   = STREND	; Memory begin = STREND
    34                          MEMEND   = MEMSIZ	; Memory end
    35                          
    36                          ; Variablen
    37                          
    38                          STRPTR   = FRETOP	; String-Pointer = FRETOP
    39                          STRDP    = $22		; String-Descriptor-Address
    40                          BERANF   = $4C		; Bereichsanfang
    41                          NEWPTR	 = $4E		; Neuer String-Pointer
    42                          PTR      = $50		; Array-Pointer
    43                          LEN      = $52		; String-Length
    44                          ; $54-$56 belegt
    45                          STAT     = $57		; String-State
    46                          ; $58-5B wird von MOVBLOCK zerstört!
    47                          STRADR   = $58		; String-Address (temp.)
    48                          BEREND   = $5D		; Bereichsende
    49                          BUFPTR   = $5F		; Buffer-Pointer
    50                          			; (MOVBLOCK: Quellblockanfang!)
    51                          
    52                          CPTR     = $22		; Pointer für Install-Routine
    53                          
    54                          ; zu rettender Zeropage-Bereich
    55                          ZPSTART  = $4C		; 1. zu rettende
    56                          ZPEND    = $52		; letzte zu rettende Stelle
    57                          ZPLEN    = ZPEND-ZPSTART+1
    58                          			; Anzahl zu rettenden Bytes
    59                          
    60                          ; Konstanten
    61                          
    62                          ; für Variabe STAT (String State):
    63                          STAT_SDS = 5		; String-Descriptor-Stack
    64                          STAT_VAR = 3		; einfache Variablen
    65                          STAT_ARY = 1		; Array
    66                          
    67                          
    68                          ; Speicherorte
    69                          
    70                          GARBCOL  = $B526	; Einsprungpunkt der GC
    71                          
    72                          MOVBLOCK = $A3BF	; Block verschieben
    73                          			; zerstört $58/$59/$5A/$5B/$22
    74                          
    75                          BASIC    = $A000        ; BASIC-ROM
    76                          KERNAL   = $E000        ; KERNAL-ROM
    77                          ROMSIZE  = $2000        ; ROM-Länge 8 KByte8
    78                          BUF	 = KERNAL	; Puffer unter KERNAL
    79                          BUFSIZE  = ROMSIZE	; Puffergröße
    80                          
    81                          ; I/O-BEREICHE/-ADRESSEN
    82                          
    83                          VIDBASE  = $0400	; Video RAM
    84                          COLBASE  = $D800	; Color RAM
    85                          
    86                          MARKE    = "*"		; Aktivitätsanzeige
    87                          MARKEFARBE = 9		; rot
    88                          MARKEOFF = 40*25-1	; Markenposition
    89                          MARKEVID = VIDBASE+MARKEOFF
    90                          MARKECOL = COLBASE+MARKEOFF
    91                          
    92                          PROZPORT = $01		; Prozessorport
    93                          MEMROM   = %00110111	; Basic+Kernal ROM, $37
    94                          MEMBAS   = %00110110	; Basic RAM+Kernal ROM, $34
    95                          MEMRAM   = %00110101	; Basic+Kernal RAM, $35
    96                          
    97                          
    98                          ; Debugging
    99                          
   100                          ;!set debug=1
   101                          
   102                          
   103                          ; Installer
   104                          
   105                          INSTALL
   106                          	; BASIC ins RAM kopieren, um die GC-Routine
   107                          	; zu patchen ...
   108  c500 a937               	LDA #MEMROM
   109  c502 8501               	STA PROZPORT	; alles ROM (also vom ROM kopieren)
   110  c504 a000               	LDY #<BASIC	; ROM-Beginn
   111  c506 8422               	STY CPTR
   112  c508 a9a0               	LDA #>BASIC
   113  c50a 8523               	STA CPTR+1	; BASIC-ROM Anfang
   114  c50c a220               	LDX #>($2000)	; BASIC-ROM Länge in Pages
   115  c50e b122               CPYROM	LDA (CPTR),Y	; ROM lesen
   116  c510 9122               	STA (CPTR),Y	; RAM schreiben
   117  c512 c8                 	INY
   118  c513 d0f9               	BNE CPYROM
   119  c515 e623               	INC CPTR+1	; nächste Page
   120  c517 ca                 	DEX		; Page-Zähler
   121  c518 d0f4               	BNE CPYROM
   122  c51a a501               	LDA PROZPORT	; auf RAM umschalten
   123  c51c 29fe               	AND #%11111110	; "BASIC-ROM aus"-Maske
   124  c51e 8501               	STA PROZPORT
   125  c520 a930               	LDA #<COLLECT	; "JMP COLLECT"
   126  c522 8d27b5             	STA GARBCOL+1	; patchen ...
   127  c525 a9c5               	LDA #>COLLECT
   128  c527 8d28b5             	STA GARBCOL+2
   129  c52a a94c               	LDA #$4C	; JMP-Opcode
   130  c52c 8d26b5             	STA GARBCOL
   131  c52f 60                 	RTS
   132                          
   133                          !ifdef debug {
   134                          !source "debug.asm"
   135                          }
   136                          
   137                          ; *** Garbage Collector
   138                          
   139                          COLLECT
   140                          !ifdef debug {
   141                          	JSR gra_on
   142                          }
   143  c530 ade707             	LDA MARKEVID	; Kontrollanzeige Bildschirm
   144  c533 8d00c7             	STA ORIGVID
   145  c536 a92a               	LDA #MARKE
   146  c538 8de707             	STA MARKEVID	; Marke: Zeichen
   147  c53b ade7db             	LDA MARKECOL	; sichern
   148  c53e 8d01c7             	STA ORIGCOL
   149  c541 a909               	LDA #MARKEFARBE
   150  c543 8de7db             	STA MARKECOL	; Marke: Farbe sichern
   151                          
   152  c546 a207               	LDX #ZPLEN	; Zeropage-Reg.
   153  c548 b54b               SAVLOOP	LDA ZPSTART-1,X	; retten
   154  c54a 9d01c7             	STA SAVE-1,X
   155  c54d ca                 	DEX
   156  c54e d0f8               	BNE SAVLOOP
   157                          
   158  c550 a537               	LDA MEMEND	; String-Pointer
   159  c552 a638               	LDX MEMEND+1	; und Bereichanfang 
   160  c554 8533               	STA STRPTR	; auf Speicherende
   161  c556 8634               	STX STRPTR+1	; setzen.
   162  c558 854c               	STA BERANF
   163  c55a 864d               	STX BERANF+1
   164                          
   165                          ; *** Nächster zu betrachtender Bereich am String-Heap
   166                          
   167                          ;                        STRADR
   168                          ;       +-------------------------------------+
   169                          ;       |                                     |
   170                          ;       |                                     V
   171                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   172                          ;   |L|PTR|      |      noch nicht       | gesuchte | frei | behandelte |
   173                          ;   | |   |      |  behandelte Strings   | Strings  |      |   Strings  |
   174                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   175                          ;    ^            ^                       ^          ^      ^            ^
   176                          ;    |            |                       |          |      |            |
   177                          ;    STRDP        STREND                  BERANF     BEREND STRPTR       MEMSIZ
   178                          ;                                                           =FRETOP
   179                          ;   SDS,VAR,ARY  |<-------------------- String-Heap -------------------->|
   180                          ;
   181                          ; Der Bereich BERANF bis BEREND (gesuchte Strings) ist immer um 256 Bytes 
   182                          ; kleiner als der Pufferbereich, da am Ende des Bereichs ein String beginnen
   183                          ; könnte, der max. 254 Bytes das Bereichsende überragen könnte. Dieser 
   184                          ; "Überhang" muss im Puffer Platz haben und dort reserviert sein!
   185                          
   186                          NEXTBLOCK
   187  c55c a533               	LDA STRPTR	; NEWPTR parallel mit
   188  c55e 854e               	STA NEWPTR	; BUFPTR mitziehen ...
   189  c560 a534               	LDA STRPTR+1
   190  c562 854f               	STA NEWPTR+1
   191  c564 a64c               	LDX BERANF	; Bereich war zuletzt
   192  c566 a54d               	LDA BERANF+1	; String-Heap-Ende?
   193  c568 e431               	CPX STREND
   194  c56a d004               	BNE +
   195  c56c c532               	CMP STREND+1
   196  c56e f01b               	BEQ EXIT	; ja -> fertig
   197                          
   198  c570 865d               +	STX BEREND	; um Pufferlänge - 256
   199  c572 855e               	STA BEREND+1	; nach unten verlegen.
   200  c574 38                 	SEC		
   201  c575 e91f               	SBC #(>BUFSIZE-1) ; Bereichslänge in Pages,
   202                          			; kann um 254 Bytes überragt werden!
   203  c577 9008               	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
   204  c579 854d               	STA BERANF+1
   205  c57b e431               	CPX STREND	; Ende des String-Heaps erreicht?
   206  c57d e532               	SBC STREND+1
   207  c57f b021               	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
   208                          LASTRANGE
   209  c581 a531               	LDA STREND	; Bereichanfang =
   210  c583 a632               	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
   211  c585 854c               	STA BERANF	; 
   212  c587 864d               	STX BERANF+1	; 
   213  c589 d017               	BNE STRINRANGE	; immer, weil High-Byte >0
   214                          
   215                          
   216                          ; *** Ende der Garbage Collection
   217                          
   218                          EXIT
   219  c58b a207               	LDX #ZPLEN
   220  c58d bd01c7             RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
   221  c590 954b               	STA ZPSTART-1,X	; restaurieren.
   222  c592 ca                 	DEX
   223  c593 d0f8               	BNE RESLOOP
   224                          
   225  c595 ad00c7             	LDA ORIGVID	; Kontrollanzeige löschen
   226  c598 8de707             	STA MARKEVID	; und alten Zustand wieder
   227  c59b ad01c7             	LDA ORIGCOL	; herstellen.
   228  c59e 8de7db             	STA MARKECOL
   229                          !ifdef debug {
   230                          	JSR gra_off
   231                          }
   232  c5a1 60                 	RTS
   233                          
   234                          
   235                          ; *** Bereich durchgehen
   236                          
   237                          STRINRANGE
   238                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   239                          	LDA #>(BUF+BUFSIZE)
   240                          	STA BUFPTR+1
   241                          	LDA #<(BUF+BUFSIZE)
   242                          	STA BUFPTR
   243                          } else {
   244                          			; Sonderfall Pufferende bei $FFFF
   245  c5a2 a900               	LDA #0		; Buffer-Pointer auf
   246  c5a4 855f               	STA BUFPTR	; $10000 (65536) = 0
   247  c5a6 8560               	STA BUFPTR+1	; setzen.
   248                          }
   249  c5a8 38                 	SEC
   250  c5a9 24                 	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
   251                          NEXTSTR	
   252  c5aa 18                 	CLC
   253                          NEXTSTR1
   254  c5ab 2019c6             	JSR GETSA	; Nächste String-Adresse holen.
   255  c5ae f03b               	BEQ COPYBACK	; keinen String mehr gefunden!
   256                          
   257  c5b0 98                 	TYA		; high Byte
   258  c5b1 e45d               	CPX BEREND	; X/A >= BEREND:
   259  c5b3 e55e               	SBC BEREND+1	; oberhalb des Bereichs, dann
   260  c5b5 b0f3               	BCS NEXTSTR	; nächster String!
   261                          
   262  c5b7 98                 	TYA		; high Byte
   263  c5b8 e44c               	CPX BERANF	; X/A < BERANF:
   264  c5ba e54d               	SBC BERANF+1	; unterhalb des Bereichs, dann
   265  c5bc 90ed               	BCC NEXTSTR1	; nächster String!
   266                          			; Innerhalb des Bereichs:
   267  c5be a55f               	LDA BUFPTR	; Pufferzeiger um
   268  c5c0 e552               	SBC LEN		; String-Länge nach unten
   269  c5c2 855f               	STA BUFPTR	; setzen.
   270  c5c4 b002               	BCS +
   271  c5c6 c660               	DEC BUFPTR+1	; Überlauf High-Byte
   272                          
   273  c5c8 8459               +	STY STRADR+1	; String-Adresse abspeichern
   274  c5ca 8658               	STX STRADR	; für Kopieraktion.
   275                          
   276  c5cc a452               	LDY LEN		; String-Länge (> 0)
   277  c5ce d004               	BNE NBENTRY	; immer, mit Dekrement beginnen!
   278  c5d0 b158               NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
   279  c5d2 915f               	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
   280  c5d4 88                 NBENTRY	DEY		; schreibt ins RAM unters ROM!
   281  c5d5 d0f9               	BNE NEXTBYT
   282                          LEN1
   283  c5d7 b158               	LDA (STRADR),Y	; Das 0. Byte extra
   284  c5d9 915f               	STA (BUFPTR),Y	; übertragen
   285                          
   286  c5db 38                 	SEC		; Neue String-Adresse:
   287  c5dc a54e               	LDA NEWPTR	; Einfach den Pointer
   288  c5de e552               	SBC LEN		; mitziehen, ebenso um
   289  c5e0 854e               	STA NEWPTR	; String-Länge nach unten
   290  c5e2 b002               	BCS +		; setzen.
   291  c5e4 c64f               	DEC NEWPTR+1	; Überlauf High-Byte
   292                          +
   293  c5e6 20f1c6             	JSR CORR	; String-Adresse in Descriptor ändern.
   294                          			; Immmer Z=0,
   295  c5e9 d0bf               	BNE NEXTSTR	; zum nächsten String.
   296                          
   297                          
   298                          ; *** Pufferinhalt wieder zurück auf String-Heap
   299                          
   300                          ; 0 ------------------------------------------- FFFF	
   301                          ;        Ziel                        Quelle
   302                          ;          +--------------------------+
   303                          ;          |                          |
   304                          ;          V                         /^\
   305                          ;     |||||||||||                |||||||||||
   306                          ;     ^          ^               ^          ^ 
   307                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   308                          
   309                          COPYBACK
   310                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   311                          	LDA BUFPTR	; Puffer leer ...
   312                          	CMP #<(BUF+BUFSIZE)
   313                          	BNE +
   314                          	LDA BUFPTR+1	; Wenn Pointer am Ende ...
   315                          	CMP #>(BUF+BUFSIZE)
   316                          	BEQ NOCOPY	; ist der Puffer leer, ev. nächster
   317                          +			; Bereich ...
   318                          } else {
   319                          			; Sonderfall: Pufferende bei $FFFF
   320  c5eb a55f               	LDA BUFPTR	; Puffer leer,
   321  c5ed 0560               	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
   322  c5ef f025               	BEQ NOCOPY	; War es letzter Bereich?
   323                          }
   324                          
   325  c5f1 a533               	LDA STRPTR
   326  c5f3 8558               	STA $58		; Zielblockende+1
   327  c5f5 a534               	LDA STRPTR+1
   328  c5f7 8559               	STA $59
   329  c5f9 a54e               	LDA NEWPTR
   330  c5fb 8533               	STA STRPTR	; neues FRETOP
   331  c5fd a54f               	LDA NEWPTR+1	
   332  c5ff 8534               	STA STRPTR+1
   333                          
   334                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   335                          	LDA #<(BUF+BUFSIZE)
   336                          	STA $5A
   337                          	LDA #>(BUF+BUFSIZE)
   338                          	STA $5B
   339                          } else {
   340                          			; Sonderfall Pufferende bei $FFFF
   341  c601 a900               	LDA #$00	; Quellblockende+1
   342  c603 855a               	STA $5A
   343  c605 855b               	STA $5B
   344                          }
   345                          			; Quellbockanfang = BUFPTR
   346                          
   347  c607 78                 	SEI		; keine Interrupts zulassen, wegen
   348  c608 a501               	LDA PROZPORT	; KERNAL-ROM wegblenden
   349  c60a 48                 	PHA		; damit das Puffer-RAM zugänglich
   350  c60b a935               	LDA #MEMRAM	; wird!
   351  c60d 8501               	STA PROZPORT
   352                          
   353  c60f 20bfa3             	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
   354                          			; Z=1
   355  c612 68                 	PLA		; ursprünglicher Zustand
   356  c613 8501               	STA PROZPORT	; KERNAL-ROM wieder aktivieren
   357  c615 58                 	CLI
   358                          NOCOPY
   359  c616 4c5cc5             	JMP NEXTBLOCK	; nächsten Bereich
   360                          
   361                          
   362                          ;
   363                          ; *** Get String - nächsten String mit Länge ungleich 0
   364                          ;
   365                          ; ( C-Flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-Flag )
   366                          ;
   367                          ; Bei C=1 wird beim SDS gestartet, sonst von der letzten
   368                          ; Position gemäß STRDP und String-Status STAT.
   369                          ; Das Z-Flag ist gesetzt, wenn kein String mehr
   370                          ; vorhanden ist, sonst in X/Y die Adresse und in LEN
   371                          ; die Stringlänge.
   372                          
   373  c619 9063               GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
   374                          			; sonst Start bei SDS ...
   375                          
   376                          ; *** String-Descriptor-Stack (SDS): TOSS bis TSSP
   377                          ;
   378                          ;    +-------------+
   379                          ;    |             V
   380                          ;    |    belegt->|<-frei
   381                          ;   +-+     +-----+-----+-----+
   382                          ;   | |     |S|L|H|S|L|H|S|L|H|
   383                          ;   +-+     +-----+-----+-----+
   384                          ;    ^       ^     ^     ^     ^
   385                          ;    $16     $19   $1C   $1F   $22
   386                          ;    TSSP    TOSS
   387                          
   388                          DESCSTACK
   389  c61b a000               	LDY #0
   390  c61d 8423               	STY STRDP+1	; Descriptor auf
   391  c61f a905               	LDA #STAT_SDS	; Status: SDS
   392  c621 8557               	STA STAT
   393  c623 a219               	LDX #TOSS	; SDS Start
   394  c625 d005               	BNE ISDSTEND	; immer verzweigen
   395  c627 a622               DSTACK	LDX STRDP
   396  c629 e8                 NEXTDST	INX		; nächster Descriptor
   397  c62a e8                 	INX
   398  c62b e8                 	INX
   399                          ISDSTEND
   400  c62c e416               	CPX TSSP	; Stack durch?
   401  c62e f010               	BEQ VARS
   402  c630 b500               	LDA 0,X		; String-Länge
   403  c632 f0f5               	BEQ NEXTDST
   404  c634 8552               	STA LEN		; Rückgabevariable
   405  c636 8622               	STX STRDP	; festhalten
   406  c638 b502               	LDA 2,X		; String-Adr. high
   407  c63a a8                 	TAY
   408  c63b b501               	LDA 1,X		; String-Adr. low
   409  c63d aa                 	TAX
   410  c63e 98                 	TYA		; immer ungleich 0, Z=0
   411  c63f 60                 	RTS		; Adresse in X/Y retour
   412                          
   413                          ; *** Variablen: VARTAB bis ARYTAB
   414                          
   415  c640 a52d               VARS	LDA VARTAB	; Variablenanfang
   416  c642 a62e               	LDX VARTAB+1
   417  c644 8522               	STA STRDP
   418  c646 8623               	STX STRDP+1
   419  c648 a003               	LDY #STAT_VAR	; Status: einfache Variablen
   420  c64a 8457               	STY STAT
   421  c64c d00b               	BNE ISVAREND	; immer verzweigen
   422                          VAR
   423  c64e 18                 NEXTVAR	CLC		; nächste Variable
   424  c64f a522               	LDA STRDP
   425  c651 6907               	ADC #7		; Variablenlänge
   426  c653 8522               	STA STRDP
   427  c655 9002               	BCC ISVAREND
   428  c657 e623               	INC STRDP+1	; Überlauf High-Byte
   429                          ISVAREND
   430  c659 c52f               	CMP ARYTAB
   431  c65b d006               	BNE CHECKVAR
   432  c65d a623               	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
   433  c65f e430               	CPX ARYTAB+1
   434  c661 f026               	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
   435                          CHECKVAR
   436  c663 a000               	LDY #0		; Variablenname
   437  c665 b122               	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
   438  c667 30e5               	BMI NEXTVAR	; kein String, nächste V.
   439  c669 c8                 	INY
   440  c66a b122               	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
   441  c66c 10e0               	BPL NEXTVAR	; kein String, nächste V.
   442  c66e c8                 	INY
   443  c66f b122               	LDA (STRDP),Y	; String-Länge
   444  c671 f0db               	BEQ NEXTVAR	; = 0, Nächste Variable
   445  c673 8552               	STA LEN		; Rückgabevariable
   446  c675 c8                 	INY
   447  c676 b122               	LDA (STRDP),Y	; String-Adresse low
   448  c678 aa                 	TAX
   449  c679 c8                 	INY
   450  c67a b122               	LDA (STRDP),Y	; String-Adresse high
   451  c67c a8                 	TAY		; immer ungleich 0, Z=0
   452  c67d 60                 	RTS		; Adresse in X/Y retour
   453                          
   454                          CHECKTYPE
   455  c67e a557               	LDA STAT	; GETSA-Einstieg mit C=0
   456  c680 c903               	CMP #STAT_VAR	; String-Status?
   457  c682 9042               	BCC ARRAY	; =1 -> Arrays
   458  c684 f0c8               	BEQ VAR		; =3 -> Variablen
   459  c686 4c27c6             	JMP DSTACK	; =5 -> String-Desc.-Stack
   460                          
   461                          ; *** Arrays: ARYTAB bis STREND
   462                          
   463  c689 8550               ARRAYS	STA PTR		; A/X von Variablendurchlauf
   464  c68b 8651               	STX PTR+1	; Start Array-Array-Bereich
   465  c68d a001               	LDY #STAT_ARY
   466  c68f 8457               	STY STAT	; Status: Arrays
   467                          ISARREND
   468  c691 a550               	LDA PTR
   469  c693 a651               	LDX PTR+1
   470  c695 e432               CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
   471  c697 d004                       BNE NEXTARR	; erreicht?
   472  c699 c531               	CMP STREND
   473  c69b f04f               	BEQ NOSTRING	; Arrays fertig -> kein String
   474                          NEXTARR
   475  c69d 8522               	STA STRDP	; immer C=0
   476  c69f 8623               	STX STRDP+1
   477  c6a1 a000               	LDY #0
   478  c6a3 b122               	LDA (STRDP),Y	; Array-Name
   479  c6a5 aa                 	TAX		; Var.-Typ merken
   480  c6a6 c8                 	INY
   481  c6a7 b122               	LDA (STRDP),Y
   482  c6a9 08                 	PHP		; Var.-Typ merken
   483  c6aa c8                 	INY
   484  c6ab b122               	LDA (STRDP),Y	; Offset nächstes Array
   485  c6ad 6550               	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
   486  c6af 8550               	STA PTR		; Start Folge-Array
   487  c6b1 c8                 	INY
   488  c6b2 b122               	LDA (STRDP),Y
   489  c6b4 6551               	ADC PTR+1
   490  c6b6 8551               	STA PTR+1
   491  c6b8 28                 	PLP		; Var.-Typ holen
   492  c6b9 10d6               	BPL ISARREND	; kein String-Array
   493  c6bb 8a                 	TXA		; Var.-Typ holen
   494  c6bc 30d3               	BMI ISARREND	; kein String-Array
   495  c6be c8                 	INY
   496  c6bf b122               	LDA (STRDP),Y	; Anzahl der Dimensionen
   497  c6c1 0a                 	ASL		; *2
   498  c6c2 6905               	ADC #5		; Offset = Dimensionen*2+5
   499                          			; C=0 solange Dim.. <= 125
   500  c6c4 d003               	BNE ADVDESC	; immer verzweigen
   501                          ARRAY			; Einstieg bei Fortsetzung
   502                          NEXTASTR
   503  c6c6 18                 	CLC
   504  c6c7 a903               	LDA #3		; String-Descriptor-Länge
   505  c6c9 6522               ADVDESC	ADC STRDP	; nächten String
   506  c6cb 8522               	STA STRDP
   507  c6cd 9002               	BCC +
   508  c6cf e623               	INC STRDP+1	; Überlauf High-Byte
   509  c6d1 c550               +	CMP PTR		; Array durch?
   510  c6d3 d006               	BNE IS0ASTR
   511  c6d5 a623               	LDX STRDP+1
   512  c6d7 e451               	CPX PTR+1
   513  c6d9 f0ba               	BEQ CHKAEND	; A/X = PTR, Array-Ende prüfen
   514                          IS0ASTR
   515  c6db a000               	LDY #0
   516  c6dd b122               	LDA (STRDP),Y	; String-Länge
   517  c6df f0e5               	BEQ NEXTASTR	; weiter im Array
   518  c6e1 8552               	STA LEN		; Rückgabevariable
   519  c6e3 c8                 	INY
   520  c6e4 b122               	LDA (STRDP),Y	; String-Adresse low
   521  c6e6 aa                 	TAX
   522  c6e7 c8                 	INY
   523  c6e8 b122               	LDA (STRDP),Y	; String-Adresse high
   524  c6ea a8                 	TAY		; immer ungleich 0, Z=0
   525  c6eb 60                 	RTS		; Adresse in X/Y retour
   526                          
   527                          NOSTRING
   528  c6ec a900               	LDA #0		; Länge 0 
   529  c6ee 8552               	STA LEN		; kein String gefunden
   530  c6f0 60                 	RTS		; Z=1
   531                          
   532                          ;
   533                          ; CORR - String-Adresse im Descriptor korrigieren
   534                          ;
   535                          ; ( STRADR, STAT -> )
   536                          ;
   537  c6f1 a557               CORR	LDA STAT	; String-Status
   538  c6f3 2903               	AND #%011	; nur 2 Bits
   539  c6f5 a8                 	TAY		; Lage des Descriptors
   540  c6f6 a54e               	LDA NEWPTR	;
   541  c6f8 9122               	STA (STRDP),Y	; ... bei SDS
   542  c6fa c8                 	INY		; ... und Array verschieden!
   543  c6fb a54f               	LDA NEWPTR+1
   544  c6fd 9122               	STA (STRDP),Y
   545  c6ff 60                 	RTS
   546                          
   547  c700 00                 ORIGVID !byte 0		; original Video der Markenposition
   548  c701 00                 ORIGCOL !byte 0		; original Farbe der Markenposition
   549  c702 00                 SAVE	!byte 0		; gesicherte ZP-Variablen
   550                          *=*+ZPLEN-1
   551                          
   552                          
