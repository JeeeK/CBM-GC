
; ******** Source: jk-gc.asm
     1                          *= $C500
     2                          ;
     3                          ;.OPT LIST,NOSYM
     4                          ;
     5                          ; *************************
     6                          ; *  GARBAGE  COLLECTION  *
     7                          ; *   von Johann Klasek   *
     8                          ; *   j AT klasek DOT at  *
     9                          ; * 1985-12-27 VERS. 1.1  *
    10                          ; * 2013-11-24 VERS. 2.0  *
    11                          ; * 2019-02-15 VERS. 2.1  *
    12                          ; * 2020-10-08 VERS. 2.2  *
    13                          ; *************************
    14                          ;
    15                          ; Aufruf: SYS ...
    16                          ; Räumt Stringspeicher auf
    17                          ; Es werden nur jene Speicherstellen
    18                          ; benutzt, die auch die normale
    19                          ; GC verwendet; alle anderen
    20                          ; werden wieder restauriert.
    21                          
    22                          
    23                          ; Optionen:
    24                          
    25                          ; Entweder als Patch des BASIC-ROMs in der RAM-Kopie von $A000 bis $BFFF
    26                          ; oder GC via IRQ anspringen (auskommentieren):
    27                          ;basic_patch = 1
    28                          ;
    29                          ;	Ad Platzverbrauch: 
    30                          ;	 * Der normal Hook in BASIC-RAM-Kopie braucht 48 Bytes.
    31                          ;	 * Der IRQ-Hook ist sehr aufwendig und braucht 244 Bytes, weil
    32                          ;	   hier auch noch 60 Bytes für die Blockkopierroutine
    33                          ;	   benötigt wird, da hier die ROM-Variante nicht genutzt
    34                          ; 	   werden kann!
    35                          
    36                          ; Wenn aktiv, wird als Puffer das RAM unter dem BASIC-ROM verwendet,
    37                          ; sonst jenes unter dem KERNAL. So kann etwa das RAM unter dem KERNAL
    38                          ; für Grafikzwecke verwendet werden ...
    39                          ;basic_rom_buffer = 1
    40                          
    41                          ; Original MOVBLOCK-Routine (Kopie aus dem ROM) verwenden (nur
    42                          ; im Fall, wenn basic_patch nicht aktiviert ist!).
    43                          ; Sonst wird ein optimierte Variante verwendet.
    44                          ;orig_movblock = 1
    45                          
    46                          
    47                          
    48                          
    49                          ; BASIC Systemvariablen
    50                          
    51                          TOSS     = $19		; Top of Stringstack
    52                          EOSS     = $22		; End of Stringstack +1
    53                          TSSP     = $16		; Temp. Stringstack Pointer
    54                          
    55                          VARTAB   = $2D		; BASIC-Programmende = Variablenanfang
    56                          ARYTAB   = $2F		; Variablenende = Arraybereichanfang
    57                          STREND   = $31		; Arraybereichende = unterste String-Heap-Adresse
    58                          FRETOP   = $33		; aktuelle String-Heap-Adresse
    59                          MEMSIZ   = $37		; höchste RAM-Adresse für BASIC, Start
    60                          			; des nach unten wachsenden String-Heaps
    61                          MEMBEG   = STREND	; Memory begin = STREND
    62                          MEMEND   = MEMSIZ	; Memory end
    63                          
    64                          V_IRQ    = $0314	; IRQ-Vektor, 2 Bytes
    65                          
    66                          ; Variablen
    67                          
    68                          STRPTR   = FRETOP	; String-Pointer = FRETOP
    69                          STRDP    = $22		; String-Descriptor-Address
    70                          BERANF   = $4C		; Bereichsanfang
    71                          NEWPTR	 = $4E		; Neuer String-Pointer
    72                          PTR      = $50		; Array-Pointer
    73                          LEN      = $52		; String-Length
    74                          ; $54-$56 belegt
    75                          STAT     = $57		; String-State
    76                          ; $58-5B wird von MOVBLOCK zerstört!
    77                          STRADR   = $58		; String-Address (temp.)
    78                          BEREND   = $5D		; Bereichsende
    79                          BUFPTR   = $5F		; Buffer-Pointer
    80                          			; (MOVBLOCK: Quellblockanfang!)
    81                          
    82                          CPTR     = $22		; Pointer für Install-Routine
    83                          
    84                          ; zu rettender Zeropage-Bereich
    85                          ZPSTART  = $4C		; 1. zu rettende
    86                          ZPEND    = $52		; letzte zu rettende Stelle
    87                          ZPLEN    = ZPEND-ZPSTART+1
    88                          			; Anzahl zu rettenden Bytes
    89                          
    90                          ; Konstanten
    91                          
    92                          ; für Variabe STAT (String State):
    93                          STAT_SDS = 5		; String-Descriptor-Stack
    94                          STAT_VAR = 3		; einfache Variablen
    95                          STAT_ARY = 1		; Array
    96                          
    97                          
    98                          ; Speicherorte
    99                          
   100                          GARBCOL  = $B526	; Einsprungpunkt der GC
   101                          
   102                          BASIC    = $A000        ; BASIC-ROM
   103                          KERNAL   = $E000        ; KERNAL-ROM
   104                          ROMSIZE  = $2000        ; ROM-Länge 8 KByte8
   105                          
   106                          ; Puffer
   107                          !ifndef basic_rom_buffer {
   108                          BUF	 = KERNAL	; Puffer unter KERNAL
   109                          } else {
   110                          BUF	 = BASIC	; Puffer unter KERNAL
   111                          }
   112                          BUFSIZE  = ROMSIZE	; Puffergröße
   113                          
   114                          ; I/O-BEREICHE/-ADRESSEN
   115                          
   116                          VIDBASE  = $0400	; Video RAM
   117                          COLBASE  = $D800	; Color RAM
   118                          
   119                          MARKE    = "*"		; Aktivitätsanzeige
   120                          MARKEFARBE = 9		; rot
   121                          MARKEOFF = 40*25-1	; Markenposition
   122                          MARKEVID = VIDBASE+MARKEOFF
   123                          MARKECOL = COLBASE+MARKEOFF
   124                          
   125                          PROZPORT = $01		; Prozessorport
   126                          MEMROM   = %00110111	; Basic+Kernal ROM, $37
   127                          MEMBAS   = %00110110	; Basic RAM+Kernal ROM, $34
   128                          MEMRAM   = %00110101	; Basic+Kernal RAM, $35
   129                          
   130                          
   131                          ; Debugging
   132                          
   133                          ;!set debug=1
   134                          
   135                          
   136                          ; Installer
   137                          
   138                          INSTALL
   139                          
   140                          !ifdef basic_patch {
   141                          
   142                          MOVBLOCK = $A3BF	; Block verschieben aus BASIC-ROM
   143                          			; zerstört $58/$59/$5A/$5B/$22
   144                          
   145                          	; BASIC ins RAM kopieren, um die GC-Routine
   146                          	; zu patchen ...
   147  c500 a937               	LDA #MEMROM
   148  c502 8501               	STA PROZPORT	; alles ROM (also vom ROM kopieren)
   149  c504 a000               	LDY #<BASIC	; ROM-Beginn
   150  c506 8422               	STY CPTR
   151  c508 a9a0               	LDA #>BASIC
   152  c50a 8523               	STA CPTR+1	; BASIC-ROM Anfang
   153  c50c a220               	LDX #>($2000)	; BASIC-ROM Länge in Pages
   154  c50e b122               CPYROM	LDA (CPTR),Y	; ROM lesen
   155  c510 9122               	STA (CPTR),Y	; RAM schreiben
   156  c512 c8                 	INY
   157  c513 d0f9               	BNE CPYROM
   158  c515 e623               	INC CPTR+1	; nächste Page
   159  c517 ca                 	DEX		; Page-Zähler
   160  c518 d0f4               	BNE CPYROM
   161  c51a a501               	LDA PROZPORT	; auf RAM umschalten
   162  c51c 29fe               	AND #%11111110	; "BASIC-ROM aus"-Maske
   163  c51e 8501               	STA PROZPORT
   164  c520 a930               	LDA #<COLLECT	; "JMP COLLECT"
   165  c522 8d27b5             	STA GARBCOL+1	; patchen ...
   166  c525 a9c5               	LDA #>COLLECT
   167  c527 8d28b5             	STA GARBCOL+2
   168  c52a a94c               	LDA #$4C	; JMP-Opcode
   169  c52c 8d26b5             	STA GARBCOL
   170  c52f 60                 	RTS
   171                          } else {
   172                          	SEI
   173                          	LDA V_IRQ	; bisherige IRQ-Routine aufheben
   174                          	LDX V_IRQ+1
   175                          	CMP #<(IRQ)
   176                          	BNE HOOK
   177                          	CPX #>(IRQ)
   178                          	BEQ INSTEXIT	; Vektor bereits installiert
   179                          HOOK
   180                          	STA ORIGIRQ
   181                          	STX ORIGIRQ+1
   182                          	LDA #<(IRQ)	; GC-Routine einhängen ...
   183                          	LDX #>(IRQ)
   184                          	STA V_IRQ
   185                          	STX V_IRQ+1
   186                          INSTEXIT
   187                          	CLI
   188                          	RTS
   189                          
   190                          ; In der IRQ-Routine wird festgestellt, ob die alte GC
   191                          ; gerade aktiv ist, was dann der Fall ist, wenn
   192                          ;   * am Stack der Caller-PC der Open-Space-for-Memory-Routine ($B62B-1) ist,
   193                          ;   * am Stack einer der drei Caller-PC der Search-for-Next-String-Routine
   194                          ;     ist,
   195                          ;   * der PC im Bereich von $B526 bis $B63D liegt.
   196                          ; Gewisse Zustände müssen dabei allerings korrigiert werden.
   197                          ; Korrekturfälle:
   198                          ;   1. Wenn der PC im Bereich von GC_CRIT_START bis GC_CRIT_END liegt,
   199                          ;      ist der Descriptor inkonsistent -> High-Byte setzen:
   200                          ;         LDY $55
   201                          ;         INY
   202                          ;         INY
   203                          ;         LDA $59
   204                          ;         STA ($4E),y
   205                          ;   2. Wenn der PC im Bereich von GC_PHP_START bis GC_PHP_END liegt,
   206                          ;      ist der Stack inkonsistent -> ein Byte vom Stack nehmen.
   207                          ;   3. Wenn die Subroutine "Open Space in Memory" unterbrochen
   208                          ;      wurde (an der Aufruferadresse erkannt), dann muss der
   209                          ;      gerade kopierte String fertig übertragen werden, daher
   210                          ;      kehrt das RTI wieder zur Routine zurück und erst mit
   211                          ;      dem RTS wird über die manipulierte Adresse am Stack
   212                          ;      zur neuen GC gesprungen.
   213                          ;   4. Wenn in Subroutine "Search for Next String" unterbrochen
   214                          ;      wurde (an den drei möglichen Aufrufadressen erkannt),
   215                          ;      befindet sich am Stack die Adresse des Aufrufers, wobei
   216                          ;      am Stack die RTI-Rückkehradresse auf eine Korrektur-
   217                          ;      routine gelenkt wird, die 2 Bytes vom Stack nimmt und
   218                          ;      zur neuen GC verzweigt.
   219                          ;
   220                          ; Sonst kann, wenn der Unterbrechungs-PC im GC-Code-Bereich liegt,
   221                          ; direkt zu neuen GC verzweigt werden.
   222                          ;
   223                          ; Egal wo die GC unterbrochen wird, Top of String-Heap ($33/$34)
   224                          ; ist zwar bereits zerstört, aber das ist egal, weil
   225                          ; die neue GC diesen ohnehin neu ermittelt.
   226                          ; 
   227                          
   228                          GC_START      = $B526	; PC in diesem Bereich -> GC aktiv
   229                          GC_END        = $B63C
   230                          GC_CRIT_START = $B632	; PC in diesem Bereich -> 
   231                          			; Descriptor muss korrigiert werden!
   232                          GC_CRIT_END   = $B638
   233                          GC_PHP_START  = $B58A	; PC in diesem Bereich -> 
   234                          			; SR (von PHP) vom Stack nehmen!
   235                          GC_PHP_END    = $B598	; hier ist das PLP
   236                          
   237                          CALLER_OPSP   = $B628+2	; PC-1 der Return-Adresse des JSR $A3BF
   238                          			; Aufrufs (Open Space for Memory) bei $B628.
   239                          CALLER_SNS1   = $B561+2 ; PC-1 Return-Adressen der Aufrufstellen
   240                          CALLER_SNS2   = $B548+2 ; der "Search for Next String"-Routine
   241                          CALLER_SNS3   = $B5B8+2
   242                          
   243                          IRQ
   244                          	SEI
   245                          
   246                          	; IRQ-Stackframe:
   247                          	; $104,X Status-Reg
   248                          	; $105,X Low-PC
   249                          	; $106,X High-PC
   250                          	; ev. aufrufene Routine, falls in einer Subroutine:
   251                          	; $107,X Low-Byte des (Aufruf-PC)-1
   252                          	; $108,X High-Byte des (Aufruf-PC)-1
   253                          
   254                          	LDA $108,X	; Aufrufer PC high
   255                          	TAY
   256                          	LDA $107,X	; Aufrufer PC low
   257                          	; Aufruf-PC-1 in A/Y
   258                          			; sind wir in "Open Space"?
   259                          	CMP #<(CALLER_OPSP)
   260                          	BNE CHK_SNS
   261                          	CPY #>(CALLER_OPSP)
   262                          	BNE CHK_SNS
   263                          IN_OPSP
   264                          	; mit RTI zurück in die unterbrochene
   265                          	; Kopierroutine, damit der String
   266                          	; fertig übertragen wird, aber mit
   267                          	; dem RTS dort, geht es dann zu
   268                          	; Descriptor-Korrekturroutine
   269                          	; die dann endlich die neue CG aufrufen
   270                          	; darf.
   271                          	LDA #<(CORR_STR-1)
   272                          	STA $107,X	; wegen RTS minus 1
   273                          	LDA #>(CORR_STR-1)
   274                          	STA $108,X	; immer >0
   275                          	BNE CONT	; immer, mit RTI zurück
   276                          
   277                          CORR_STR
   278                          	LDY $55		; Offset des Descriptors
   279                          	INY		; String-Länge
   280                          	LDA $58		; String-Adresse low
   281                          	STA ($4E),Y	; in den Descriptor
   282                          	INY
   283                          	LDA $59		; String-Adresse high
   284                          	STA ($4E),Y	; nun String-Adresse high setzen
   285                          	BNE START_COLLECT
   286                          			; immer, weil immer >0
   287                          
   288                          	
   289                          CHK_SNS			; sind wir in "Search for Next String"?
   290                          	CPY #>(CALLER_SNS1)
   291                          			; High-Byte ist für alle drei Adressen gleich!
   292                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   293                          	  !error "High-Byte von CALLER_SNS* sind verschieden. Sie müssen gleich sein!"
   294                          	}
   295                          	BNE CHK_PC
   296                          	CMP #<(CALLER_SNS1)
   297                          	BEQ IN_SUB
   298                          	CMP #<(CALLER_SNS2)
   299                          	BEQ IN_SUB
   300                          	CMP #<(CALLER_SNS3)
   301                          	BNE CHK_PC
   302                          
   303                          IN_SUB
   304                          	LDA #<(SKIPSUB) ; Unterbrechungs-PC umbiegen ...
   305                          	STA $105,X	; Low-Byte
   306                          	LDA #>(SKIPSUB)
   307                          	STA $106,X	; High-Byte
   308                          			; mit dem RTI geht es zu SKIPSUB wo
   309                          			; der Caller vom Stack genommen wird.
   310                          
   311                          CONT	JMP (ORIGIRQ)	; weiter zum Original-Code (Kette).
   312                          
   313                          CHK_PC
   314                          	LDA $106,X	; Unterbrechungs-PC
   315                          	TAY		; High-Byte
   316                          	LDA $105,X	; Low-Byte
   317                          	CPY #>(GC_START)
   318                          	BCC CONT	; vor der GC
   319                          	BNE +		; über GC-Anfang
   320                          	CMP #<(GC_START)
   321                          	BCC CONT	; unterhalb GC
   322                          +	CPY #>(GC_END+1)
   323                          	BCC ++		; in der GC!
   324                          	BNE CONT	; über der GC
   325                          	CMP #<(GC_END+1)
   326                          	BCS CONT	; über der GC
   327                          ++
   328                          	; Die GC wurde unterbrochen!
   329                          	; In irgendwelchen Spezialbereichen, wo wir mehr machen müssen?
   330                          
   331                          	; in PHP/PLP Sektion?
   332                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   333                          	  !error "High-Byte von GC_PHP_START und GC_PHP_END sind nicht gleich!"
   334                          	}
   335                          	CPY #>(GC_PHP_START)
   336                          	BNE +		; nicht in richter Page, nächster Bereichstest
   337                          	CMP #<(GC_PHP_START)
   338                          	BCC +		; unterhalb, keine Stack-Korrektur
   339                          	CMP #<(GC_PHP_END+1)
   340                          	BCS +		; darüber, keine Stack-Korrektur
   341                          	; Stack-Korrektur:
   342                          	PLA		; SR vom Stack nehmen, setzt ev. Flags N,Z aber nicht C
   343                          	BCC TO_COLLECT	; C immer 0, mittels RTI zu COLLECT
   344                          +
   345                          	; in kritischer Sektion?
   346                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   347                          	  !error "High-Byte von GC_CRIT_START und GC_CRIT_END sind nicht gleich!"
   348                          	}
   349                          	CPY #>(GC_CRIT_START)
   350                          	BNE +		; nicht in richtiger Page, dann normal weiter
   351                          	CMP #<(GC_CRIT_START)
   352                          	BCC +		; unterhalb, keine Korrektur
   353                          	CMP #<(GC_CRIT_END+1)
   354                          	BCS +		; darüber, keine Korrektur
   355                          
   356                          	; Korrektur, High Byte der String-Adresse in Descriptor setzen,
   357                          	; weil bereits das Low-Byte gesetzt wurde. Die Adresse im Descriptor
   358                          	; wäre sonst inkonsistent!
   359                          	LDY $55		; Offset des Descriptors
   360                          	INY		; String-Länge
   361                          	INY		; String-Adresse-Low (ist schon gesetzt!)
   362                          	LDA $59
   363                          	STA ($4E),Y	; nun String-Adresse-High setzen
   364                          +
   365                          	; mittels RTI COLLECT aufrufen:
   366                          TO_COLLECT
   367                          	LDA #<(START_COLLECT)
   368                          			; Rückkehradresse für RTI am Stack umbiegen ...
   369                          	STA $0105,X	; Low-Byte
   370                          	LDA #>(START_COLLECT)
   371                          	STA $0106,X	; High-Byte
   372                          	BNE CONT	; IRQ behandeln, RTI startet dann die neue GC ...
   373                          
   374                          SKIPSUB			; Open-Space- oder Search-for-Next-String-Routine abgebrochen:
   375                          	PLA		; kommt direkt von RTI, Aufruf-PC (für RTS) in alter GC
   376                          	PLA		; verwerfen und die neue GC übernimmt ...
   377                          			; direkt durch zu COLLECT
   378                          START_COLLECT
   379                          	LDA #3
   380                          	STA $53		; Step-Size für nächsten Descriptor auf
   381                          			; Ausgangswert setzen (wird von alter GC
   382                          			; nicht initialisiert!)
   383                          }
   384                          
   385                          ; *** Garbage Collector
   386                          
   387                          COLLECT
   388                          !ifdef debug {
   389                          	JSR gra_on
   390                          }
   391  c530 ade707             	LDA MARKEVID	; Kontrollanzeige Bildschirm
   392  c533 8dfcc6             	STA ORIGVID
   393  c536 a92a               	LDA #MARKE
   394  c538 8de707             	STA MARKEVID	; Marke: Zeichen
   395  c53b ade7db             	LDA MARKECOL	; sichern
   396  c53e 8dfdc6             	STA ORIGCOL
   397  c541 a909               	LDA #MARKEFARBE
   398  c543 8de7db             	STA MARKECOL	; Marke: Farbe sichern
   399                          
   400  c546 a207               	LDX #ZPLEN	; Zeropage-Reg.
   401  c548 b54b               SAVLOOP	LDA ZPSTART-1,X	; retten
   402  c54a 9dfdc6             	STA SAVE-1,X
   403  c54d ca                 	DEX
   404  c54e d0f8               	BNE SAVLOOP
   405                          
   406  c550 a537               	LDA MEMEND	; String-Pointer
   407  c552 a638               	LDX MEMEND+1	; und Bereichanfang 
   408  c554 8533               	STA STRPTR	; auf Speicherende
   409  c556 8634               	STX STRPTR+1	; setzen.
   410  c558 854c               	STA BERANF
   411  c55a 864d               	STX BERANF+1
   412                          
   413                          ; *** Nächster zu betrachtender Bereich am String-Heap
   414                          
   415                          ;                        STRADR
   416                          ;       +-------------------------------------+
   417                          ;       |                                     |
   418                          ;       |                                     V
   419                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   420                          ;   |L|PTR|      |      noch nicht       | gesuchte | frei | behandelte |
   421                          ;   | |   |      |  behandelte Strings   | Strings  |      |   Strings  |
   422                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   423                          ;    ^            ^                       ^          ^      ^            ^
   424                          ;    |            |                       |          |      |            |
   425                          ;    STRDP        STREND                  BERANF     BEREND STRPTR       MEMSIZ
   426                          ;                                                           =FRETOP
   427                          ;   SDS,VAR,ARY  |<-------------------- String-Heap -------------------->|
   428                          ;
   429                          ; Der Bereich BERANF bis BEREND (gesuchte Strings) ist immer um 256 Bytes 
   430                          ; kleiner als der Pufferbereich, da am Ende des Bereichs ein String beginnen
   431                          ; könnte, der max. 254 Bytes das Bereichsende überragen könnte. Dieser 
   432                          ; "Überhang" muss im Puffer Platz haben und dort reserviert sein!
   433                          
   434                          NEXTBLOCK
   435  c55c a533               	LDA STRPTR	; NEWPTR parallel mit
   436  c55e 854e               	STA NEWPTR	; BUFPTR mitziehen ...
   437  c560 a534               	LDA STRPTR+1
   438  c562 854f               	STA NEWPTR+1
   439  c564 a64c               	LDX BERANF	; Bereich war zuletzt
   440  c566 a54d               	LDA BERANF+1	; String-Heap-Ende?
   441  c568 e431               	CPX STREND
   442  c56a d004               	BNE +
   443  c56c c532               	CMP STREND+1
   444  c56e f01b               	BEQ EXIT	; ja -> fertig
   445                          +
   446  c570 865d               	STX BEREND	; um Pufferlänge - 256
   447  c572 855e               	STA BEREND+1	; nach unten verlegen.
   448                          	!if <BUFSIZE > 0 {
   449                          	  !error "BUFSIZE ist nicht ein Vielfaches von 256 ($100)!"
   450                          	}
   451  c574 38                 	SEC		
   452  c575 e91f               	SBC #(>BUFSIZE-1) ; Bereichslänge in Pages,
   453                          			; kann um 254 Bytes überragt werden!
   454  c577 9008               	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
   455  c579 854d               	STA BERANF+1
   456  c57b e431               	CPX STREND	; Ende des String-Heaps erreicht?
   457  c57d e532               	SBC STREND+1
   458  c57f b021               	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
   459                          LASTRANGE
   460  c581 a531               	LDA STREND	; Bereichanfang =
   461  c583 a632               	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
   462  c585 854c               	STA BERANF	; 
   463  c587 864d               	STX BERANF+1	; 
   464  c589 d017               	BNE STRINRANGE	; immer, weil High-Byte >0
   465                          
   466                          
   467                          ; *** Ende der Garbage Collection
   468                          
   469                          EXIT
   470  c58b a207               	LDX #ZPLEN
   471  c58d bdfdc6             RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
   472  c590 954b               	STA ZPSTART-1,X	; restaurieren.
   473  c592 ca                 	DEX
   474  c593 d0f8               	BNE RESLOOP
   475                          
   476  c595 adfcc6             	LDA ORIGVID	; Kontrollanzeige löschen
   477  c598 8de707             	STA MARKEVID	; und alten Zustand wieder
   478  c59b adfdc6             	LDA ORIGCOL	; herstellen.
   479  c59e 8de7db             	STA MARKECOL
   480                          !ifdef debug {
   481                          	JSR gra_off
   482                          }
   483  c5a1 60                 	RTS
   484                          
   485                          
   486                          ; *** Bereich durchgehen
   487                          
   488                          STRINRANGE
   489                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   490                          	LDA #>(BUF+BUFSIZE)
   491                          	STA BUFPTR+1
   492                          	LDA #<(BUF+BUFSIZE)
   493                          	STA BUFPTR
   494                          } else {
   495                          			; Sonderfall Pufferende bei $FFFF
   496  c5a2 a900               	LDA #0		; Buffer-Pointer auf
   497  c5a4 855f               	STA BUFPTR	; $10000 (65536) = 0
   498  c5a6 8560               	STA BUFPTR+1	; setzen.
   499                          }
   500  c5a8 38                 	SEC
   501  c5a9 24                 	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
   502                          NEXTSTR	
   503  c5aa 18                 	CLC
   504                          NEXTSTR1
   505  c5ab 2015c6             	JSR GETSA	; Nächste String-Adresse holen.
   506  c5ae f03b               	BEQ COPYBACK	; keinen String mehr gefunden!
   507                          
   508  c5b0 98                 	TYA		; high Byte
   509  c5b1 e45d               	CPX BEREND	; X/A >= BEREND:
   510  c5b3 e55e               	SBC BEREND+1	; oberhalb des Bereichs, dann
   511  c5b5 b0f3               	BCS NEXTSTR	; nächster String!
   512                          
   513  c5b7 98                 	TYA		; high Byte
   514  c5b8 e44c               	CPX BERANF	; X/A < BERANF:
   515  c5ba e54d               	SBC BERANF+1	; unterhalb des Bereichs, dann
   516  c5bc 90ed               	BCC NEXTSTR1	; nächster String!
   517                          			; Innerhalb des Bereichs:
   518  c5be a55f               	LDA BUFPTR	; Pufferzeiger um
   519  c5c0 e552               	SBC LEN		; String-Länge nach unten
   520  c5c2 855f               	STA BUFPTR	; setzen.
   521  c5c4 b002               	BCS +
   522  c5c6 c660               	DEC BUFPTR+1	; Überlauf High-Byte
   523                          
   524  c5c8 8459               +	STY STRADR+1	; String-Adresse abspeichern
   525  c5ca 8658               	STX STRADR	; für Kopieraktion.
   526                          
   527  c5cc a452               	LDY LEN		; String-Länge (> 0)
   528  c5ce d004               	BNE NBENTRY	; immer, mit Dekrement beginnen!
   529  c5d0 b158               NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
   530  c5d2 915f               	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
   531  c5d4 88                 NBENTRY	DEY		; schreibt ins RAM unters ROM!
   532  c5d5 d0f9               	BNE NEXTBYT
   533                          LEN1
   534  c5d7 b158               	LDA (STRADR),Y	; Das 0. Byte extra
   535  c5d9 915f               	STA (BUFPTR),Y	; übertragen
   536                          
   537  c5db 38                 	SEC		; Neue String-Adresse:
   538  c5dc a54e               	LDA NEWPTR	; Einfach den Pointer
   539  c5de e552               	SBC LEN		; mitziehen, ebenso um
   540  c5e0 854e               	STA NEWPTR	; String-Länge nach unten
   541  c5e2 b002               	BCS +		; setzen.
   542  c5e4 c64f               	DEC NEWPTR+1	; Überlauf High-Byte
   543                          +
   544  c5e6 20edc6             	JSR CORR	; String-Adresse in Descriptor ändern.
   545                          			; Immmer Z=0,
   546  c5e9 d0bf               	BNE NEXTSTR	; zum nächsten String.
   547                          
   548                          
   549                          ; *** Pufferinhalt wieder zurück auf String-Heap
   550                          
   551                          ; 0 ------------------------------------------- FFFF	
   552                          ;        Ziel                        Quelle
   553                          ;          +--------------------------+
   554                          ;          |                          |
   555                          ;          V                         /^\
   556                          ;     |||||||||||                |||||||||||
   557                          ;     ^          ^               ^          ^ 
   558                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   559                          
   560                          COPYBACK
   561                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   562                          	LDA BUFPTR	; Puffer leer ...
   563                          	CMP #<(BUF+BUFSIZE)
   564                          	BNE +
   565                          	LDA BUFPTR+1	; Wenn Pointer am Ende ...
   566                          	CMP #>(BUF+BUFSIZE)
   567                          	BEQ NOCOPY	; ist der Puffer leer, ev. nächster
   568                          +			; Bereich ...
   569                          } else {
   570                          			; Sonderfall: Pufferende bei $FFFF
   571  c5eb a55f               	LDA BUFPTR	; Puffer leer,
   572  c5ed 0560               	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
   573  c5ef f021               	BEQ NOCOPY	; War es letzter Bereich?
   574                          }
   575                          
   576                          !ifdef orig_movblock {
   577                          	LDA STRPTR	; Original MOVBLOCK braucht
   578                          	LDX STRPTR+1	; Zielblockende+1
   579                          } else {
   580  c5f1 a54e               	LDA NEWPTR	; Optimiertes MOVBLOCK braucht
   581  c5f3 a64f               	LDX NEWPTR+1	; Zielblockanfang	
   582                          }
   583  c5f5 8558               	STA $58		; = STRADR
   584  c5f7 8659               	STX $59
   585                          
   586                          !ifdef orig_movblock {
   587                          	LDA NEWPTR
   588                          	LDX NEWPTR+1	
   589                          }
   590  c5f9 8533               	STA STRPTR	; neues FRETOP
   591  c5fb 8634               	STX STRPTR+1
   592                          
   593                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   594                          	LDA #<(BUF+BUFSIZE)
   595                          	STA $5A
   596                          	LDA #>(BUF+BUFSIZE)
   597                          	STA $5B
   598                          } else {
   599                          			; Sonderfall Pufferende bei $FFFF
   600  c5fd a900               	LDA #$00	; Quellblockende+1
   601  c5ff 855a               	STA $5A
   602  c601 855b               	STA $5B
   603                          }
   604                          			; Quellbockanfang = BUFPTR
   605                          
   606  c603 78                 	SEI		; keine Interrupts zulassen, wegen
   607  c604 a501               	LDA PROZPORT	; KERNAL-ROM wegblenden
   608  c606 48                 	PHA		; damit das Puffer-RAM zugänglich
   609  c607 a935               	LDA #MEMRAM	; wird. Auch BASIC-ROM ist damit weg!
   610  c609 8501               	STA PROZPORT
   611                          
   612  c60b 20bfa3             	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
   613                          			; bei IRQ-Anbindung eigene Kopie verwenden,
   614                          			; da das BASIC-ROM ausgeblendet ist!
   615                          			; Andernfalls ist ja die ROM-Kopie im RAM da.
   616                          			; Z=1
   617  c60e 68                 	PLA		; ursprünglicher Zustand
   618  c60f 8501               	STA PROZPORT	; KERNAL-ROM wieder aktivieren
   619  c611 58                 	CLI
   620                          NOCOPY
   621  c612 4c5cc5             	JMP NEXTBLOCK	; nächsten Bereich
   622                          
   623                          
   624                          ;
   625                          ; *** Get String - nächsten String mit Länge ungleich 0
   626                          ;
   627                          ; ( C-Flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-Flag )
   628                          ;
   629                          ; Bei C=1 wird beim SDS gestartet, sonst von der letzten
   630                          ; Position gemäß STRDP und String-Status STAT.
   631                          ; Das Z-Flag ist gesetzt, wenn kein String mehr
   632                          ; vorhanden ist, sonst in X/Y die Adresse und in LEN
   633                          ; die Stringlänge.
   634                          
   635  c615 9063               GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
   636                          			; sonst Start bei SDS ...
   637                          
   638                          ; *** String-Descriptor-Stack (SDS): TOSS bis TSSP
   639                          ;
   640                          ;    +-------------+
   641                          ;    |             V
   642                          ;    |    belegt->|<-frei
   643                          ;   +-+     +-----+-----+-----+
   644                          ;   | |     |S|L|H|S|L|H|S|L|H|
   645                          ;   +-+     +-----+-----+-----+
   646                          ;    ^       ^     ^     ^     ^
   647                          ;    $16     $19   $1C   $1F   $22
   648                          ;    TSSP    TOSS
   649                          
   650                          DESCSTACK
   651  c617 a000               	LDY #0
   652  c619 8423               	STY STRDP+1	; Descriptor auf
   653  c61b a905               	LDA #STAT_SDS	; Status: SDS
   654  c61d 8557               	STA STAT
   655  c61f a219               	LDX #TOSS	; SDS Start
   656  c621 d005               	BNE ISDSTEND	; immer verzweigen
   657  c623 a622               DSTACK	LDX STRDP
   658  c625 e8                 NEXTDST	INX		; nächster Descriptor
   659  c626 e8                 	INX
   660  c627 e8                 	INX
   661                          ISDSTEND
   662  c628 e416               	CPX TSSP	; Stack durch?
   663  c62a f010               	BEQ VARS
   664  c62c b500               	LDA 0,X		; String-Länge
   665  c62e f0f5               	BEQ NEXTDST
   666  c630 8552               	STA LEN		; Rückgabevariable
   667  c632 8622               	STX STRDP	; festhalten
   668  c634 b502               	LDA 2,X		; String-Adr. high
   669  c636 a8                 	TAY
   670  c637 b501               	LDA 1,X		; String-Adr. low
   671  c639 aa                 	TAX
   672  c63a 98                 	TYA		; immer ungleich 0, Z=0
   673  c63b 60                 	RTS		; Adresse in X/Y retour
   674                          
   675                          ; *** Variablen: VARTAB bis ARYTAB
   676                          
   677  c63c a52d               VARS	LDA VARTAB	; Variablenanfang
   678  c63e a62e               	LDX VARTAB+1
   679  c640 8522               	STA STRDP
   680  c642 8623               	STX STRDP+1
   681  c644 a003               	LDY #STAT_VAR	; Status: einfache Variablen
   682  c646 8457               	STY STAT
   683  c648 d00b               	BNE ISVAREND	; immer verzweigen
   684                          VAR
   685  c64a 18                 NEXTVAR	CLC		; nächste Variable
   686  c64b a522               	LDA STRDP
   687  c64d 6907               	ADC #7		; Variablenlänge
   688  c64f 8522               	STA STRDP
   689  c651 9002               	BCC ISVAREND
   690  c653 e623               	INC STRDP+1	; Überlauf High-Byte
   691                          ISVAREND
   692  c655 c52f               	CMP ARYTAB
   693  c657 d006               	BNE CHECKVAR
   694  c659 a623               	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
   695  c65b e430               	CPX ARYTAB+1
   696  c65d f026               	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
   697                          CHECKVAR
   698  c65f a000               	LDY #0		; Variablenname
   699  c661 b122               	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
   700  c663 30e5               	BMI NEXTVAR	; kein String, nächste V.
   701  c665 c8                 	INY
   702  c666 b122               	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
   703  c668 10e0               	BPL NEXTVAR	; kein String, nächste V.
   704  c66a c8                 	INY
   705  c66b b122               	LDA (STRDP),Y	; String-Länge
   706  c66d f0db               	BEQ NEXTVAR	; = 0, Nächste Variable
   707  c66f 8552               	STA LEN		; Rückgabevariable
   708  c671 c8                 	INY
   709  c672 b122               	LDA (STRDP),Y	; String-Adresse low
   710  c674 aa                 	TAX
   711  c675 c8                 	INY
   712  c676 b122               	LDA (STRDP),Y	; String-Adresse high
   713  c678 a8                 	TAY		; immer ungleich 0, Z=0
   714  c679 60                 	RTS		; Adresse in X/Y retour
   715                          
   716                          CHECKTYPE
   717  c67a a557               	LDA STAT	; GETSA-Einstieg mit C=0
   718  c67c c903               	CMP #STAT_VAR	; String-Status?
   719  c67e 9042               	BCC ARRAY	; =1 -> Arrays
   720  c680 f0c8               	BEQ VAR		; =3 -> Variablen
   721  c682 4c23c6             	JMP DSTACK	; =5 -> String-Desc.-Stack
   722                          
   723                          ; *** Arrays: ARYTAB bis STREND
   724                          
   725  c685 8550               ARRAYS	STA PTR		; A/X von Variablendurchlauf
   726  c687 8651               	STX PTR+1	; Start Array-Array-Bereich
   727  c689 a001               	LDY #STAT_ARY
   728  c68b 8457               	STY STAT	; Status: Arrays
   729                          ISARREND
   730  c68d a550               	LDA PTR
   731  c68f a651               	LDX PTR+1
   732  c691 e432               CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
   733  c693 d004                       BNE NEXTARR	; erreicht?
   734  c695 c531               	CMP STREND
   735  c697 f04f               	BEQ NOSTRING	; Arrays fertig -> kein String
   736                          NEXTARR
   737  c699 8522               	STA STRDP	; immer C=0
   738  c69b 8623               	STX STRDP+1
   739  c69d a000               	LDY #0
   740  c69f b122               	LDA (STRDP),Y	; Array-Name
   741  c6a1 aa                 	TAX		; Array-Typ merken
   742  c6a2 c8                 	INY
   743  c6a3 b122               	LDA (STRDP),Y
   744  c6a5 08                 	PHP		; Array-Typ merken
   745  c6a6 c8                 	INY
   746  c6a7 b122               	LDA (STRDP),Y	; Offset nächstes Array
   747  c6a9 6550               	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
   748  c6ab 8550               	STA PTR		; Start Folge-Array
   749  c6ad c8                 	INY
   750  c6ae b122               	LDA (STRDP),Y
   751  c6b0 6551               	ADC PTR+1
   752  c6b2 8551               	STA PTR+1
   753  c6b4 28                 	PLP		; Var.-Typ holen
   754  c6b5 10d6               	BPL ISARREND	; kein String-Array
   755  c6b7 8a                 	TXA		; Var.-Typ holen
   756  c6b8 30d3               	BMI ISARREND	; kein String-Array
   757  c6ba c8                 	INY
   758  c6bb b122               	LDA (STRDP),Y	; Anzahl der Dimensionen
   759  c6bd 0a                 	ASL		; *2
   760  c6be 6905               	ADC #5		; Offset = Dimensionen*2+5
   761                          			; C=0 solange Dim.. <= 125
   762  c6c0 d003               	BNE ADVDESC	; immer verzweigen
   763                          ARRAY			; Einstieg bei Fortsetzung
   764                          NEXTASTR
   765  c6c2 18                 	CLC
   766  c6c3 a903               	LDA #3		; String-Descriptor-Länge
   767  c6c5 6522               ADVDESC	ADC STRDP	; nächten String
   768  c6c7 8522               	STA STRDP
   769  c6c9 9002               	BCC +
   770  c6cb e623               	INC STRDP+1	; Überlauf High-Byte
   771  c6cd c550               +	CMP PTR		; Array durch?
   772  c6cf d006               	BNE IS0ASTR
   773  c6d1 a623               	LDX STRDP+1
   774  c6d3 e451               	CPX PTR+1
   775  c6d5 f0ba               	BEQ CHKAEND	; A/X = PTR, Array-Ende prüfen
   776                          IS0ASTR
   777  c6d7 a000               	LDY #0
   778  c6d9 b122               	LDA (STRDP),Y	; String-Länge
   779  c6db f0e5               	BEQ NEXTASTR	; weiter im Array
   780  c6dd 8552               	STA LEN		; Rückgabevariable
   781  c6df c8                 	INY
   782  c6e0 b122               	LDA (STRDP),Y	; String-Adresse low
   783  c6e2 aa                 	TAX
   784  c6e3 c8                 	INY
   785  c6e4 b122               	LDA (STRDP),Y	; String-Adresse high
   786  c6e6 a8                 	TAY		; immer ungleich 0, Z=0
   787  c6e7 60                 	RTS		; Adresse in X/Y retour
   788                          
   789                          NOSTRING
   790  c6e8 a900               	LDA #0		; Länge 0 
   791  c6ea 8552               	STA LEN		; kein String gefunden
   792  c6ec 60                 	RTS		; Z=1
   793                          
   794                          ;
   795                          ; CORR - String-Adresse im Descriptor korrigieren
   796                          ;
   797                          ; ( STRADR, STAT -> )
   798                          ;
   799  c6ed a557               CORR	LDA STAT	; String-Status
   800  c6ef 2903               	AND #%011	; nur 2 Bits
   801  c6f1 a8                 	TAY		; Lage des Descriptors
   802  c6f2 a54e               	LDA NEWPTR	;
   803  c6f4 9122               	STA (STRDP),Y	; ... bei SDS
   804  c6f6 c8                 	INY		; ... und Array verschieden!
   805  c6f7 a54f               	LDA NEWPTR+1
   806  c6f9 9122               	STA (STRDP),Y
   807  c6fb 60                 	RTS
   808                          
   809                          !ifndef basic_patch {
   810                          
   811                            !ifndef orig_movblock {
   812                          	; Die optimiert Routine
   813                          	;
   814                          	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
   815                          	; ist beim Ausblenden des KERNALs, auch immer BASIC
   816                          	; ausgeblendet ist, nicht verfügbar. Daher eine
   817                          	; extra Routine.
   818                          
   819                          	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
   820                          	; Für das Kopieren zu niedrigen Adressen ist es Überlappungsrobust.
   821                          	; Eingabe: $5F/$60 Startadresse Quelle
   822                          	;	   $5A/$5B Endadresse+1 der Quelle
   823                          	;	   $58/$59 Startadresse Ziel
   824                          	; Zerstört: A, X, Y
   825                          	; Ausgabe: $58/$59 hat den Wert Zielendadresse+1
   826                          	;          X = 0
   827                          	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
   828                          	;	   Z-Flag = 1
   829                          MOVBLOCK
   830                                  SEC
   831                                  LDA $5A       ; Quelle Endadresse low
   832                                  SBC $5F       ; minus Quelle Startadresse low
   833                                  TAY           ; Länge low
   834                                  LDA $5B       ; Endadresse high
   835                                  SBC $60       ; minus Startadresse high
   836                                  TAX           ; Länge high, als DEC-DEC-Counter
   837                                  TYA           ; Länge low
   838                                  BEQ copy      ; wenn 0, dann geht es einfach ...
   839                                  CLC           ; Länge in A
   840                                  ADC $5F       ; Quellstartadresse um Low-Byte-Offset
   841                                  STA $5F       ; korrigieren: -(-Länge) -> +Länge
   842                                  BCS +         ; da eigentlich Subtraktion, Überlauf
   843                                  DEC $60       ; entspr. in Startadresse high korr.
   844                          +
   845                                  TYA           ; Länge low
   846                                  CLC
   847                                  ADC $58       ; Zielstartadresse um Low-Bye-Offset
   848                                  STA $58       ; korrigieren: -(-Länge) -> +Länge
   849                                  BCS +         ; da eigentlich Subtraktion, Überlauf
   850                                  DEC $59       ; entspr. in Startadresse high korr.
   851                          +
   852                          	INX           ; Page-Zähler (alle ganzen und einen teilweise)
   853                          	TYA           ; Länge low
   854                          	EOR #$FF      ; negieren (2er-Komplement):
   855                          	TAY           ; NOT(X)+1
   856                          	INY
   857                          copy    LDA ($5F),Y   ; Quelle auf
   858                                  STA ($58),Y   ; Ziel kopieren, aufsteigend
   859                                  INY
   860                                  BNE copy
   861                                  INC $60       ; Quelle high
   862                                  INC $59       ; Ziel high
   863                                  DEX           ; Blockzähler 
   864                                  BNE copy
   865                                  RTS
   866                          	; braucht 6 Bytes weniger als die Originalroutine.
   867                            } else {
   868                          	; Die originale Routine aus dem ROM:
   869                          	;
   870                          	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
   871                          	; Da beim Ausblenden des KERNALs auch immer BASIC
   872                          	; ausgeblendet ist, kann die Routine nicht im ROM aufgerufen
   873                          	; werden!
   874                          
   875                          	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
   876                          	; Für das Kopieren zu höheren Adressen ist es Überlappungsrobust.
   877                          	; Eingabe: $5F/$60 Startadresse Quelle
   878                          	;	   $5A/$5B Endadresse+1 der Quelle
   879                          	;	   $58/$59 Endadresse+1 des Ziels
   880                          	; Zerstört: $22, A, X, Y
   881                          	; Ausgabe: $58/$59 hat den Wert des 1. Bytes des Zielbereichs.
   882                          	;          X = 0
   883                          	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
   884                          	;	   Z-Flag = 1
   885                          MOVBLOCK
   886                                  SEC
   887                                  LDA $5A       ; Endadresse low
   888                                  SBC $5F       ; minus Startadresse low
   889                                  STA $22       ; Länge low
   890                                  TAY
   891                                  LDA $5B       ; Endadresse high
   892                                  SBC $60       ; minus Startadresse high
   893                                  TAX           ; Länge high
   894                                  INX           ; Länge als DEC-DEC-Counter
   895                                  TYA           ; Länge low
   896                                  BEQ +         ; wenn 0, dann 
   897                                  LDA $5A       ; Endadresse um Low-Byte-Offset
   898                                  SEC           ; der Länge korrigieren
   899                                  SBC $22       ; Länge low
   900                                  STA $5A       ;
   901                                  BCS ++
   902                                  DEC $5B       ; Endadresse high korr.
   903                                  SEC
   904                          ++      LDA $58       ; Zieladresse um Low-Bye-Offset
   905                                  SBC $22       ; der Länge korrigieren
   906                                  STA $58
   907                                  BCS +++	      ; High byte
   908                                  DEC $59       ; 
   909                                  BCC +++
   910                          -       LDA ($5A),Y   ; Quelle auf
   911                                  STA ($58),Y   ; Ziel kopieren von höhere
   912                          --		      ; auf niedrigere Adressen
   913                          +++     DEY
   914                                  BNE -
   915                                  LDA ($5A),Y   ; Quelle
   916                                  STA ($58),Y   ; Ziel
   917                          +       DEC $5B       ; Quelle high
   918                                  DEC $59       ; Ziel high
   919                                  DEX           ; Blockzähler 
   920                                  BNE --
   921                                  RTS
   922                            }
   923                          }
   924                          
   925                          
   926                          !ifdef debug {
   927                          !source "debug.asm"
   928                          }
   929                          
   930  c6fc 00                 ORIGVID !byte 0		; originale Video der Markenposition
   931  c6fd 00                 ORIGCOL !byte 0		; originale Farbe der Markenposition
   932                          !ifndef basic_patch {
   933                          ORIGIRQ !byte 0,0	; originaler IRQ-Vektor
   934                          }
   935  c6fe 00                 SAVE	!byte 0		; gesicherte ZP-Variablen
   936                          *=*+ZPLEN-1
   937                          
   938                          
