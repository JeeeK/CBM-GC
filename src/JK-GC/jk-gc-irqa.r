
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
    16                          ; R�umt Stringspeicher auf
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
    32                          ;	   hier auch noch 60 Bytes f�r die Blockkopierroutine
    33                          ;	   ben�tigt wird, da hier die ROM-Variante nicht genutzt
    34                          ; 	   werden kann!
    35                          
    36                          ; Wenn aktiv, wird als Puffer das RAM unter dem BASIC-ROM verwendet,
    37                          ; sonst jenes unter dem KERNAL. So kann etwa das RAM unter dem KERNAL
    38                          ; f�r Grafikzwecke verwendet werden ...
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
    59                          MEMSIZ   = $37		; h�chste RAM-Adresse f�r BASIC, Start
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
    76                          ; $58-5B wird von MOVBLOCK zerst�rt!
    77                          STRADR   = $58		; String-Address (temp.)
    78                          BEREND   = $5D		; Bereichsende
    79                          BUFPTR   = $5F		; Buffer-Pointer
    80                          			; (MOVBLOCK: Quellblockanfang!)
    81                          
    82                          CPTR     = $22		; Pointer f�r Install-Routine
    83                          
    84                          ; zu rettender Zeropage-Bereich
    85                          ZPSTART  = $4C		; 1. zu rettende
    86                          ZPEND    = $52		; letzte zu rettende Stelle
    87                          ZPLEN    = ZPEND-ZPSTART+1
    88                          			; Anzahl zu rettenden Bytes
    89                          
    90                          ; Konstanten
    91                          
    92                          ; f�r Variabe STAT (String State):
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
   104                          ROMSIZE  = $2000        ; ROM-L�nge 8 KByte8
   105                          
   106                          ; Puffer
   107                          !ifndef basic_rom_buffer {
   108                          BUF	 = KERNAL	; Puffer unter KERNAL
   109                          } else {
   110                          BUF	 = BASIC	; Puffer unter KERNAL
   111                          }
   112                          BUFSIZE  = ROMSIZE	; Puffergr��e
   113                          
   114                          ; I/O-BEREICHE/-ADRESSEN
   115                          
   116                          VIDBASE  = $0400	; Video RAM
   117                          COLBASE  = $D800	; Color RAM
   118                          
   119                          MARKE    = "*"		; Aktivit�tsanzeige
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
   143                          			; zerst�rt $58/$59/$5A/$5B/$22
   144                          
   145                          	; BASIC ins RAM kopieren, um die GC-Routine
   146                          	; zu patchen ...
   147                          	LDA #MEMROM
   148                          	STA PROZPORT	; alles ROM (also vom ROM kopieren)
   149                          	LDY #<BASIC	; ROM-Beginn
   150                          	STY CPTR
   151                          	LDA #>BASIC
   152                          	STA CPTR+1	; BASIC-ROM Anfang
   153                          	LDX #>($2000)	; BASIC-ROM L�nge in Pages
   154                          CPYROM	LDA (CPTR),Y	; ROM lesen
   155                          	STA (CPTR),Y	; RAM schreiben
   156                          	INY
   157                          	BNE CPYROM
   158                          	INC CPTR+1	; n�chste Page
   159                          	DEX		; Page-Z�hler
   160                          	BNE CPYROM
   161                          	LDA PROZPORT	; auf RAM umschalten
   162                          	AND #%11111110	; "BASIC-ROM aus"-Maske
   163                          	STA PROZPORT
   164                          	LDA #<COLLECT	; "JMP COLLECT"
   165                          	STA GARBCOL+1	; patchen ...
   166                          	LDA #>COLLECT
   167                          	STA GARBCOL+2
   168                          	LDA #$4C	; JMP-Opcode
   169                          	STA GARBCOL
   170                          	RTS
   171                          } else {
   172  c500 78                 	SEI
   173  c501 ad1403             	LDA V_IRQ	; bisherige IRQ-Routine aufheben
   174  c504 ae1503             	LDX V_IRQ+1
   175  c507 c921               	CMP #<(IRQ)
   176  c509 d004               	BNE HOOK
   177  c50b e0c5               	CPX #>(IRQ)
   178  c50d f010               	BEQ INSTEXIT	; Vektor bereits installiert
   179                          HOOK
   180  c50f 8dc6c7             	STA ORIGIRQ
   181  c512 8ec7c7             	STX ORIGIRQ+1
   182  c515 a921               	LDA #<(IRQ)	; GC-Routine einh�ngen ...
   183  c517 a2c5               	LDX #>(IRQ)
   184  c519 8d1403             	STA V_IRQ
   185  c51c 8e1503             	STX V_IRQ+1
   186                          INSTEXIT
   187  c51f 58                 	CLI
   188  c520 60                 	RTS
   189                          
   190                          ; In der IRQ-Routine wird festgestellt, ob die alte GC
   191                          ; gerade aktiv ist, was dann der Fall ist, wenn
   192                          ;   * am Stack der Caller-PC der Open-Space-for-Memory-Routine ($B62B-1) ist,
   193                          ;   * am Stack einer der drei Caller-PC der Search-for-Next-String-Routine
   194                          ;     ist,
   195                          ;   * der PC im Bereich von $B526 bis $B63D liegt.
   196                          ; Gewisse Zust�nde m�ssen dabei allerings korrigiert werden.
   197                          ; Korrekturf�lle:
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
   209                          ;      gerade kopierte String fertig �bertragen werden, daher
   210                          ;      kehrt das RTI wieder zur Routine zur�ck und erst mit
   211                          ;      dem RTS wird �ber die manipulierte Adresse am Stack
   212                          ;      zur neuen GC gesprungen.
   213                          ;   4. Wenn in Subroutine "Search for Next String" unterbrochen
   214                          ;      wurde (an den drei m�glichen Aufrufadressen erkannt),
   215                          ;      befindet sich am Stack die Adresse des Aufrufers, wobei
   216                          ;      am Stack die RTI-R�ckkehradresse auf eine Korrektur-
   217                          ;      routine gelenkt wird, die 2 Bytes vom Stack nimmt und
   218                          ;      zur neuen GC verzweigt.
   219                          ;
   220                          ; Sonst kann, wenn der Unterbrechungs-PC im GC-Code-Bereich liegt,
   221                          ; direkt zu neuen GC verzweigt werden.
   222                          ;
   223                          ; Egal wo die GC unterbrochen wird, Top of String-Heap ($33/$34)
   224                          ; ist zwar bereits zerst�rt, aber das ist egal, weil
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
   244  c521 78                 	SEI
   245                          
   246                          	; IRQ-Stackframe:
   247                          	; $104,X Status-Reg
   248                          	; $105,X Low-PC
   249                          	; $106,X High-PC
   250                          	; ev. aufrufene Routine, falls in einer Subroutine:
   251                          	; $107,X Low-Byte des (Aufruf-PC)-1
   252                          	; $108,X High-Byte des (Aufruf-PC)-1
   253                          
   254  c522 bd0801             	LDA $108,X	; Aufrufer PC high
   255  c525 a8                 	TAY
   256  c526 bd0701             	LDA $107,X	; Aufrufer PC low
   257                          	; Aufruf-PC-1 in A/Y
   258                          			; sind wir in "Open Space"?
   259  c529 c92a               	CMP #<(CALLER_OPSP)
   260  c52b d01e               	BNE CHK_SNS
   261  c52d c0b6               	CPY #>(CALLER_OPSP)
   262  c52f d01a               	BNE CHK_SNS
   263                          IN_OPSP
   264                          	; mit RTI zur�ck in die unterbrochene
   265                          	; Kopierroutine, damit der String
   266                          	; fertig �bertragen wird, aber mit
   267                          	; dem RTS dort, geht es dann zu
   268                          	; Descriptor-Korrekturroutine
   269                          	; die dann endlich die neue CG aufrufen
   270                          	; darf.
   271  c531 a93c               	LDA #<(CORR_STR-1)
   272  c533 9d0701             	STA $107,X	; wegen RTS minus 1
   273  c536 a9c5               	LDA #>(CORR_STR-1)
   274  c538 9d0801             	STA $108,X	; immer >0
   275  c53b d028               	BNE CONT	; immer, mit RTI zur�ck
   276                          
   277                          CORR_STR
   278  c53d a455               	LDY $55		; Offset des Descriptors
   279  c53f c8                 	INY		; String-L�nge
   280  c540 a558               	LDA $58		; String-Adresse low
   281  c542 914e               	STA ($4E),Y	; in den Descriptor
   282  c544 c8                 	INY
   283  c545 a559               	LDA $59		; String-Adresse high
   284  c547 914e               	STA ($4E),Y	; nun String-Adresse high setzen
   285  c549 d069               	BNE START_COLLECT
   286                          			; immer, weil immer >0
   287                          
   288                          	
   289                          CHK_SNS			; sind wir in "Search for Next String"?
   290  c54b c0b5               	CPY #>(CALLER_SNS1)
   291                          			; High-Byte ist f�r alle drei Adressen gleich!
   292                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   293                          	  !error "High-Byte von CALLER_SNS* sind verschieden. Sie m�ssen gleich sein!"
   294                          	}
   295  c54d d019               	BNE CHK_PC
   296  c54f c963               	CMP #<(CALLER_SNS1)
   297  c551 f008               	BEQ IN_SUB
   298  c553 c94a               	CMP #<(CALLER_SNS2)
   299  c555 f004               	BEQ IN_SUB
   300  c557 c9ba               	CMP #<(CALLER_SNS3)
   301  c559 d00d               	BNE CHK_PC
   302                          
   303                          IN_SUB
   304  c55b a9b2               	LDA #<(SKIPSUB) ; Unterbrechungs-PC umbiegen ...
   305  c55d 9d0501             	STA $105,X	; Low-Byte
   306  c560 a9c5               	LDA #>(SKIPSUB)
   307  c562 9d0601             	STA $106,X	; High-Byte
   308                          			; mit dem RTI geht es zu SKIPSUB wo
   309                          			; der Caller vom Stack genommen wird.
   310                          
   311  c565 6cc6c7             CONT	JMP (ORIGIRQ)	; weiter zum Original-Code (Kette).
   312                          
   313                          CHK_PC
   314  c568 bd0601             	LDA $106,X	; Unterbrechungs-PC
   315  c56b a8                 	TAY		; High-Byte
   316  c56c bd0501             	LDA $105,X	; Low-Byte
   317  c56f c0b5               	CPY #>(GC_START)
   318  c571 90f2               	BCC CONT	; vor der GC
   319  c573 d004               	BNE +		; �ber GC-Anfang
   320  c575 c926               	CMP #<(GC_START)
   321  c577 90ec               	BCC CONT	; unterhalb GC
   322  c579 c0b6               +	CPY #>(GC_END+1)
   323  c57b 9006               	BCC ++		; in der GC!
   324  c57d d0e6               	BNE CONT	; �ber der GC
   325  c57f c93d               	CMP #<(GC_END+1)
   326  c581 b0e2               	BCS CONT	; �ber der GC
   327                          ++
   328                          	; Die GC wurde unterbrochen!
   329                          	; In irgendwelchen Spezialbereichen, wo wir mehr machen m�ssen?
   330                          
   331                          	; in PHP/PLP Sektion?
   332                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   333                          	  !error "High-Byte von GC_PHP_START und GC_PHP_END sind nicht gleich!"
   334                          	}
   335  c583 c0b5               	CPY #>(GC_PHP_START)
   336  c585 d00b               	BNE +		; nicht in richter Page, n�chster Bereichstest
   337  c587 c98a               	CMP #<(GC_PHP_START)
   338  c589 9007               	BCC +		; unterhalb, keine Stack-Korrektur
   339  c58b c999               	CMP #<(GC_PHP_END+1)
   340  c58d b003               	BCS +		; dar�ber, keine Stack-Korrektur
   341                          	; Stack-Korrektur:
   342  c58f 68                 	PLA		; SR vom Stack nehmen, setzt ev. Flags N,Z aber nicht C
   343  c590 9014               	BCC TO_COLLECT	; C immer 0, mittels RTI zu COLLECT
   344                          +
   345                          	; in kritischer Sektion?
   346                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   347                          	  !error "High-Byte von GC_CRIT_START und GC_CRIT_END sind nicht gleich!"
   348                          	}
   349  c592 c0b6               	CPY #>(GC_CRIT_START)
   350  c594 d010               	BNE +		; nicht in richtiger Page, dann normal weiter
   351  c596 c932               	CMP #<(GC_CRIT_START)
   352  c598 900c               	BCC +		; unterhalb, keine Korrektur
   353  c59a c939               	CMP #<(GC_CRIT_END+1)
   354  c59c b008               	BCS +		; dar�ber, keine Korrektur
   355                          
   356                          	; Korrektur, High Byte der String-Adresse in Descriptor setzen,
   357                          	; weil bereits das Low-Byte gesetzt wurde. Die Adresse im Descriptor
   358                          	; w�re sonst inkonsistent!
   359  c59e a455               	LDY $55		; Offset des Descriptors
   360  c5a0 c8                 	INY		; String-L�nge
   361  c5a1 c8                 	INY		; String-Adresse-Low (ist schon gesetzt!)
   362  c5a2 a559               	LDA $59
   363  c5a4 914e               	STA ($4E),Y	; nun String-Adresse-High setzen
   364                          +
   365                          	; mittels RTI COLLECT aufrufen:
   366                          TO_COLLECT
   367  c5a6 a9b4               	LDA #<(START_COLLECT)
   368                          			; R�ckkehradresse f�r RTI am Stack umbiegen ...
   369  c5a8 9d0501             	STA $0105,X	; Low-Byte
   370  c5ab a9c5               	LDA #>(START_COLLECT)
   371  c5ad 9d0601             	STA $0106,X	; High-Byte
   372  c5b0 d0b3               	BNE CONT	; IRQ behandeln, RTI startet dann die neue GC ...
   373                          
   374                          SKIPSUB			; Open-Space- oder Search-for-Next-String-Routine abgebrochen:
   375  c5b2 68                 	PLA		; kommt direkt von RTI, Aufruf-PC (f�r RTS) in alter GC
   376  c5b3 68                 	PLA		; verwerfen und die neue GC �bernimmt ...
   377                          			; direkt durch zu COLLECT
   378                          START_COLLECT
   379  c5b4 a903               	LDA #3
   380  c5b6 8553               	STA $53		; Step-Size f�r n�chsten Descriptor auf
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
   391  c5b8 ade707             	LDA MARKEVID	; Kontrollanzeige Bildschirm
   392  c5bb 8dc4c7             	STA ORIGVID
   393  c5be a92a               	LDA #MARKE
   394  c5c0 8de707             	STA MARKEVID	; Marke: Zeichen
   395  c5c3 ade7db             	LDA MARKECOL	; sichern
   396  c5c6 8dc5c7             	STA ORIGCOL
   397  c5c9 a909               	LDA #MARKEFARBE
   398  c5cb 8de7db             	STA MARKECOL	; Marke: Farbe sichern
   399                          
   400  c5ce a207               	LDX #ZPLEN	; Zeropage-Reg.
   401  c5d0 b54b               SAVLOOP	LDA ZPSTART-1,X	; retten
   402  c5d2 9dc7c7             	STA SAVE-1,X
   403  c5d5 ca                 	DEX
   404  c5d6 d0f8               	BNE SAVLOOP
   405                          
   406  c5d8 a537               	LDA MEMEND	; String-Pointer
   407  c5da a638               	LDX MEMEND+1	; und Bereichanfang 
   408  c5dc 8533               	STA STRPTR	; auf Speicherende
   409  c5de 8634               	STX STRPTR+1	; setzen.
   410  c5e0 854c               	STA BERANF
   411  c5e2 864d               	STX BERANF+1
   412                          
   413                          ; *** N�chster zu betrachtender Bereich am String-Heap
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
   431                          ; k�nnte, der max. 254 Bytes das Bereichsende �berragen k�nnte. Dieser 
   432                          ; "�berhang" muss im Puffer Platz haben und dort reserviert sein!
   433                          
   434                          NEXTBLOCK
   435  c5e4 a533               	LDA STRPTR	; NEWPTR parallel mit
   436  c5e6 854e               	STA NEWPTR	; BUFPTR mitziehen ...
   437  c5e8 a534               	LDA STRPTR+1
   438  c5ea 854f               	STA NEWPTR+1
   439  c5ec a64c               	LDX BERANF	; Bereich war zuletzt
   440  c5ee a54d               	LDA BERANF+1	; String-Heap-Ende?
   441  c5f0 e431               	CPX STREND
   442  c5f2 d004               	BNE +
   443  c5f4 c532               	CMP STREND+1
   444  c5f6 f01b               	BEQ EXIT	; ja -> fertig
   445                          +
   446  c5f8 865d               	STX BEREND	; um Pufferl�nge - 256
   447  c5fa 855e               	STA BEREND+1	; nach unten verlegen.
   448                          	!if <BUFSIZE > 0 {
   449                          	  !error "BUFSIZE ist nicht ein Vielfaches von 256 ($100)!"
   450                          	}
   451  c5fc 38                 	SEC		
   452  c5fd e91f               	SBC #(>BUFSIZE-1) ; Bereichsl�nge in Pages,
   453                          			; kann um 254 Bytes �berragt werden!
   454  c5ff 9008               	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
   455  c601 854d               	STA BERANF+1
   456  c603 e431               	CPX STREND	; Ende des String-Heaps erreicht?
   457  c605 e532               	SBC STREND+1
   458  c607 b021               	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
   459                          LASTRANGE
   460  c609 a531               	LDA STREND	; Bereichanfang =
   461  c60b a632               	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
   462  c60d 854c               	STA BERANF	; 
   463  c60f 864d               	STX BERANF+1	; 
   464  c611 d017               	BNE STRINRANGE	; immer, weil High-Byte >0
   465                          
   466                          
   467                          ; *** Ende der Garbage Collection
   468                          
   469                          EXIT
   470  c613 a207               	LDX #ZPLEN
   471  c615 bdc7c7             RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
   472  c618 954b               	STA ZPSTART-1,X	; restaurieren.
   473  c61a ca                 	DEX
   474  c61b d0f8               	BNE RESLOOP
   475                          
   476  c61d adc4c7             	LDA ORIGVID	; Kontrollanzeige l�schen
   477  c620 8de707             	STA MARKEVID	; und alten Zustand wieder
   478  c623 adc5c7             	LDA ORIGCOL	; herstellen.
   479  c626 8de7db             	STA MARKECOL
   480                          !ifdef debug {
   481                          	JSR gra_off
   482                          }
   483  c629 60                 	RTS
   484                          
   485                          
   486                          ; *** Bereich durchgehen
   487                          
   488                          STRINRANGE
   489                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   490  c62a a9c0               	LDA #>(BUF+BUFSIZE)
   491  c62c 8560               	STA BUFPTR+1
   492  c62e a900               	LDA #<(BUF+BUFSIZE)
   493  c630 855f               	STA BUFPTR
   494                          } else {
   495                          			; Sonderfall Pufferende bei $FFFF
   496                          	LDA #0		; Buffer-Pointer auf
   497                          	STA BUFPTR	; $10000 (65536) = 0
   498                          	STA BUFPTR+1	; setzen.
   499                          }
   500  c632 38                 	SEC
   501  c633 24                 	!byte $24	; BIT ZP, d.h. n�chsten Befehl ignorieren!
   502                          NEXTSTR	
   503  c634 18                 	CLC
   504                          NEXTSTR1
   505  c635 20a7c6             	JSR GETSA	; N�chste String-Adresse holen.
   506  c638 f03b               	BEQ COPYBACK	; keinen String mehr gefunden!
   507                          
   508  c63a 98                 	TYA		; high Byte
   509  c63b e45d               	CPX BEREND	; X/A >= BEREND:
   510  c63d e55e               	SBC BEREND+1	; oberhalb des Bereichs, dann
   511  c63f b0f3               	BCS NEXTSTR	; n�chster String!
   512                          
   513  c641 98                 	TYA		; high Byte
   514  c642 e44c               	CPX BERANF	; X/A < BERANF:
   515  c644 e54d               	SBC BERANF+1	; unterhalb des Bereichs, dann
   516  c646 90ed               	BCC NEXTSTR1	; n�chster String!
   517                          			; Innerhalb des Bereichs:
   518  c648 a55f               	LDA BUFPTR	; Pufferzeiger um
   519  c64a e552               	SBC LEN		; String-L�nge nach unten
   520  c64c 855f               	STA BUFPTR	; setzen.
   521  c64e b002               	BCS +
   522  c650 c660               	DEC BUFPTR+1	; �berlauf High-Byte
   523                          
   524  c652 8459               +	STY STRADR+1	; String-Adresse abspeichern
   525  c654 8658               	STX STRADR	; f�r Kopieraktion.
   526                          
   527  c656 a452               	LDY LEN		; String-L�nge (> 0)
   528  c658 d004               	BNE NBENTRY	; immer, mit Dekrement beginnen!
   529  c65a b158               NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
   530  c65c 915f               	STA (BUFPTR),Y	; �bertragen, ROM ist aktiv
   531  c65e 88                 NBENTRY	DEY		; schreibt ins RAM unters ROM!
   532  c65f d0f9               	BNE NEXTBYT
   533                          LEN1
   534  c661 b158               	LDA (STRADR),Y	; Das 0. Byte extra
   535  c663 915f               	STA (BUFPTR),Y	; �bertragen
   536                          
   537  c665 38                 	SEC		; Neue String-Adresse:
   538  c666 a54e               	LDA NEWPTR	; Einfach den Pointer
   539  c668 e552               	SBC LEN		; mitziehen, ebenso um
   540  c66a 854e               	STA NEWPTR	; String-L�nge nach unten
   541  c66c b002               	BCS +		; setzen.
   542  c66e c64f               	DEC NEWPTR+1	; �berlauf High-Byte
   543                          +
   544  c670 207fc7             	JSR CORR	; String-Adresse in Descriptor �ndern.
   545                          			; Immmer Z=0,
   546  c673 d0bf               	BNE NEXTSTR	; zum n�chsten String.
   547                          
   548                          
   549                          ; *** Pufferinhalt wieder zur�ck auf String-Heap
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
   562  c675 a55f               	LDA BUFPTR	; Puffer leer ...
   563  c677 c900               	CMP #<(BUF+BUFSIZE)
   564  c679 d006               	BNE +
   565  c67b a560               	LDA BUFPTR+1	; Wenn Pointer am Ende ...
   566  c67d c9c0               	CMP #>(BUF+BUFSIZE)
   567  c67f f023               	BEQ NOCOPY	; ist der Puffer leer, ev. n�chster
   568                          +			; Bereich ...
   569                          } else {
   570                          			; Sonderfall: Pufferende bei $FFFF
   571                          	LDA BUFPTR	; Puffer leer,
   572                          	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
   573                          	BEQ NOCOPY	; War es letzter Bereich?
   574                          }
   575                          
   576                          !ifdef orig_movblock {
   577                          	LDA STRPTR	; Original MOVBLOCK braucht
   578                          	LDX STRPTR+1	; Zielblockende+1
   579                          } else {
   580  c681 a54e               	LDA NEWPTR	; Optimiertes MOVBLOCK braucht
   581  c683 a64f               	LDX NEWPTR+1	; Zielblockanfang	
   582                          }
   583  c685 8558               	STA $58		; = STRADR
   584  c687 8659               	STX $59
   585                          
   586                          !ifdef orig_movblock {
   587                          	LDA NEWPTR
   588                          	LDX NEWPTR+1	
   589                          }
   590  c689 8533               	STA STRPTR	; neues FRETOP
   591  c68b 8634               	STX STRPTR+1
   592                          
   593                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   594  c68d a900               	LDA #<(BUF+BUFSIZE)
   595  c68f 855a               	STA $5A
   596  c691 a9c0               	LDA #>(BUF+BUFSIZE)
   597  c693 855b               	STA $5B
   598                          } else {
   599                          			; Sonderfall Pufferende bei $FFFF
   600                          	LDA #$00	; Quellblockende+1
   601                          	STA $5A
   602                          	STA $5B
   603                          }
   604                          			; Quellbockanfang = BUFPTR
   605                          
   606  c695 78                 	SEI		; keine Interrupts zulassen, wegen
   607  c696 a501               	LDA PROZPORT	; KERNAL-ROM wegblenden
   608  c698 48                 	PHA		; damit das Puffer-RAM zug�nglich
   609  c699 a935               	LDA #MEMRAM	; wird. Auch BASIC-ROM ist damit weg!
   610  c69b 8501               	STA PROZPORT
   611                          
   612  c69d 208ec7             	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
   613                          			; bei IRQ-Anbindung eigene Kopie verwenden,
   614                          			; da das BASIC-ROM ausgeblendet ist!
   615                          			; Andernfalls ist ja die ROM-Kopie im RAM da.
   616                          			; Z=1
   617  c6a0 68                 	PLA		; urspr�nglicher Zustand
   618  c6a1 8501               	STA PROZPORT	; KERNAL-ROM wieder aktivieren
   619  c6a3 58                 	CLI
   620                          NOCOPY
   621  c6a4 4ce4c5             	JMP NEXTBLOCK	; n�chsten Bereich
   622                          
   623                          
   624                          ;
   625                          ; *** Get String - n�chsten String mit L�nge ungleich 0
   626                          ;
   627                          ; ( C-Flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-Flag )
   628                          ;
   629                          ; Bei C=1 wird beim SDS gestartet, sonst von der letzten
   630                          ; Position gem�� STRDP und String-Status STAT.
   631                          ; Das Z-Flag ist gesetzt, wenn kein String mehr
   632                          ; vorhanden ist, sonst in X/Y die Adresse und in LEN
   633                          ; die Stringl�nge.
   634                          
   635  c6a7 9063               GETSA	BCC CHECKTYPE	; C=0 -> n�chsten String laut STAT
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
   651  c6a9 a000               	LDY #0
   652  c6ab 8423               	STY STRDP+1	; Descriptor auf
   653  c6ad a905               	LDA #STAT_SDS	; Status: SDS
   654  c6af 8557               	STA STAT
   655  c6b1 a219               	LDX #TOSS	; SDS Start
   656  c6b3 d005               	BNE ISDSTEND	; immer verzweigen
   657  c6b5 a622               DSTACK	LDX STRDP
   658  c6b7 e8                 NEXTDST	INX		; n�chster Descriptor
   659  c6b8 e8                 	INX
   660  c6b9 e8                 	INX
   661                          ISDSTEND
   662  c6ba e416               	CPX TSSP	; Stack durch?
   663  c6bc f010               	BEQ VARS
   664  c6be b500               	LDA 0,X		; String-L�nge
   665  c6c0 f0f5               	BEQ NEXTDST
   666  c6c2 8552               	STA LEN		; R�ckgabevariable
   667  c6c4 8622               	STX STRDP	; festhalten
   668  c6c6 b502               	LDA 2,X		; String-Adr. high
   669  c6c8 a8                 	TAY
   670  c6c9 b501               	LDA 1,X		; String-Adr. low
   671  c6cb aa                 	TAX
   672  c6cc 98                 	TYA		; immer ungleich 0, Z=0
   673  c6cd 60                 	RTS		; Adresse in X/Y retour
   674                          
   675                          ; *** Variablen: VARTAB bis ARYTAB
   676                          
   677  c6ce a52d               VARS	LDA VARTAB	; Variablenanfang
   678  c6d0 a62e               	LDX VARTAB+1
   679  c6d2 8522               	STA STRDP
   680  c6d4 8623               	STX STRDP+1
   681  c6d6 a003               	LDY #STAT_VAR	; Status: einfache Variablen
   682  c6d8 8457               	STY STAT
   683  c6da d00b               	BNE ISVAREND	; immer verzweigen
   684                          VAR
   685  c6dc 18                 NEXTVAR	CLC		; n�chste Variable
   686  c6dd a522               	LDA STRDP
   687  c6df 6907               	ADC #7		; Variablenl�nge
   688  c6e1 8522               	STA STRDP
   689  c6e3 9002               	BCC ISVAREND
   690  c6e5 e623               	INC STRDP+1	; �berlauf High-Byte
   691                          ISVAREND
   692  c6e7 c52f               	CMP ARYTAB
   693  c6e9 d006               	BNE CHECKVAR
   694  c6eb a623               	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
   695  c6ed e430               	CPX ARYTAB+1
   696  c6ef f026               	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
   697                          CHECKVAR
   698  c6f1 a000               	LDY #0		; Variablenname
   699  c6f3 b122               	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
   700  c6f5 30e5               	BMI NEXTVAR	; kein String, n�chste V.
   701  c6f7 c8                 	INY
   702  c6f8 b122               	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
   703  c6fa 10e0               	BPL NEXTVAR	; kein String, n�chste V.
   704  c6fc c8                 	INY
   705  c6fd b122               	LDA (STRDP),Y	; String-L�nge
   706  c6ff f0db               	BEQ NEXTVAR	; = 0, N�chste Variable
   707  c701 8552               	STA LEN		; R�ckgabevariable
   708  c703 c8                 	INY
   709  c704 b122               	LDA (STRDP),Y	; String-Adresse low
   710  c706 aa                 	TAX
   711  c707 c8                 	INY
   712  c708 b122               	LDA (STRDP),Y	; String-Adresse high
   713  c70a a8                 	TAY		; immer ungleich 0, Z=0
   714  c70b 60                 	RTS		; Adresse in X/Y retour
   715                          
   716                          CHECKTYPE
   717  c70c a557               	LDA STAT	; GETSA-Einstieg mit C=0
   718  c70e c903               	CMP #STAT_VAR	; String-Status?
   719  c710 9042               	BCC ARRAY	; =1 -> Arrays
   720  c712 f0c8               	BEQ VAR		; =3 -> Variablen
   721  c714 4cb5c6             	JMP DSTACK	; =5 -> String-Desc.-Stack
   722                          
   723                          ; *** Arrays: ARYTAB bis STREND
   724                          
   725  c717 8550               ARRAYS	STA PTR		; A/X von Variablendurchlauf
   726  c719 8651               	STX PTR+1	; Start Array-Array-Bereich
   727  c71b a001               	LDY #STAT_ARY
   728  c71d 8457               	STY STAT	; Status: Arrays
   729                          ISARREND
   730  c71f a550               	LDA PTR
   731  c721 a651               	LDX PTR+1
   732  c723 e432               CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
   733  c725 d004                       BNE NEXTARR	; erreicht?
   734  c727 c531               	CMP STREND
   735  c729 f04f               	BEQ NOSTRING	; Arrays fertig -> kein String
   736                          NEXTARR
   737  c72b 8522               	STA STRDP	; immer C=0
   738  c72d 8623               	STX STRDP+1
   739  c72f a000               	LDY #0
   740  c731 b122               	LDA (STRDP),Y	; Array-Name
   741  c733 aa                 	TAX		; Array-Typ merken
   742  c734 c8                 	INY
   743  c735 b122               	LDA (STRDP),Y
   744  c737 08                 	PHP		; Array-Typ merken
   745  c738 c8                 	INY
   746  c739 b122               	LDA (STRDP),Y	; Offset n�chstes Array
   747  c73b 6550               	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
   748  c73d 8550               	STA PTR		; Start Folge-Array
   749  c73f c8                 	INY
   750  c740 b122               	LDA (STRDP),Y
   751  c742 6551               	ADC PTR+1
   752  c744 8551               	STA PTR+1
   753  c746 28                 	PLP		; Var.-Typ holen
   754  c747 10d6               	BPL ISARREND	; kein String-Array
   755  c749 8a                 	TXA		; Var.-Typ holen
   756  c74a 30d3               	BMI ISARREND	; kein String-Array
   757  c74c c8                 	INY
   758  c74d b122               	LDA (STRDP),Y	; Anzahl der Dimensionen
   759  c74f 0a                 	ASL		; *2
   760  c750 6905               	ADC #5		; Offset = Dimensionen*2+5
   761                          			; C=0 solange Dim.. <= 125
   762  c752 d003               	BNE ADVDESC	; immer verzweigen
   763                          ARRAY			; Einstieg bei Fortsetzung
   764                          NEXTASTR
   765  c754 18                 	CLC
   766  c755 a903               	LDA #3		; String-Descriptor-L�nge
   767  c757 6522               ADVDESC	ADC STRDP	; n�chten String
   768  c759 8522               	STA STRDP
   769  c75b 9002               	BCC +
   770  c75d e623               	INC STRDP+1	; �berlauf High-Byte
   771  c75f c550               +	CMP PTR		; Array durch?
   772  c761 d006               	BNE IS0ASTR
   773  c763 a623               	LDX STRDP+1
   774  c765 e451               	CPX PTR+1
   775  c767 f0ba               	BEQ CHKAEND	; A/X = PTR, Array-Ende pr�fen
   776                          IS0ASTR
   777  c769 a000               	LDY #0
   778  c76b b122               	LDA (STRDP),Y	; String-L�nge
   779  c76d f0e5               	BEQ NEXTASTR	; weiter im Array
   780  c76f 8552               	STA LEN		; R�ckgabevariable
   781  c771 c8                 	INY
   782  c772 b122               	LDA (STRDP),Y	; String-Adresse low
   783  c774 aa                 	TAX
   784  c775 c8                 	INY
   785  c776 b122               	LDA (STRDP),Y	; String-Adresse high
   786  c778 a8                 	TAY		; immer ungleich 0, Z=0
   787  c779 60                 	RTS		; Adresse in X/Y retour
   788                          
   789                          NOSTRING
   790  c77a a900               	LDA #0		; L�nge 0 
   791  c77c 8552               	STA LEN		; kein String gefunden
   792  c77e 60                 	RTS		; Z=1
   793                          
   794                          ;
   795                          ; CORR - String-Adresse im Descriptor korrigieren
   796                          ;
   797                          ; ( STRADR, STAT -> )
   798                          ;
   799  c77f a557               CORR	LDA STAT	; String-Status
   800  c781 2903               	AND #%011	; nur 2 Bits
   801  c783 a8                 	TAY		; Lage des Descriptors
   802  c784 a54e               	LDA NEWPTR	;
   803  c786 9122               	STA (STRDP),Y	; ... bei SDS
   804  c788 c8                 	INY		; ... und Array verschieden!
   805  c789 a54f               	LDA NEWPTR+1
   806  c78b 9122               	STA (STRDP),Y
   807  c78d 60                 	RTS
   808                          
   809                          !ifndef basic_patch {
   810                          
   811                            !ifndef orig_movblock {
   812                          	; Die optimiert Routine
   813                          	;
   814                          	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
   815                          	; ist beim Ausblenden des KERNALs, auch immer BASIC
   816                          	; ausgeblendet ist, nicht verf�gbar. Daher eine
   817                          	; extra Routine.
   818                          
   819                          	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
   820                          	; F�r das Kopieren zu niedrigen Adressen ist es �berlappungsrobust.
   821                          	; Eingabe: $5F/$60 Startadresse Quelle
   822                          	;	   $5A/$5B Endadresse+1 der Quelle
   823                          	;	   $58/$59 Startadresse Ziel
   824                          	; Zerst�rt: A, X, Y
   825                          	; Ausgabe: $58/$59 hat den Wert Zielendadresse+1
   826                          	;          X = 0
   827                          	;	   Y = 0 (wenn die Bereichsl�nge > 0 ist)
   828                          	;	   Z-Flag = 1
   829                          MOVBLOCK
   830  c78e 38                         SEC
   831  c78f a55a                       LDA $5A       ; Quelle Endadresse low
   832  c791 e55f                       SBC $5F       ; minus Quelle Startadresse low
   833  c793 a8                         TAY           ; L�nge low
   834  c794 a55b                       LDA $5B       ; Endadresse high
   835  c796 e560                       SBC $60       ; minus Startadresse high
   836  c798 aa                         TAX           ; L�nge high, als DEC-DEC-Counter
   837  c799 98                         TYA           ; L�nge low
   838  c79a f019                       BEQ copy      ; wenn 0, dann geht es einfach ...
   839  c79c 18                         CLC           ; L�nge in A
   840  c79d 655f                       ADC $5F       ; Quellstartadresse um Low-Byte-Offset
   841  c79f 855f                       STA $5F       ; korrigieren: -(-L�nge) -> +L�nge
   842  c7a1 b002                       BCS +         ; da eigentlich Subtraktion, �berlauf
   843  c7a3 c660                       DEC $60       ; entspr. in Startadresse high korr.
   844                          +
   845  c7a5 98                         TYA           ; L�nge low
   846  c7a6 18                         CLC
   847  c7a7 6558                       ADC $58       ; Zielstartadresse um Low-Bye-Offset
   848  c7a9 8558                       STA $58       ; korrigieren: -(-L�nge) -> +L�nge
   849  c7ab b002                       BCS +         ; da eigentlich Subtraktion, �berlauf
   850  c7ad c659                       DEC $59       ; entspr. in Startadresse high korr.
   851                          +
   852  c7af e8                 	INX           ; Page-Z�hler (alle ganzen und einen teilweise)
   853  c7b0 98                 	TYA           ; L�nge low
   854  c7b1 49ff               	EOR #$FF      ; negieren (2er-Komplement):
   855  c7b3 a8                 	TAY           ; NOT(X)+1
   856  c7b4 c8                 	INY
   857  c7b5 b15f               copy    LDA ($5F),Y   ; Quelle auf
   858  c7b7 9158                       STA ($58),Y   ; Ziel kopieren, aufsteigend
   859  c7b9 c8                         INY
   860  c7ba d0f9                       BNE copy
   861  c7bc e660                       INC $60       ; Quelle high
   862  c7be e659                       INC $59       ; Ziel high
   863  c7c0 ca                         DEX           ; Blockz�hler 
   864  c7c1 d0f2                       BNE copy
   865  c7c3 60                         RTS
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
   876                          	; F�r das Kopieren zu h�heren Adressen ist es �berlappungsrobust.
   877                          	; Eingabe: $5F/$60 Startadresse Quelle
   878                          	;	   $5A/$5B Endadresse+1 der Quelle
   879                          	;	   $58/$59 Endadresse+1 des Ziels
   880                          	; Zerst�rt: $22, A, X, Y
   881                          	; Ausgabe: $58/$59 hat den Wert des 1. Bytes des Zielbereichs.
   882                          	;          X = 0
   883                          	;	   Y = 0 (wenn die Bereichsl�nge > 0 ist)
   884                          	;	   Z-Flag = 1
   885                          MOVBLOCK
   886                                  SEC
   887                                  LDA $5A       ; Endadresse low
   888                                  SBC $5F       ; minus Startadresse low
   889                                  STA $22       ; L�nge low
   890                                  TAY
   891                                  LDA $5B       ; Endadresse high
   892                                  SBC $60       ; minus Startadresse high
   893                                  TAX           ; L�nge high
   894                                  INX           ; L�nge als DEC-DEC-Counter
   895                                  TYA           ; L�nge low
   896                                  BEQ +         ; wenn 0, dann 
   897                                  LDA $5A       ; Endadresse um Low-Byte-Offset
   898                                  SEC           ; der L�nge korrigieren
   899                                  SBC $22       ; L�nge low
   900                                  STA $5A       ;
   901                                  BCS ++
   902                                  DEC $5B       ; Endadresse high korr.
   903                                  SEC
   904                          ++      LDA $58       ; Zieladresse um Low-Bye-Offset
   905                                  SBC $22       ; der L�nge korrigieren
   906                                  STA $58
   907                                  BCS +++	      ; High byte
   908                                  DEC $59       ; 
   909                                  BCC +++
   910                          -       LDA ($5A),Y   ; Quelle auf
   911                                  STA ($58),Y   ; Ziel kopieren von h�here
   912                          --		      ; auf niedrigere Adressen
   913                          +++     DEY
   914                                  BNE -
   915                                  LDA ($5A),Y   ; Quelle
   916                                  STA ($58),Y   ; Ziel
   917                          +       DEC $5B       ; Quelle high
   918                                  DEC $59       ; Ziel high
   919                                  DEX           ; Blockz�hler 
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
   930  c7c4 00                 ORIGVID !byte 0		; originale Video der Markenposition
   931  c7c5 00                 ORIGCOL !byte 0		; originale Farbe der Markenposition
   932                          !ifndef basic_patch {
   933  c7c6 0000               ORIGIRQ !byte 0,0	; originaler IRQ-Vektor
   934                          }
   935  c7c8 00                 SAVE	!byte 0		; gesicherte ZP-Variablen
   936                          *=*+ZPLEN-1
   937                          
   938                          
