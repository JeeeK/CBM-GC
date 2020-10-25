
; ******** Source: jk-gc.asm
     1                          ;
     2                          ; *************************
     3                          ; *  GARBAGE  COLLECTION  *
     4                          ; *   von Johann Klasek   *
     5                          ; *   j AT klasek DOT at  *
     6                          ; * 1985-12-27 VERS. 1.1  *
     7                          ; * 2013-11-24 VERS. 2.0  *
     8                          ; * 2019-02-15 VERS. 2.1  *
     9                          ; * 2020-10-10 VERS. 2.2  *
    10                          ; *************************
    11                          ;
    12                          ; Aufruf: SYS ...
    13                          ; Räumt Stringspeicher auf
    14                          ; Es werden nur jene Speicherstellen
    15                          ; benutzt, die auch die normale
    16                          ; GC verwendet; alle anderen
    17                          ; werden wieder restauriert.
    18                          
    19                          ; Start des Codes ...
    20                          
    21                          !ifdef start {
    22                          	*=start
    23                          } else {
    24                          	*= $C500
    25                          }
    26                          
    27                          ; Optionen:
    28                          
    29                          ; Entweder als Patch des BASIC-ROMs in der RAM-Kopie von $A000 bis $BFFF
    30                          ; oder GC via IRQ anspringen (auskommentieren):
    31                          ;basic_patch = 1
    32                          ;
    33                          ;	Ad Platzverbrauch: 
    34                          ;	 * Der normal Hook in BASIC-RAM-Kopie braucht 48 Bytes.
    35                          ;	 * Der IRQ-Hook ist sehr aufwendig und braucht 244 Bytes, weil
    36                          ;	   hier auch noch 60 Bytes für die Blockkopierroutine
    37                          ;	   benötigt wird, da hier die ROM-Variante nicht genutzt
    38                          ; 	   werden kann!
    39                          
    40                          ; Wenn aktiv, wird als Puffer das RAM unter dem BASIC-ROM verwendet,
    41                          ; sonst jenes unter dem KERNAL. So kann etwa das RAM unter dem KERNAL
    42                          ; für Grafikzwecke verwendet werden ...
    43                          ;basic_rom_buffer = 1
    44                          
    45                          ; Original MOVBLOCK-Routine (Kopie aus dem ROM) verwenden (nur
    46                          ; im Fall, wenn basic_patch nicht aktiviert ist!).
    47                          ; Sonst wird ein optimierte Variante verwendet.
    48                          ;orig_movblock = 1
    49                          
    50                          
    51                          ; Optionsabhängigkeiten
    52                          
    53                          ; Wenn BASIC im RAM gepatcht wird, immer die originale MOVBLOCK-Routine
    54                          ; verwenden!
    55                          !ifdef basic_patch {
    56                          orig_movblock = 1
    57                          }
    58                          
    59                          ; In der BASIC-Patch-Variante darf der Puffer nicht auch im RAM unter
    60                          ; dem BASIC-ROM sein!
    61                          !ifdef basic_patch {
    62                            !ifdef basic_rom_buffer {
    63                              !error "Ungültige Option: basic_rom_buffer und basic_patch dürfen nicht gleichzeitig aktiv sein!"
    64                            }
    65                          }
    66                          
    67                          
    68                          ; BASIC Systemvariablen
    69                          
    70                          TOSS     = $19		; Top of Stringstack
    71                          EOSS     = $22		; End of Stringstack +1
    72                          TSSP     = $16		; Temp. Stringstack Pointer
    73                          
    74                          VARTAB   = $2D		; BASIC-Programmende = Variablenanfang
    75                          ARYTAB   = $2F		; Variablenende = Arraybereichanfang
    76                          STREND   = $31		; Arraybereichende = unterste String-Heap-Adresse
    77                          FRETOP   = $33		; aktuelle String-Heap-Adresse
    78                          MEMSIZ   = $37		; höchste RAM-Adresse für BASIC, Start
    79                          			; des nach unten wachsenden String-Heaps
    80                          MEMBEG   = STREND	; Memory begin = STREND
    81                          MEMEND   = MEMSIZ	; Memory end
    82                          
    83                          V_IRQ    = $0314	; IRQ-Vektor, 2 Bytes
    84                          
    85                          ; Variablen
    86                          
    87                          STRPTR   = FRETOP	; String-Pointer = FRETOP
    88                          STRDP    = $22		; String-Descriptor-Address
    89                          			; $22 wird von originalem MOVBLOCK zerstört!
    90                          RNGBEG   = $4C		; Bereichsanfang
    91                          NEWPTR	 = $4E		; Neuer String-Pointer
    92                          PTR      = $50		; Array-Pointer
    93                          LEN      = $52		; String-Length
    94                          ; $54-$56 belegt
    95                          STAT     = $57		; String-Status, Werte siehe
    96                          			; STAT_* weiter unten.
    97                          ; $58-5B wird von MOVBLOCK zerstört!
    98                          STRADR   = $58		; String-Address (temp.)
    99                          			; (MOVBLOCK: Zielblockende+1 bzw. Zielblockanfang)
   100                          RNGEND   = $5D		; Bereichsende
   101                          BUFPTR   = $5F		; Buffer-Pointer
   102                          			; (MOVBLOCK: Quellblockanfang!)
   103                          
   104                          CPTR     = $22		; Pointer für Install-Routine
   105                          
   106                          ; zu rettender Zeropage-Bereich
   107                          ZPSTART  = $4C		; 1. zu rettende
   108                          ZPEND    = $52		; letzte zu rettende Stelle
   109                          ZPLEN    = ZPEND-ZPSTART+1
   110                          			; Anzahl zu rettenden Bytes
   111                          
   112                          ; Konstanten
   113                          
   114                          ; für Variabe STAT (String-Status):
   115                          STAT_SDS = 5		; String-Descriptor-Stack
   116                          STAT_VAR = 3		; einfache Variablen
   117                          STAT_ARY = 1		; Array
   118                          
   119                          
   120                          ; Speicherorte
   121                          
   122                          GARBCOL  = $B526	; Einsprungpunkt der GC
   123                          
   124                          BASIC    = $A000        ; BASIC-ROM
   125                          KERNAL   = $E000        ; KERNAL-ROM
   126                          ROMSIZE  = $2000        ; ROM-Länge 8 KByte8
   127                          
   128                          ; Puffer
   129                          !ifndef basic_rom_buffer {
   130                          BUF	 = KERNAL	; Puffer unter KERNAL-ROM
   131                          } else {
   132                          BUF	 = BASIC	; Puffer unter BASIC-ROM
   133                          }
   134                          BUFSIZE  = ROMSIZE	; Puffergröße
   135                          
   136                          ; I/O-BEREICHE/-ADRESSEN
   137                          
   138                          VIDBASE  = $0400	; Video RAM
   139                          COLBASE  = $D800	; Color RAM
   140                          
   141                          MARKE    = "*"		; Aktivitätsanzeige
   142                          MARKEFARBE = 9		; rot
   143                          MARKEOFF = 40*25-1	; Markenposition
   144                          MARKEVID = VIDBASE+MARKEOFF
   145                          MARKECOL = COLBASE+MARKEOFF
   146                          
   147                          PROCPORT = $01		; Prozessorport
   148                          MEMROM   = %00110111	; Basic+Kernal ROM, $37
   149                          MEMBAS   = %00110110	; Basic RAM+Kernal ROM, $34
   150                          MEMRAM   = %00110101	; Basic+Kernal RAM, $35
   151                          
   152                          
   153                          ; Debugging
   154                          
   155                          ;!set debug=1
   156                          
   157                          
   158                          ; Installer
   159                          
   160                          INSTALL
   161                          
   162  c500 2c0000             	BIT $0000	; Argument hält die Kennung
   163                          			; quasi als NOP-Befehlt.
   164                          	* = *-2		; Operand überschreiben!
   165  c501 4743               	!text "GC"	; Kennung für Ladetest
   166                          			; Für alle Varianten gleich!
   167                          !ifdef basic_patch {
   168                          
   169                          	; BASIC-ROM/RAM-Patch-Einbindung
   170                          
   171                          MOVBLOCK = $A3BF	; Block verschieben aus BASIC-ROM
   172                          			; zerstört $58/$59/$5A/$5B/$22
   173                          
   174                          	; BASIC ins RAM kopieren, um die GC-Routine
   175                          	; zu patchen ...
   176  c503 a937               	LDA #MEMROM
   177  c505 8501               	STA PROCPORT	; alles ROM (also vom ROM kopieren)
   178  c507 a000               	LDY #<BASIC	; ROM-Beginn
   179  c509 8422               	STY CPTR
   180  c50b a9a0               	LDA #>BASIC
   181  c50d 8523               	STA CPTR+1	; BASIC-ROM Anfang
   182  c50f a220               	LDX #>($2000)	; BASIC-ROM Länge in Pages
   183  c511 b122               CPYROM	LDA (CPTR),Y	; ROM lesen
   184  c513 9122               	STA (CPTR),Y	; RAM schreiben
   185  c515 c8                 	INY
   186  c516 d0f9               	BNE CPYROM
   187  c518 e623               	INC CPTR+1	; nächste Page
   188  c51a ca                 	DEX		; Page-Zähler
   189  c51b d0f4               	BNE CPYROM
   190  c51d a501               	LDA PROCPORT	; auf RAM umschalten
   191  c51f 29fe               	AND #%11111110	; "BASIC-ROM aus"-Maske
   192  c521 8501               	STA PROCPORT
   193  c523 a933               	LDA #<COLLECT	; "JMP COLLECT"
   194  c525 8d27b5             	STA GARBCOL+1	; patchen ...
   195  c528 a9c5               	LDA #>COLLECT
   196  c52a 8d28b5             	STA GARBCOL+2
   197  c52d a94c               	LDA #$4C	; JMP-Opcode
   198  c52f 8d26b5             	STA GARBCOL
   199  c532 60                 	RTS
   200                          } else {
   201                          
   202                          	; IRQ-Einbindung
   203                          
   204                          	SEI
   205                          	LDA V_IRQ	; bisherige IRQ-Routine aufheben
   206                          	LDX V_IRQ+1
   207                          	CMP #<(IRQ)
   208                          	BNE HOOK
   209                          	CPX #>(IRQ)
   210                          	BEQ INSTEXIT	; Vektor bereits installiert
   211                          HOOK
   212                          	STA ORIGIRQ
   213                          	STX ORIGIRQ+1
   214                          	LDA #<(IRQ)	; GC-Routine einhängen ...
   215                          	LDX #>(IRQ)
   216                          	STA V_IRQ
   217                          	STX V_IRQ+1
   218                          INSTEXIT
   219                          	CLI
   220                          	RTS
   221                          
   222                          ; In der IRQ-Routine wird festgestellt, ob die alte GC
   223                          ; gerade aktiv ist, was dann der Fall ist, wenn
   224                          ;   * am Stack der Caller-PC der Open-Space-for-Memory-Routine ($B62B-1) ist,
   225                          ;   * am Stack einer der drei Caller-PC der Search-for-Next-String-Routine
   226                          ;     ist,
   227                          ;   * der PC im Bereich von $B526 bis $B63D liegt.
   228                          ; Gewisse Zustände müssen dabei allerings korrigiert werden.
   229                          ; Korrekturfälle:
   230                          ;   1. Wenn der PC im Bereich von GC_CRIT_START bis GC_CRIT_END liegt,
   231                          ;      ist der Descriptor inkonsistent -> High-Byte setzen:
   232                          ;         LDY $55
   233                          ;         INY
   234                          ;         INY
   235                          ;         LDA $59
   236                          ;         STA ($4E),y
   237                          ;      Bei Punkt 3 bereits der gesamte Descriptor
   238                          ;      korrigiert (nach dem Verschieben des Strings),
   239                          ;      aber es bringt faktisch keine Ersparnis diese Routine
   240                          ;      mitzuverwenden.
   241                          ;   2. Wenn der PC im Bereich von GC_PHP_START bis GC_PHP_END liegt,
   242                          ;      ist der Stack inkonsistent -> ein Byte vom Stack nehmen.
   243                          ;   3. Wenn die Subroutine "Open Space in Memory" unterbrochen
   244                          ;      wurde (an der Aufruferadresse erkannt), dann muss der
   245                          ;      gerade kopierte String fertig übertragen werden, daher
   246                          ;      kehrt das RTI wieder zur Routine zurück und erst mit
   247                          ;      dem RTS wird über die manipulierte Adresse am Stack
   248                          ;      erst zur Korrektur des Descriptors und dann zur neuen GC
   249                          ;      gesprungen.
   250                          ;   4. Wenn in Subroutine "Search for Next String" unterbrochen
   251                          ;      wurde (an den drei möglichen Aufrufadressen erkannt),
   252                          ;      befindet sich am Stack die Adresse des Aufrufers, wobei
   253                          ;      am Stack die RTI-Rückkehradresse auf eine Korrektur-
   254                          ;      routine gelenkt wird, die 2 Bytes vom Stack nimmt und
   255                          ;      zur neuen GC verzweigt.
   256                          ;
   257                          ; Sonst kann, wenn der Unterbrechungs-PC im GC-Code-Bereich liegt,
   258                          ; direkt zu neuen GC verzweigt werden.
   259                          ;
   260                          ; Egal wo die GC unterbrochen wird, Top of String-Heap ($33/$34)
   261                          ; ist zwar bereits zerstört, aber das ist egal, weil
   262                          ; die neue GC diesen ohnehin neu ermittelt.
   263                          ; 
   264                          
   265                          GC_START      = $B526	; PC in diesem Bereich -> GC aktiv
   266                          GC_END        = $B63C
   267                          GC_CRIT_START = $B632	; PC in diesem Bereich -> 
   268                          			; Descriptor muss korrigiert werden!
   269                          GC_CRIT_END   = $B638
   270                          GC_PHP_START  = $B58A	; PC in diesem Bereich -> 
   271                          			; SR (von PHP) vom Stack nehmen!
   272                          GC_PHP_END    = $B598	; hier ist das PLP
   273                          
   274                          CALLER_OPSP   = $B628+2	; PC-1 der Return-Adresse des JSR $A3BF
   275                          			; Aufrufs (Open Space for Memory) bei $B628.
   276                          CALLER_SNS1   = $B561+2 ; PC-1 Return-Adressen der Aufrufstellen
   277                          CALLER_SNS2   = $B548+2 ; der "Search for Next String"-Routine
   278                          CALLER_SNS3   = $B5B8+2
   279                          
   280                          IRQ
   281                          	SEI
   282                          
   283                          	; IRQ-Stackframe:
   284                          	; $104,X Status-Reg
   285                          	; $105,X Low-PC
   286                          	; $106,X High-PC
   287                          	; ev. aufrufene Routine, falls in einer Subroutine:
   288                          	; $107,X Low-Byte des (Aufruf-PC)-1
   289                          	; $108,X High-Byte des (Aufruf-PC)-1
   290                          
   291                          	LDA $108,X	; Aufrufer PC high
   292                          	TAY
   293                          	LDA $107,X	; Aufrufer PC low
   294                          	; Aufruf-PC-1 in A/Y
   295                          			; sind wir in "Open Space"?
   296                          	CMP #<(CALLER_OPSP)
   297                          	BNE CHK_SNS
   298                          	CPY #>(CALLER_OPSP)
   299                          	BNE CHK_SNS
   300                          IN_OPSP
   301                          	; mit RTI zurück in die unterbrochene
   302                          	; Kopierroutine, damit der String
   303                          	; fertig übertragen wird, aber mit
   304                          	; dem RTS dort, geht es dann zu
   305                          	; Descriptor-Korrekturroutine
   306                          	; die dann endlich die neue CG aufrufen
   307                          	; darf.
   308                          	LDA #<(CORR_STR-1)
   309                          	STA $107,X	; wegen RTS minus 1
   310                          	LDA #>(CORR_STR-1)
   311                          	STA $108,X	; immer >0
   312                          	BNE CONT	; immer, mit RTI zurück
   313                          
   314                          CORR_STR
   315                          	LDY $55		; Offset des Descriptors
   316                          	INY		; String-Länge
   317                          	LDA $58		; String-Adresse low
   318                          	STA ($4E),Y	; in den Descriptor
   319                          	INY
   320                          	LDA $59		; String-Adresse high
   321                          	STA ($4E),Y	; nun String-Adresse high setzen
   322                          	BNE START_COLLECT
   323                          			; immer, weil immer >0
   324                          
   325                          	
   326                          CHK_SNS			; sind wir in "Search for Next String"?
   327                          	CPY #>(CALLER_SNS1)
   328                          			; High-Byte ist für alle drei Adressen gleich!
   329                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   330                          	  !error "High-Byte von CALLER_SNS* sind verschieden. Sie müssen gleich sein!"
   331                          	}
   332                          	BNE CHK_PC
   333                          	CMP #<(CALLER_SNS1)
   334                          	BEQ IN_SUB
   335                          	CMP #<(CALLER_SNS2)
   336                          	BEQ IN_SUB
   337                          	CMP #<(CALLER_SNS3)
   338                          	BNE CHK_PC
   339                          
   340                          IN_SUB
   341                          	LDA #<(SKIPSUB) ; Unterbrechungs-PC umbiegen ...
   342                          	STA $105,X	; Low-Byte
   343                          	LDA #>(SKIPSUB)
   344                          	STA $106,X	; High-Byte
   345                          			; mit dem RTI geht es zu SKIPSUB wo
   346                          			; der Caller vom Stack genommen wird.
   347                          
   348                          CONT	JMP (ORIGIRQ)	; weiter zum Original-Code (Kette).
   349                          
   350                          CHK_PC
   351                          	LDA $106,X	; Unterbrechungs-PC
   352                          	TAY		; High-Byte
   353                          	LDA $105,X	; Low-Byte
   354                          	CPY #>(GC_START)
   355                          	BCC CONT	; vor der GC
   356                          	BNE +		; über GC-Anfang
   357                          	CMP #<(GC_START)
   358                          	BCC CONT	; unterhalb GC
   359                          +	CPY #>(GC_END+1)
   360                          	BCC ++		; in der GC!
   361                          	BNE CONT	; über der GC
   362                          	CMP #<(GC_END+1)
   363                          	BCS CONT	; über der GC
   364                          ++
   365                          	; Die GC wurde unterbrochen!
   366                          	; In irgendwelchen Spezialbereichen, wo wir mehr machen müssen?
   367                          
   368                          	; in PHP/PLP Sektion?
   369                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   370                          	  !error "High-Byte von GC_PHP_START und GC_PHP_END sind nicht gleich!"
   371                          	}
   372                          	CPY #>(GC_PHP_START)
   373                          	BNE +		; nicht in richter Page, nächster Bereichstest
   374                          	CMP #<(GC_PHP_START)
   375                          	BCC +		; unterhalb, keine Stack-Korrektur
   376                          	CMP #<(GC_PHP_END+1)
   377                          	BCS +		; darüber, keine Stack-Korrektur
   378                          	; Stack-Korrektur:
   379                          	PLA		; SR vom Stack nehmen, setzt ev. Flags N,Z aber nicht C
   380                          	BCC TO_COLLECT	; C immer 0, mittels RTI zu COLLECT
   381                          +
   382                          	; in kritischer Sektion?
   383                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   384                          	  !error "High-Byte von GC_CRIT_START und GC_CRIT_END sind nicht gleich!"
   385                          	}
   386                          	CPY #>(GC_CRIT_START)
   387                          	BNE +		; nicht in richtiger Page, dann normal weiter
   388                          	CMP #<(GC_CRIT_START)
   389                          	BCC +		; unterhalb, keine Korrektur
   390                          	CMP #<(GC_CRIT_END+1)
   391                          	BCS +		; darüber, keine Korrektur
   392                          
   393                          	; Korrektur, High Byte der String-Adresse in Descriptor setzen,
   394                          	; weil bereits das Low-Byte gesetzt wurde. Die Adresse im Descriptor
   395                          	; wäre sonst inkonsistent!
   396                          	LDY $55		; Offset des Descriptors
   397                          	INY		; String-Länge
   398                          	INY		; String-Adresse-Low (ist schon gesetzt!)
   399                          	LDA $59
   400                          	STA ($4E),Y	; nun String-Adresse-High setzen
   401                          
   402                          	; Der obige Teil könnte theoretisch auch die Descriptorkorrektur
   403                          	; bei CORR_STR nutzen (die aber nicht im IRQ-Kontext läuft und
   404                          	; deswegen hier nicht direkt angesprungen werden kann). Statt
   405                          	; über RTI mit Fortsetzung bei START_COLLECT, müsste 
   406                          	; alternativ CORR_STR verwendet werden. Doch eine entsprechende
   407                          	; Umsetzung würde auch 7 Bytes erfordern, die gegenüber dem 
   408                          	; obigen Code nur 1 Byte spart. Der Klarheit wegen wird hier auf
   409                          	; diesen Variante verzichtet.
   410                          +
   411                          	; mittels RTI COLLECT aufrufen:
   412                          TO_COLLECT
   413                          	LDA #<(START_COLLECT)
   414                          			; Rückkehradresse für RTI am Stack umbiegen ...
   415                          	STA $0105,X	; Low-Byte
   416                          	LDA #>(START_COLLECT)
   417                          	STA $0106,X	; High-Byte
   418                          	BNE CONT	; IRQ behandeln, RTI startet dann die neue GC ...
   419                          
   420                          SKIPSUB			; Open-Space- oder Search-for-Next-String-Routine abgebrochen:
   421                          	PLA		; kommt direkt von RTI, Aufruf-PC (für RTS) in alter GC
   422                          	PLA		; verwerfen und die neue GC übernimmt ...
   423                          			; direkt durch zu COLLECT
   424                          START_COLLECT
   425                          	LDA #3
   426                          	STA $53		; Step-Size für nächsten Descriptor auf
   427                          			; Ausgangswert setzen. (Wird von alter GC
   428                          			; nicht initialisiert!)
   429                          }
   430                          
   431                          ; *** Garbage Collector
   432                          
   433                          COLLECT
   434                          !ifdef debug {
   435                          	JSR gra_on
   436                          }
   437  c533 ade707             	LDA MARKEVID	; Kontrollanzeige Bildschirm
   438  c536 8d03c7             	STA ORIGVID
   439  c539 a92a               	LDA #MARKE
   440  c53b 8de707             	STA MARKEVID	; Marke: Zeichen
   441  c53e ade7db             	LDA MARKECOL	; sichern
   442  c541 8d04c7             	STA ORIGCOL
   443  c544 a909               	LDA #MARKEFARBE
   444  c546 8de7db             	STA MARKECOL	; Marke: Farbe sichern
   445                          
   446  c549 a207               	LDX #ZPLEN	; Zeropage-Reg.
   447  c54b b54b               SAVLOOP	LDA ZPSTART-1,X	; retten
   448  c54d 9d04c7             	STA SAVE-1,X
   449  c550 ca                 	DEX
   450  c551 d0f8               	BNE SAVLOOP
   451                          
   452  c553 a537               	LDA MEMEND	; String-Pointer
   453  c555 a638               	LDX MEMEND+1	; und Bereichanfang 
   454  c557 8533               	STA STRPTR	; auf Speicherende
   455  c559 8634               	STX STRPTR+1	; setzen.
   456  c55b 854c               	STA RNGBEG
   457  c55d 864d               	STX RNGBEG+1
   458                          
   459                          ; *** Nächster zu betrachtender Bereich am String-Heap
   460                          
   461                          ;                        STRADR
   462                          ;       +-------------------------------------+
   463                          ;       |                                     |
   464                          ;       |                                     V
   465                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   466                          ;   |L|PTR|      |      noch nicht       | gesuchte | frei | behandelte |
   467                          ;   | |   |      |  behandelte Strings   | Strings  |      |   Strings  |
   468                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   469                          ;    ^            ^                       ^          ^      ^            ^
   470                          ;    |            |                       |          |      |            |
   471                          ;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
   472                          ;                                                           =FRETOP
   473                          ;   SDS,VAR,ARY  |<-------------------- String-Heap -------------------->|
   474                          ;
   475                          ; Der Bereich RNGBEG bis RNGEND (gesuchte Strings) ist immer um 256 Bytes 
   476                          ; kleiner als der Pufferbereich, da am Ende des Bereichs ein String beginnen
   477                          ; könnte, der max. 254 Bytes das Bereichsende überragen könnte. Dieser 
   478                          ; "Überhang" muss im Puffer Platz haben und dort reserviert sein!
   479                          
   480                          NEXTBLOCK
   481  c55f a533               	LDA STRPTR	; NEWPTR parallel mit
   482  c561 854e               	STA NEWPTR	; BUFPTR mitziehen ...
   483  c563 a534               	LDA STRPTR+1
   484  c565 854f               	STA NEWPTR+1
   485  c567 a64c               	LDX RNGBEG	; Bereich war zuletzt
   486  c569 a54d               	LDA RNGBEG+1	; String-Heap-Ende?
   487  c56b e431               	CPX STREND
   488  c56d d004               	BNE +
   489  c56f c532               	CMP STREND+1
   490  c571 f01b               	BEQ EXIT	; ja -> fertig
   491                          +
   492  c573 865d               	STX RNGEND	; um Pufferlänge - 256
   493  c575 855e               	STA RNGEND+1	; nach unten verlegen.
   494                          	!if <BUFSIZE > 0 {
   495                          	  !error "BUFSIZE ist nicht ein Vielfaches von 256 ($100)!"
   496                          	}
   497  c577 38                 	SEC		
   498  c578 e91f               	SBC #(>BUFSIZE-1) ; Bereichslänge in Pages,
   499                          			; kann um 254 Bytes überragt werden!
   500  c57a 9008               	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
   501  c57c 854d               	STA RNGBEG+1
   502  c57e e431               	CPX STREND	; Ende des String-Heaps erreicht?
   503  c580 e532               	SBC STREND+1
   504  c582 b021               	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
   505                          LASTRANGE
   506  c584 a531               	LDA STREND	; Bereichanfang =
   507  c586 a632               	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
   508  c588 854c               	STA RNGBEG	; 
   509  c58a 864d               	STX RNGBEG+1	; 
   510  c58c d017               	BNE STRINRANGE	; immer, weil High-Byte >0
   511                          
   512                          
   513                          ; *** Ende der Garbage Collection
   514                          
   515                          EXIT
   516  c58e a207               	LDX #ZPLEN
   517  c590 bd04c7             RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
   518  c593 954b               	STA ZPSTART-1,X	; restaurieren.
   519  c595 ca                 	DEX
   520  c596 d0f8               	BNE RESLOOP
   521                          
   522  c598 ad03c7             	LDA ORIGVID	; Kontrollanzeige löschen
   523  c59b 8de707             	STA MARKEVID	; und alten Zustand wieder
   524  c59e ad04c7             	LDA ORIGCOL	; herstellen.
   525  c5a1 8de7db             	STA MARKECOL
   526                          !ifdef debug {
   527                          	JSR gra_off
   528                          }
   529  c5a4 60                 	RTS
   530                          
   531                          
   532                          ; *** Bereich durchgehen
   533                          
   534                          STRINRANGE
   535                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   536                          	LDA #>(BUF+BUFSIZE)
   537                          	STA BUFPTR+1
   538                          	LDA #<(BUF+BUFSIZE)
   539                          	STA BUFPTR
   540                          } else {
   541                          			; Sonderfall Pufferende bei $FFFF
   542  c5a5 a900               	LDA #0		; Buffer-Pointer auf
   543  c5a7 855f               	STA BUFPTR	; $10000 (65536) = 0
   544  c5a9 8560               	STA BUFPTR+1	; setzen.
   545                          }
   546  c5ab 38                 	SEC
   547  c5ac 24                 	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
   548                          NEXTSTR	
   549  c5ad 18                 	CLC
   550                          NEXTSTR1
   551  c5ae 201cc6             	JSR GETSA	; Nächste String-Adresse holen.
   552  c5b1 f03b               	BEQ COPYBACK	; keinen String mehr gefunden!
   553                          
   554  c5b3 98                 	TYA		; high Byte
   555  c5b4 e45d               	CPX RNGEND	; X/A >= RNGEND:
   556  c5b6 e55e               	SBC RNGEND+1	; oberhalb des Bereichs, dann
   557  c5b8 b0f3               	BCS NEXTSTR	; nächster String!
   558                          
   559  c5ba 98                 	TYA		; high Byte
   560  c5bb e44c               	CPX RNGBEG	; X/A < RNGBEG:
   561  c5bd e54d               	SBC RNGBEG+1	; unterhalb des Bereichs, dann
   562  c5bf 90ed               	BCC NEXTSTR1	; nächster String!
   563                          			; Innerhalb des Bereichs:
   564  c5c1 a55f               	LDA BUFPTR	; Pufferzeiger um
   565  c5c3 e552               	SBC LEN		; String-Länge nach unten
   566  c5c5 855f               	STA BUFPTR	; setzen.
   567  c5c7 b002               	BCS +
   568  c5c9 c660               	DEC BUFPTR+1	; Überlauf High-Byte
   569                          
   570  c5cb 8459               +	STY STRADR+1	; String-Adresse abspeichern
   571  c5cd 8658               	STX STRADR	; für Kopieraktion.
   572                          
   573  c5cf a452               	LDY LEN		; String-Länge (> 0)
   574  c5d1 d004               	BNE NBENTRY	; immer, mit Dekrement beginnen!
   575  c5d3 b158               NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
   576  c5d5 915f               	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
   577  c5d7 88                 NBENTRY	DEY		; schreibt ins RAM unters ROM!
   578  c5d8 d0f9               	BNE NEXTBYT
   579                          LEN1
   580  c5da b158               	LDA (STRADR),Y	; Das 0. Byte extra
   581  c5dc 915f               	STA (BUFPTR),Y	; übertragen
   582                          
   583  c5de 38                 	SEC		; Neue String-Adresse:
   584  c5df a54e               	LDA NEWPTR	; Einfach den Pointer
   585  c5e1 e552               	SBC LEN		; mitziehen, ebenso um
   586  c5e3 854e               	STA NEWPTR	; String-Länge nach unten
   587  c5e5 b002               	BCS +		; setzen.
   588  c5e7 c64f               	DEC NEWPTR+1	; Überlauf High-Byte
   589                          +
   590  c5e9 20f4c6             	JSR CORR	; String-Adresse in Descriptor ändern.
   591                          			; Immmer Z=0,
   592  c5ec d0bf               	BNE NEXTSTR	; zum nächsten String.
   593                          
   594                          
   595                          ; *** Pufferinhalt wieder zurück auf String-Heap
   596                          
   597                          ; 0 ------------------------------------------- FFFF	
   598                          ;        Ziel                        Quelle
   599                          ;          +--------------------------+
   600                          ;          |                          |
   601                          ;          V                         /^\
   602                          ;     |||||||||||                |||||||||||
   603                          ;     ^          ^               ^          ^ 
   604                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   605                          
   606                          COPYBACK
   607                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   608                          	LDA BUFPTR	; Puffer leer ...
   609                          	CMP #<(BUF+BUFSIZE)
   610                          	BNE +
   611                          	LDA BUFPTR+1	; Wenn Pointer am Ende ...
   612                          	CMP #>(BUF+BUFSIZE)
   613                          	BEQ NOCOPY	; ist der Puffer leer, ev. nächster
   614                          +			; Bereich ...
   615                          } else {
   616                          			; Sonderfall: Pufferende bei $FFFF
   617  c5ee a55f               	LDA BUFPTR	; Puffer leer,
   618  c5f0 0560               	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
   619  c5f2 f025               	BEQ NOCOPY	; War es letzter Bereich?
   620                          }
   621                          
   622                          !ifdef orig_movblock {
   623  c5f4 a533               	LDA STRPTR	; Original MOVBLOCK braucht
   624  c5f6 a634               	LDX STRPTR+1	; Zielblockende+1
   625                          } else {
   626                          	LDA NEWPTR	; Optimiertes MOVBLOCK braucht
   627                          	LDX NEWPTR+1	; Zielblockanfang	
   628                          }
   629  c5f8 8558               	STA $58		; = STRADR
   630  c5fa 8659               	STX $59		; je nach MOVBLOCK-Variante
   631                          			; Ende+1 oder Anfang des Zielblocks
   632                          
   633                          !ifdef orig_movblock {
   634  c5fc a54e               	LDA NEWPTR
   635  c5fe a64f               	LDX NEWPTR+1	
   636                          }
   637  c600 8533               	STA STRPTR	; neues FRETOP
   638  c602 8634               	STX STRPTR+1
   639                          
   640                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   641                          	LDA #<(BUF+BUFSIZE)
   642                          	STA $5A		; Quellblockende+1
   643                          	LDA #>(BUF+BUFSIZE)
   644                          	STA $5B
   645                          } else {
   646                          			; Sonderfall Pufferende bei $FFFF
   647  c604 a900               	LDA #$00	; Quellblockende+1
   648  c606 855a               	STA $5A
   649  c608 855b               	STA $5B
   650                          }
   651                          			; Quellbockanfang = BUFPTR
   652                          
   653  c60a 78                 	SEI		; keine Interrupts zulassen, wegen
   654  c60b a501               	LDA PROCPORT	; KERNAL-ROM wegblenden
   655  c60d 48                 	PHA		; damit das Puffer-RAM zugänglich
   656  c60e a935               	LDA #MEMRAM	; wird. Auch BASIC-ROM ist damit weg!
   657                          			; Sowohl bei Puffer $F000 als auch $A000
   658                          			; werden beide ROMs ausgeblendet.
   659  c610 8501               	STA PROCPORT
   660                          
   661  c612 20bfa3             	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
   662                          			; bei IRQ-Anbindung eigene Kopie verwenden,
   663                          			; da das BASIC-ROM ausgeblendet ist!
   664                          			; Andernfalls ist ja die ROM-Kopie im RAM da.
   665                          			; Z=1
   666  c615 68                 	PLA		; ursprünglicher Zustand
   667  c616 8501               	STA PROCPORT	; KERNAL-ROM wieder aktivieren
   668  c618 58                 	CLI
   669                          NOCOPY
   670  c619 4c5fc5             	JMP NEXTBLOCK	; nächsten Bereich
   671                          
   672                          
   673                          ;
   674                          ; *** Get String - nächsten String mit Länge ungleich 0
   675                          ;
   676                          ; ( C-Flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-Flag )
   677                          ;
   678                          ; Bei C=1 wird beim SDS gestartet, sonst von der letzten
   679                          ; Position gemäß STRDP und String-Status STAT.
   680                          ; Das Z-Flag ist gesetzt, wenn kein String mehr
   681                          ; vorhanden ist, sonst in X/Y die Adresse und in LEN
   682                          ; die Stringlänge.
   683                          
   684  c61c 9063               GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
   685                          			; sonst Start bei SDS ...
   686                          
   687                          ; *** String-Descriptor-Stack (SDS): TOSS bis TSSP
   688                          ;
   689                          ;    +-------------+
   690                          ;    |             V
   691                          ;    |    belegt->|<-frei
   692                          ;   +-+     +-----+-----+-----+
   693                          ;   | |     |S|L|H|S|L|H|S|L|H|
   694                          ;   +-+     +-----+-----+-----+
   695                          ;    ^       ^     ^     ^     ^
   696                          ;    $16     $19   $1C   $1F   $22
   697                          ;    TSSP    TOSS
   698                          
   699                          DESCSTACK
   700  c61e a000               	LDY #0
   701  c620 8423               	STY STRDP+1	; Descriptor auf
   702  c622 a905               	LDA #STAT_SDS	; Status: SDS
   703  c624 8557               	STA STAT
   704  c626 a219               	LDX #TOSS	; SDS Start
   705  c628 d005               	BNE ISDSTEND	; immer verzweigen
   706  c62a a622               DSTACK	LDX STRDP
   707  c62c e8                 NEXTDST	INX		; nächster Descriptor
   708  c62d e8                 	INX
   709  c62e e8                 	INX
   710                          ISDSTEND
   711  c62f e416               	CPX TSSP	; Stack durch?
   712  c631 f010               	BEQ VARS
   713  c633 b500               	LDA 0,X		; String-Länge
   714  c635 f0f5               	BEQ NEXTDST
   715  c637 8552               	STA LEN		; Rückgabevariable
   716  c639 8622               	STX STRDP	; festhalten
   717  c63b b502               	LDA 2,X		; String-Adr. high
   718  c63d a8                 	TAY
   719  c63e b501               	LDA 1,X		; String-Adr. low
   720  c640 aa                 	TAX
   721  c641 98                 	TYA		; immer ungleich 0, Z=0
   722  c642 60                 	RTS		; Adresse in X/Y retour
   723                          
   724                          ; *** Variablen: VARTAB bis ARYTAB
   725                          
   726  c643 a52d               VARS	LDA VARTAB	; Variablenanfang
   727  c645 a62e               	LDX VARTAB+1
   728  c647 8522               	STA STRDP
   729  c649 8623               	STX STRDP+1
   730  c64b a003               	LDY #STAT_VAR	; Status: einfache Variablen
   731  c64d 8457               	STY STAT
   732  c64f d00b               	BNE ISVAREND	; immer verzweigen
   733                          VAR
   734  c651 18                 NEXTVAR	CLC		; nächste Variable
   735  c652 a522               	LDA STRDP
   736  c654 6907               	ADC #7		; Variablenlänge
   737  c656 8522               	STA STRDP
   738  c658 9002               	BCC ISVAREND
   739  c65a e623               	INC STRDP+1	; Überlauf High-Byte
   740                          ISVAREND
   741  c65c c52f               	CMP ARYTAB
   742  c65e d006               	BNE CHECKVAR
   743  c660 a623               	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
   744  c662 e430               	CPX ARYTAB+1
   745  c664 f026               	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
   746                          CHECKVAR
   747  c666 a000               	LDY #0		; Variablenname
   748  c668 b122               	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
   749  c66a 30e5               	BMI NEXTVAR	; kein String, nächste V.
   750  c66c c8                 	INY
   751  c66d b122               	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
   752  c66f 10e0               	BPL NEXTVAR	; kein String, nächste V.
   753  c671 c8                 	INY
   754  c672 b122               	LDA (STRDP),Y	; String-Länge
   755  c674 f0db               	BEQ NEXTVAR	; = 0, Nächste Variable
   756  c676 8552               	STA LEN		; Rückgabevariable
   757  c678 c8                 	INY
   758  c679 b122               	LDA (STRDP),Y	; String-Adresse low
   759  c67b aa                 	TAX
   760  c67c c8                 	INY
   761  c67d b122               	LDA (STRDP),Y	; String-Adresse high
   762  c67f a8                 	TAY		; immer ungleich 0, Z=0
   763  c680 60                 	RTS		; Adresse in X/Y retour
   764                          
   765                          CHECKTYPE
   766  c681 a557               	LDA STAT	; GETSA-Einstieg mit C=0
   767  c683 c903               	CMP #STAT_VAR	; String-Status?
   768  c685 9042               	BCC ARRAY	; =1 -> Arrays
   769  c687 f0c8               	BEQ VAR		; =3 -> Variablen
   770  c689 4c2ac6             	JMP DSTACK	; =5 -> String-Desc.-Stack
   771                          
   772                          ; *** Arrays: ARYTAB bis STREND
   773                          
   774  c68c 8550               ARRAYS	STA PTR		; A/X von Variablendurchlauf
   775  c68e 8651               	STX PTR+1	; Start Array-Array-Bereich
   776  c690 a001               	LDY #STAT_ARY
   777  c692 8457               	STY STAT	; Status: Arrays
   778                          ISARREND
   779  c694 a550               	LDA PTR
   780  c696 a651               	LDX PTR+1
   781  c698 e432               CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
   782  c69a d004                       BNE NEXTARR	; erreicht?
   783  c69c c531               	CMP STREND
   784  c69e f04f               	BEQ NOSTRING	; Arrays fertig -> kein String
   785                          NEXTARR
   786  c6a0 8522               	STA STRDP	; immer C=0
   787  c6a2 8623               	STX STRDP+1
   788  c6a4 a000               	LDY #0
   789  c6a6 b122               	LDA (STRDP),Y	; Array-Name
   790  c6a8 aa                 	TAX		; Array-Typ merken
   791  c6a9 c8                 	INY
   792  c6aa b122               	LDA (STRDP),Y
   793  c6ac 08                 	PHP		; Array-Typ merken
   794  c6ad c8                 	INY
   795  c6ae b122               	LDA (STRDP),Y	; Offset nächstes Array
   796  c6b0 6550               	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
   797  c6b2 8550               	STA PTR		; Start Folge-Array
   798  c6b4 c8                 	INY
   799  c6b5 b122               	LDA (STRDP),Y
   800  c6b7 6551               	ADC PTR+1
   801  c6b9 8551               	STA PTR+1
   802  c6bb 28                 	PLP		; Var.-Typ holen
   803  c6bc 10d6               	BPL ISARREND	; kein String-Array
   804  c6be 8a                 	TXA		; Var.-Typ holen
   805  c6bf 30d3               	BMI ISARREND	; kein String-Array
   806  c6c1 c8                 	INY
   807  c6c2 b122               	LDA (STRDP),Y	; Anzahl der Dimensionen
   808  c6c4 0a                 	ASL		; *2
   809  c6c5 6905               	ADC #5		; Offset = Dimensionen*2+5
   810                          			; C=0 solange Dim.. <= 125
   811  c6c7 d003               	BNE ADVDESC	; immer verzweigen
   812                          ARRAY			; Einstieg bei Fortsetzung
   813                          NEXTASTR
   814  c6c9 18                 	CLC
   815  c6ca a903               	LDA #3		; String-Descriptor-Länge
   816  c6cc 6522               ADVDESC	ADC STRDP	; nächten String
   817  c6ce 8522               	STA STRDP
   818  c6d0 9002               	BCC +
   819  c6d2 e623               	INC STRDP+1	; Überlauf High-Byte
   820  c6d4 c550               +	CMP PTR		; Array durch?
   821  c6d6 d006               	BNE IS0ASTR
   822  c6d8 a623               	LDX STRDP+1
   823  c6da e451               	CPX PTR+1
   824  c6dc f0ba               	BEQ CHKAEND	; A/X = PTR, Array-Ende prüfen
   825                          IS0ASTR
   826  c6de a000               	LDY #0
   827  c6e0 b122               	LDA (STRDP),Y	; String-Länge
   828  c6e2 f0e5               	BEQ NEXTASTR	; weiter im Array
   829  c6e4 8552               	STA LEN		; Rückgabevariable
   830  c6e6 c8                 	INY
   831  c6e7 b122               	LDA (STRDP),Y	; String-Adresse low
   832  c6e9 aa                 	TAX
   833  c6ea c8                 	INY
   834  c6eb b122               	LDA (STRDP),Y	; String-Adresse high
   835  c6ed a8                 	TAY		; immer ungleich 0, Z=0
   836  c6ee 60                 	RTS		; Adresse in X/Y retour
   837                          
   838                          NOSTRING
   839  c6ef a900               	LDA #0		; Länge 0 
   840  c6f1 8552               	STA LEN		; kein String gefunden
   841  c6f3 60                 	RTS		; Z=1
   842                          
   843                          ;
   844                          ; CORR - String-Adresse im Descriptor korrigieren
   845                          ;
   846                          ; ( STRADR, STAT -> )
   847                          ;
   848  c6f4 a557               CORR	LDA STAT	; String-Status
   849  c6f6 2903               	AND #%011	; nur 2 Bits
   850  c6f8 a8                 	TAY		; Lage des Descriptors
   851  c6f9 a54e               	LDA NEWPTR	;
   852  c6fb 9122               	STA (STRDP),Y	; ... bei SDS
   853  c6fd c8                 	INY		; ... und Array verschieden!
   854  c6fe a54f               	LDA NEWPTR+1
   855  c700 9122               	STA (STRDP),Y
   856  c702 60                 	RTS
   857                          
   858                          
   859                          ; optionale MOVBLOCK-Routen (für IRQ-Hook-Methode)
   860                          
   861                          !ifndef basic_patch {
   862                          
   863                            !ifndef orig_movblock {
   864                          	; Die optimierte Routine.
   865                          	;
   866                          	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
   867                          	; ist beim Ausblenden des KERNALs, auch immer BASIC
   868                          	; ausgeblendet und damit nicht verfügbar. Daher eine
   869                          	; extra Routine, die aber etwas kürzer und schneller ist.
   870                          
   871                          	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
   872                          	; Für das Kopieren zu niedrigen Adressen ist es überlappungsrobust.
   873                          	; Eingabe: $5F/$60 Startadresse Quelle
   874                          	;	   $5A/$5B Endadresse+1 der Quelle
   875                          	;	   $58/$59 Startadresse Ziel
   876                          	; Zerstört: A, X, Y
   877                          	; Ausgabe: $58/$59 hat den Wert Zielendadresse+1
   878                          	;          X = 0
   879                          	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
   880                          	;	   Z-Flag = 1
   881                          MOVBLOCK
   882                                  SEC
   883                                  LDA $5A       ; Quelle Endadresse low
   884                                  SBC $5F       ; minus Quelle Startadresse low
   885                                  TAY           ; Länge low
   886                                  LDA $5B       ; Endadresse high
   887                                  SBC $60       ; minus Startadresse high
   888                                  TAX           ; Länge high, als DEC-DEC-Counter
   889                                  TYA           ; Länge low
   890                                  BEQ copy      ; wenn 0, dann geht es einfach ...
   891                                  CLC           ; Länge in A
   892                                  ADC $5F       ; Quellstartadresse um Low-Byte-Offset
   893                                  STA $5F       ; korrigieren: -(-Länge) -> +Länge
   894                                  BCS +         ; da eigentlich Subtraktion, Überlauf
   895                                  DEC $60       ; entspr. in Startadresse high korr.
   896                          +
   897                                  TYA           ; Länge low
   898                                  CLC
   899                                  ADC $58       ; Zielstartadresse um Low-Bye-Offset
   900                                  STA $58       ; korrigieren: -(-Länge) -> +Länge
   901                                  BCS +         ; da eigentlich Subtraktion, Überlauf
   902                                  DEC $59       ; entspr. in Startadresse high korr.
   903                          +
   904                          	INX           ; Page-Zähler (alle ganzen und einen teilweise)
   905                          	TYA           ; Länge low
   906                          	EOR #$FF      ; negieren (2er-Komplement):
   907                          	TAY           ; NOT(X)+1
   908                          	INY
   909                          copy    LDA ($5F),Y   ; Quelle auf
   910                                  STA ($58),Y   ; Ziel kopieren, aufsteigend
   911                                  INY
   912                                  BNE copy
   913                                  INC $60       ; Quelle high
   914                                  INC $59       ; Ziel high
   915                                  DEX           ; Blockzähler 
   916                                  BNE copy
   917                                  RTS
   918                          	; braucht 6 Bytes weniger als die Originalroutine.
   919                          
   920                            } else {
   921                          
   922                          	; Die originale Routine aus dem ROM:
   923                          	;
   924                          	; Die "Open Space"-Routine aus dem BASIC-ROM, $A3BF
   925                          	; Da beim Ausblenden des KERNALs auch immer BASIC
   926                          	; ausgeblendet ist, kann die Routine nicht im ROM aufgerufen
   927                          	; werden!
   928                          
   929                          	; Speicherbereich ($5F/$60) bis exkl. ($5A/$5B) -> ($58/$59)
   930                          	; Für das Kopieren zu höheren Adressen ist es Überlappungsrobust.
   931                          	; Eingabe: $5F/$60 Startadresse Quelle
   932                          	;	   $5A/$5B Endadresse+1 der Quelle
   933                          	;	   $58/$59 Endadresse+1 des Ziels
   934                          	; Zerstört: $22, A, X, Y
   935                          	; Ausgabe: $58/$59 hat den Wert des 1. Bytes des Zielbereichs.
   936                          	;          X = 0
   937                          	;	   Y = 0 (wenn die Bereichslänge > 0 ist)
   938                          	;	   Z-Flag = 1
   939                          MOVBLOCK
   940                                  SEC
   941                                  LDA $5A       ; Endadresse low
   942                                  SBC $5F       ; minus Startadresse low
   943                                  STA $22       ; Länge low
   944                                  TAY
   945                                  LDA $5B       ; Endadresse high
   946                                  SBC $60       ; minus Startadresse high
   947                                  TAX           ; Länge high
   948                                  INX           ; Länge als DEC-DEC-Counter
   949                                  TYA           ; Länge low
   950                                  BEQ +         ; wenn 0, dann 
   951                                  LDA $5A       ; Endadresse um Low-Byte-Offset
   952                                  SEC           ; der Länge korrigieren
   953                                  SBC $22       ; Länge low
   954                                  STA $5A       ;
   955                                  BCS ++
   956                                  DEC $5B       ; Endadresse high korr.
   957                                  SEC
   958                          ++      LDA $58       ; Zieladresse um Low-Bye-Offset
   959                                  SBC $22       ; der Länge korrigieren
   960                                  STA $58
   961                                  BCS +++	      ; High byte
   962                                  DEC $59       ; 
   963                                  BCC +++
   964                          -       LDA ($5A),Y   ; Quelle auf
   965                                  STA ($58),Y   ; Ziel kopieren von höhere
   966                          --		      ; auf niedrigere Adressen
   967                          +++     DEY
   968                                  BNE -
   969                                  LDA ($5A),Y   ; Quelle
   970                                  STA ($58),Y   ; Ziel
   971                          +       DEC $5B       ; Quelle high
   972                                  DEC $59       ; Ziel high
   973                                  DEX           ; Blockzähler 
   974                                  BNE --
   975                                  RTS
   976                            }
   977                          }
   978                          
   979                          
   980                          !ifdef debug {
   981                          !source "debug.asm"
   982                          }
   983                          
   984  c703 00                 ORIGVID !byte 0		; originale Video der Markenposition
   985  c704 00                 ORIGCOL !byte 0		; originale Farbe der Markenposition
   986                          !ifndef basic_patch {
   987                          ORIGIRQ !byte 0,0	; originaler IRQ-Vektor
   988                          }
   989  c705 00                 SAVE	!byte 0		; gesicherte ZP-Variablen
   990                          *=*+ZPLEN-1
   991                          
   992                          
