
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
   176                          	LDA #MEMROM
   177                          	STA PROCPORT	; alles ROM (also vom ROM kopieren)
   178                          	LDY #<BASIC	; ROM-Beginn
   179                          	STY CPTR
   180                          	LDA #>BASIC
   181                          	STA CPTR+1	; BASIC-ROM Anfang
   182                          	LDX #>($2000)	; BASIC-ROM Länge in Pages
   183                          CPYROM	LDA (CPTR),Y	; ROM lesen
   184                          	STA (CPTR),Y	; RAM schreiben
   185                          	INY
   186                          	BNE CPYROM
   187                          	INC CPTR+1	; nächste Page
   188                          	DEX		; Page-Zähler
   189                          	BNE CPYROM
   190                          	LDA PROCPORT	; auf RAM umschalten
   191                          	AND #%11111110	; "BASIC-ROM aus"-Maske
   192                          	STA PROCPORT
   193                          	LDA #<COLLECT	; "JMP COLLECT"
   194                          	STA GARBCOL+1	; patchen ...
   195                          	LDA #>COLLECT
   196                          	STA GARBCOL+2
   197                          	LDA #$4C	; JMP-Opcode
   198                          	STA GARBCOL
   199                          	RTS
   200                          } else {
   201                          
   202                          	; IRQ-Einbindung
   203                          
   204  c503 78                 	SEI
   205  c504 ad1403             	LDA V_IRQ	; bisherige IRQ-Routine aufheben
   206  c507 ae1503             	LDX V_IRQ+1
   207  c50a c924               	CMP #<(IRQ)
   208  c50c d004               	BNE HOOK
   209  c50e e0c5               	CPX #>(IRQ)
   210  c510 f010               	BEQ INSTEXIT	; Vektor bereits installiert
   211                          HOOK
   212  c512 8dc9c7             	STA ORIGIRQ
   213  c515 8ecac7             	STX ORIGIRQ+1
   214  c518 a924               	LDA #<(IRQ)	; GC-Routine einhängen ...
   215  c51a a2c5               	LDX #>(IRQ)
   216  c51c 8d1403             	STA V_IRQ
   217  c51f 8e1503             	STX V_IRQ+1
   218                          INSTEXIT
   219  c522 58                 	CLI
   220  c523 60                 	RTS
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
   281  c524 78                 	SEI
   282                          
   283                          	; IRQ-Stackframe:
   284                          	; $104,X Status-Reg
   285                          	; $105,X Low-PC
   286                          	; $106,X High-PC
   287                          	; ev. aufrufene Routine, falls in einer Subroutine:
   288                          	; $107,X Low-Byte des (Aufruf-PC)-1
   289                          	; $108,X High-Byte des (Aufruf-PC)-1
   290                          
   291  c525 bd0801             	LDA $108,X	; Aufrufer PC high
   292  c528 a8                 	TAY
   293  c529 bd0701             	LDA $107,X	; Aufrufer PC low
   294                          	; Aufruf-PC-1 in A/Y
   295                          			; sind wir in "Open Space"?
   296  c52c c92a               	CMP #<(CALLER_OPSP)
   297  c52e d01e               	BNE CHK_SNS
   298  c530 c0b6               	CPY #>(CALLER_OPSP)
   299  c532 d01a               	BNE CHK_SNS
   300                          IN_OPSP
   301                          	; mit RTI zurück in die unterbrochene
   302                          	; Kopierroutine, damit der String
   303                          	; fertig übertragen wird, aber mit
   304                          	; dem RTS dort, geht es dann zu
   305                          	; Descriptor-Korrekturroutine
   306                          	; die dann endlich die neue CG aufrufen
   307                          	; darf.
   308  c534 a93f               	LDA #<(CORR_STR-1)
   309  c536 9d0701             	STA $107,X	; wegen RTS minus 1
   310  c539 a9c5               	LDA #>(CORR_STR-1)
   311  c53b 9d0801             	STA $108,X	; immer >0
   312  c53e d028               	BNE CONT	; immer, mit RTI zurück
   313                          
   314                          CORR_STR
   315  c540 a455               	LDY $55		; Offset des Descriptors
   316  c542 c8                 	INY		; String-Länge
   317  c543 a558               	LDA $58		; String-Adresse low
   318  c545 914e               	STA ($4E),Y	; in den Descriptor
   319  c547 c8                 	INY
   320  c548 a559               	LDA $59		; String-Adresse high
   321  c54a 914e               	STA ($4E),Y	; nun String-Adresse high setzen
   322  c54c d069               	BNE START_COLLECT
   323                          			; immer, weil immer >0
   324                          
   325                          	
   326                          CHK_SNS			; sind wir in "Search for Next String"?
   327  c54e c0b5               	CPY #>(CALLER_SNS1)
   328                          			; High-Byte ist für alle drei Adressen gleich!
   329                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   330                          	  !error "High-Byte von CALLER_SNS* sind verschieden. Sie müssen gleich sein!"
   331                          	}
   332  c550 d019               	BNE CHK_PC
   333  c552 c963               	CMP #<(CALLER_SNS1)
   334  c554 f008               	BEQ IN_SUB
   335  c556 c94a               	CMP #<(CALLER_SNS2)
   336  c558 f004               	BEQ IN_SUB
   337  c55a c9ba               	CMP #<(CALLER_SNS3)
   338  c55c d00d               	BNE CHK_PC
   339                          
   340                          IN_SUB
   341  c55e a9b5               	LDA #<(SKIPSUB) ; Unterbrechungs-PC umbiegen ...
   342  c560 9d0501             	STA $105,X	; Low-Byte
   343  c563 a9c5               	LDA #>(SKIPSUB)
   344  c565 9d0601             	STA $106,X	; High-Byte
   345                          			; mit dem RTI geht es zu SKIPSUB wo
   346                          			; der Caller vom Stack genommen wird.
   347                          
   348  c568 6cc9c7             CONT	JMP (ORIGIRQ)	; weiter zum Original-Code (Kette).
   349                          
   350                          CHK_PC
   351  c56b bd0601             	LDA $106,X	; Unterbrechungs-PC
   352  c56e a8                 	TAY		; High-Byte
   353  c56f bd0501             	LDA $105,X	; Low-Byte
   354  c572 c0b5               	CPY #>(GC_START)
   355  c574 90f2               	BCC CONT	; vor der GC
   356  c576 d004               	BNE +		; über GC-Anfang
   357  c578 c926               	CMP #<(GC_START)
   358  c57a 90ec               	BCC CONT	; unterhalb GC
   359  c57c c0b6               +	CPY #>(GC_END+1)
   360  c57e 9006               	BCC ++		; in der GC!
   361  c580 d0e6               	BNE CONT	; über der GC
   362  c582 c93d               	CMP #<(GC_END+1)
   363  c584 b0e2               	BCS CONT	; über der GC
   364                          ++
   365                          	; Die GC wurde unterbrochen!
   366                          	; In irgendwelchen Spezialbereichen, wo wir mehr machen müssen?
   367                          
   368                          	; in PHP/PLP Sektion?
   369                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   370                          	  !error "High-Byte von GC_PHP_START und GC_PHP_END sind nicht gleich!"
   371                          	}
   372  c586 c0b5               	CPY #>(GC_PHP_START)
   373  c588 d00b               	BNE +		; nicht in richter Page, nächster Bereichstest
   374  c58a c98a               	CMP #<(GC_PHP_START)
   375  c58c 9007               	BCC +		; unterhalb, keine Stack-Korrektur
   376  c58e c999               	CMP #<(GC_PHP_END+1)
   377  c590 b003               	BCS +		; darüber, keine Stack-Korrektur
   378                          	; Stack-Korrektur:
   379  c592 68                 	PLA		; SR vom Stack nehmen, setzt ev. Flags N,Z aber nicht C
   380  c593 9014               	BCC TO_COLLECT	; C immer 0, mittels RTI zu COLLECT
   381                          +
   382                          	; in kritischer Sektion?
   383                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   384                          	  !error "High-Byte von GC_CRIT_START und GC_CRIT_END sind nicht gleich!"
   385                          	}
   386  c595 c0b6               	CPY #>(GC_CRIT_START)
   387  c597 d010               	BNE +		; nicht in richtiger Page, dann normal weiter
   388  c599 c932               	CMP #<(GC_CRIT_START)
   389  c59b 900c               	BCC +		; unterhalb, keine Korrektur
   390  c59d c939               	CMP #<(GC_CRIT_END+1)
   391  c59f b008               	BCS +		; darüber, keine Korrektur
   392                          
   393                          	; Korrektur, High Byte der String-Adresse in Descriptor setzen,
   394                          	; weil bereits das Low-Byte gesetzt wurde. Die Adresse im Descriptor
   395                          	; wäre sonst inkonsistent!
   396  c5a1 a455               	LDY $55		; Offset des Descriptors
   397  c5a3 c8                 	INY		; String-Länge
   398  c5a4 c8                 	INY		; String-Adresse-Low (ist schon gesetzt!)
   399  c5a5 a559               	LDA $59
   400  c5a7 914e               	STA ($4E),Y	; nun String-Adresse-High setzen
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
   413  c5a9 a9b7               	LDA #<(START_COLLECT)
   414                          			; Rückkehradresse für RTI am Stack umbiegen ...
   415  c5ab 9d0501             	STA $0105,X	; Low-Byte
   416  c5ae a9c5               	LDA #>(START_COLLECT)
   417  c5b0 9d0601             	STA $0106,X	; High-Byte
   418  c5b3 d0b3               	BNE CONT	; IRQ behandeln, RTI startet dann die neue GC ...
   419                          
   420                          SKIPSUB			; Open-Space- oder Search-for-Next-String-Routine abgebrochen:
   421  c5b5 68                 	PLA		; kommt direkt von RTI, Aufruf-PC (für RTS) in alter GC
   422  c5b6 68                 	PLA		; verwerfen und die neue GC übernimmt ...
   423                          			; direkt durch zu COLLECT
   424                          START_COLLECT
   425  c5b7 a903               	LDA #3
   426  c5b9 8553               	STA $53		; Step-Size für nächsten Descriptor auf
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
   437  c5bb ade707             	LDA MARKEVID	; Kontrollanzeige Bildschirm
   438  c5be 8dc7c7             	STA ORIGVID
   439  c5c1 a92a               	LDA #MARKE
   440  c5c3 8de707             	STA MARKEVID	; Marke: Zeichen
   441  c5c6 ade7db             	LDA MARKECOL	; sichern
   442  c5c9 8dc8c7             	STA ORIGCOL
   443  c5cc a909               	LDA #MARKEFARBE
   444  c5ce 8de7db             	STA MARKECOL	; Marke: Farbe sichern
   445                          
   446  c5d1 a207               	LDX #ZPLEN	; Zeropage-Reg.
   447  c5d3 b54b               SAVLOOP	LDA ZPSTART-1,X	; retten
   448  c5d5 9dcac7             	STA SAVE-1,X
   449  c5d8 ca                 	DEX
   450  c5d9 d0f8               	BNE SAVLOOP
   451                          
   452  c5db a537               	LDA MEMEND	; String-Pointer
   453  c5dd a638               	LDX MEMEND+1	; und Bereichanfang 
   454  c5df 8533               	STA STRPTR	; auf Speicherende
   455  c5e1 8634               	STX STRPTR+1	; setzen.
   456  c5e3 854c               	STA RNGBEG
   457  c5e5 864d               	STX RNGBEG+1
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
   481  c5e7 a533               	LDA STRPTR	; NEWPTR parallel mit
   482  c5e9 854e               	STA NEWPTR	; BUFPTR mitziehen ...
   483  c5eb a534               	LDA STRPTR+1
   484  c5ed 854f               	STA NEWPTR+1
   485  c5ef a64c               	LDX RNGBEG	; Bereich war zuletzt
   486  c5f1 a54d               	LDA RNGBEG+1	; String-Heap-Ende?
   487  c5f3 e431               	CPX STREND
   488  c5f5 d004               	BNE +
   489  c5f7 c532               	CMP STREND+1
   490  c5f9 f01b               	BEQ EXIT	; ja -> fertig
   491                          +
   492  c5fb 865d               	STX RNGEND	; um Pufferlänge - 256
   493  c5fd 855e               	STA RNGEND+1	; nach unten verlegen.
   494                          	!if <BUFSIZE > 0 {
   495                          	  !error "BUFSIZE ist nicht ein Vielfaches von 256 ($100)!"
   496                          	}
   497  c5ff 38                 	SEC		
   498  c600 e91f               	SBC #(>BUFSIZE-1) ; Bereichslänge in Pages,
   499                          			; kann um 254 Bytes überragt werden!
   500  c602 9008               	BCC LASTRANGE	; < 0 = Unterlauf (also auch <STREND)
   501  c604 854d               	STA RNGBEG+1
   502  c606 e431               	CPX STREND	; Ende des String-Heaps erreicht?
   503  c608 e532               	SBC STREND+1
   504  c60a b021               	BCS STRINRANGE	; Bereichsanfang >= String-Heap-Ende
   505                          LASTRANGE
   506  c60c a531               	LDA STREND	; Bereichanfang =
   507  c60e a632               	LDX STREND+1	; Speicheranfang (String-Heap-Ende)
   508  c610 854c               	STA RNGBEG	; 
   509  c612 864d               	STX RNGBEG+1	; 
   510  c614 d017               	BNE STRINRANGE	; immer, weil High-Byte >0
   511                          
   512                          
   513                          ; *** Ende der Garbage Collection
   514                          
   515                          EXIT
   516  c616 a207               	LDX #ZPLEN
   517  c618 bdcac7             RESLOOP	LDA SAVE-1,X	; Zeropage-Reg.
   518  c61b 954b               	STA ZPSTART-1,X	; restaurieren.
   519  c61d ca                 	DEX
   520  c61e d0f8               	BNE RESLOOP
   521                          
   522  c620 adc7c7             	LDA ORIGVID	; Kontrollanzeige löschen
   523  c623 8de707             	STA MARKEVID	; und alten Zustand wieder
   524  c626 adc8c7             	LDA ORIGCOL	; herstellen.
   525  c629 8de7db             	STA MARKECOL
   526                          !ifdef debug {
   527                          	JSR gra_off
   528                          }
   529  c62c 60                 	RTS
   530                          
   531                          
   532                          ; *** Bereich durchgehen
   533                          
   534                          STRINRANGE
   535                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   536  c62d a9c0               	LDA #>(BUF+BUFSIZE)
   537  c62f 8560               	STA BUFPTR+1
   538  c631 a900               	LDA #<(BUF+BUFSIZE)
   539  c633 855f               	STA BUFPTR
   540                          } else {
   541                          			; Sonderfall Pufferende bei $FFFF
   542                          	LDA #0		; Buffer-Pointer auf
   543                          	STA BUFPTR	; $10000 (65536) = 0
   544                          	STA BUFPTR+1	; setzen.
   545                          }
   546  c635 38                 	SEC
   547  c636 24                 	!byte $24	; BIT ZP, d.h. nächsten Befehl ignorieren!
   548                          NEXTSTR	
   549  c637 18                 	CLC
   550                          NEXTSTR1
   551  c638 20aac6             	JSR GETSA	; Nächste String-Adresse holen.
   552  c63b f03b               	BEQ COPYBACK	; keinen String mehr gefunden!
   553                          
   554  c63d 98                 	TYA		; high Byte
   555  c63e e45d               	CPX RNGEND	; X/A >= RNGEND:
   556  c640 e55e               	SBC RNGEND+1	; oberhalb des Bereichs, dann
   557  c642 b0f3               	BCS NEXTSTR	; nächster String!
   558                          
   559  c644 98                 	TYA		; high Byte
   560  c645 e44c               	CPX RNGBEG	; X/A < RNGBEG:
   561  c647 e54d               	SBC RNGBEG+1	; unterhalb des Bereichs, dann
   562  c649 90ed               	BCC NEXTSTR1	; nächster String!
   563                          			; Innerhalb des Bereichs:
   564  c64b a55f               	LDA BUFPTR	; Pufferzeiger um
   565  c64d e552               	SBC LEN		; String-Länge nach unten
   566  c64f 855f               	STA BUFPTR	; setzen.
   567  c651 b002               	BCS +
   568  c653 c660               	DEC BUFPTR+1	; Überlauf High-Byte
   569                          
   570  c655 8459               +	STY STRADR+1	; String-Adresse abspeichern
   571  c657 8658               	STX STRADR	; für Kopieraktion.
   572                          
   573  c659 a452               	LDY LEN		; String-Länge (> 0)
   574  c65b d004               	BNE NBENTRY	; immer, mit Dekrement beginnen!
   575  c65d b158               NEXTBYT	LDA (STRADR),Y	; String in den Pufferbereich
   576  c65f 915f               	STA (BUFPTR),Y	; übertragen, ROM ist aktiv
   577  c661 88                 NBENTRY	DEY		; schreibt ins RAM unters ROM!
   578  c662 d0f9               	BNE NEXTBYT
   579                          LEN1
   580  c664 b158               	LDA (STRADR),Y	; Das 0. Byte extra
   581  c666 915f               	STA (BUFPTR),Y	; übertragen
   582                          
   583  c668 38                 	SEC		; Neue String-Adresse:
   584  c669 a54e               	LDA NEWPTR	; Einfach den Pointer
   585  c66b e552               	SBC LEN		; mitziehen, ebenso um
   586  c66d 854e               	STA NEWPTR	; String-Länge nach unten
   587  c66f b002               	BCS +		; setzen.
   588  c671 c64f               	DEC NEWPTR+1	; Überlauf High-Byte
   589                          +
   590  c673 2082c7             	JSR CORR	; String-Adresse in Descriptor ändern.
   591                          			; Immmer Z=0,
   592  c676 d0bf               	BNE NEXTSTR	; zum nächsten String.
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
   608  c678 a55f               	LDA BUFPTR	; Puffer leer ...
   609  c67a c900               	CMP #<(BUF+BUFSIZE)
   610  c67c d006               	BNE +
   611  c67e a560               	LDA BUFPTR+1	; Wenn Pointer am Ende ...
   612  c680 c9c0               	CMP #>(BUF+BUFSIZE)
   613  c682 f023               	BEQ NOCOPY	; ist der Puffer leer, ev. nächster
   614                          +			; Bereich ...
   615                          } else {
   616                          			; Sonderfall: Pufferende bei $FFFF
   617                          	LDA BUFPTR	; Puffer leer,
   618                          	ORA BUFPTR+1	; wenn Pointer =0 (Ende)
   619                          	BEQ NOCOPY	; War es letzter Bereich?
   620                          }
   621                          
   622                          !ifdef orig_movblock {
   623                          	LDA STRPTR	; Original MOVBLOCK braucht
   624                          	LDX STRPTR+1	; Zielblockende+1
   625                          } else {
   626  c684 a54e               	LDA NEWPTR	; Optimiertes MOVBLOCK braucht
   627  c686 a64f               	LDX NEWPTR+1	; Zielblockanfang	
   628                          }
   629  c688 8558               	STA $58		; = STRADR
   630  c68a 8659               	STX $59		; je nach MOVBLOCK-Variante
   631                          			; Ende+1 oder Anfang des Zielblocks
   632                          
   633                          !ifdef orig_movblock {
   634                          	LDA NEWPTR
   635                          	LDX NEWPTR+1	
   636                          }
   637  c68c 8533               	STA STRPTR	; neues FRETOP
   638  c68e 8634               	STX STRPTR+1
   639                          
   640                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   641  c690 a900               	LDA #<(BUF+BUFSIZE)
   642  c692 855a               	STA $5A		; Quellblockende+1
   643  c694 a9c0               	LDA #>(BUF+BUFSIZE)
   644  c696 855b               	STA $5B
   645                          } else {
   646                          			; Sonderfall Pufferende bei $FFFF
   647                          	LDA #$00	; Quellblockende+1
   648                          	STA $5A
   649                          	STA $5B
   650                          }
   651                          			; Quellbockanfang = BUFPTR
   652                          
   653  c698 78                 	SEI		; keine Interrupts zulassen, wegen
   654  c699 a501               	LDA PROCPORT	; KERNAL-ROM wegblenden
   655  c69b 48                 	PHA		; damit das Puffer-RAM zugänglich
   656  c69c a935               	LDA #MEMRAM	; wird. Auch BASIC-ROM ist damit weg!
   657                          			; Sowohl bei Puffer $F000 als auch $A000
   658                          			; werden beide ROMs ausgeblendet.
   659  c69e 8501               	STA PROCPORT
   660                          
   661  c6a0 2091c7             	JSR MOVBLOCK	; BASIC-Routine Blockverschieben
   662                          			; bei IRQ-Anbindung eigene Kopie verwenden,
   663                          			; da das BASIC-ROM ausgeblendet ist!
   664                          			; Andernfalls ist ja die ROM-Kopie im RAM da.
   665                          			; Z=1
   666  c6a3 68                 	PLA		; ursprünglicher Zustand
   667  c6a4 8501               	STA PROCPORT	; KERNAL-ROM wieder aktivieren
   668  c6a6 58                 	CLI
   669                          NOCOPY
   670  c6a7 4ce7c5             	JMP NEXTBLOCK	; nächsten Bereich
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
   684  c6aa 9063               GETSA	BCC CHECKTYPE	; C=0 -> nächsten String laut STAT
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
   700  c6ac a000               	LDY #0
   701  c6ae 8423               	STY STRDP+1	; Descriptor auf
   702  c6b0 a905               	LDA #STAT_SDS	; Status: SDS
   703  c6b2 8557               	STA STAT
   704  c6b4 a219               	LDX #TOSS	; SDS Start
   705  c6b6 d005               	BNE ISDSTEND	; immer verzweigen
   706  c6b8 a622               DSTACK	LDX STRDP
   707  c6ba e8                 NEXTDST	INX		; nächster Descriptor
   708  c6bb e8                 	INX
   709  c6bc e8                 	INX
   710                          ISDSTEND
   711  c6bd e416               	CPX TSSP	; Stack durch?
   712  c6bf f010               	BEQ VARS
   713  c6c1 b500               	LDA 0,X		; String-Länge
   714  c6c3 f0f5               	BEQ NEXTDST
   715  c6c5 8552               	STA LEN		; Rückgabevariable
   716  c6c7 8622               	STX STRDP	; festhalten
   717  c6c9 b502               	LDA 2,X		; String-Adr. high
   718  c6cb a8                 	TAY
   719  c6cc b501               	LDA 1,X		; String-Adr. low
   720  c6ce aa                 	TAX
   721  c6cf 98                 	TYA		; immer ungleich 0, Z=0
   722  c6d0 60                 	RTS		; Adresse in X/Y retour
   723                          
   724                          ; *** Variablen: VARTAB bis ARYTAB
   725                          
   726  c6d1 a52d               VARS	LDA VARTAB	; Variablenanfang
   727  c6d3 a62e               	LDX VARTAB+1
   728  c6d5 8522               	STA STRDP
   729  c6d7 8623               	STX STRDP+1
   730  c6d9 a003               	LDY #STAT_VAR	; Status: einfache Variablen
   731  c6db 8457               	STY STAT
   732  c6dd d00b               	BNE ISVAREND	; immer verzweigen
   733                          VAR
   734  c6df 18                 NEXTVAR	CLC		; nächste Variable
   735  c6e0 a522               	LDA STRDP
   736  c6e2 6907               	ADC #7		; Variablenlänge
   737  c6e4 8522               	STA STRDP
   738  c6e6 9002               	BCC ISVAREND
   739  c6e8 e623               	INC STRDP+1	; Überlauf High-Byte
   740                          ISVAREND
   741  c6ea c52f               	CMP ARYTAB
   742  c6ec d006               	BNE CHECKVAR
   743  c6ee a623               	LDX STRDP+1	; Var.-Ende (=Array-Anfang)?
   744  c6f0 e430               	CPX ARYTAB+1
   745  c6f2 f026               	BEQ ARRAYS	; Var.-Ende, weiter mit Arrays
   746                          CHECKVAR
   747  c6f4 a000               	LDY #0		; Variablenname
   748  c6f6 b122               	LDA (STRDP),Y	; 1. Zeichen, Typ in Bit 7 
   749  c6f8 30e5               	BMI NEXTVAR	; kein String, nächste V.
   750  c6fa c8                 	INY
   751  c6fb b122               	LDA (STRDP),Y	; 2. Zeichen, Typ in Bit 7
   752  c6fd 10e0               	BPL NEXTVAR	; kein String, nächste V.
   753  c6ff c8                 	INY
   754  c700 b122               	LDA (STRDP),Y	; String-Länge
   755  c702 f0db               	BEQ NEXTVAR	; = 0, Nächste Variable
   756  c704 8552               	STA LEN		; Rückgabevariable
   757  c706 c8                 	INY
   758  c707 b122               	LDA (STRDP),Y	; String-Adresse low
   759  c709 aa                 	TAX
   760  c70a c8                 	INY
   761  c70b b122               	LDA (STRDP),Y	; String-Adresse high
   762  c70d a8                 	TAY		; immer ungleich 0, Z=0
   763  c70e 60                 	RTS		; Adresse in X/Y retour
   764                          
   765                          CHECKTYPE
   766  c70f a557               	LDA STAT	; GETSA-Einstieg mit C=0
   767  c711 c903               	CMP #STAT_VAR	; String-Status?
   768  c713 9042               	BCC ARRAY	; =1 -> Arrays
   769  c715 f0c8               	BEQ VAR		; =3 -> Variablen
   770  c717 4cb8c6             	JMP DSTACK	; =5 -> String-Desc.-Stack
   771                          
   772                          ; *** Arrays: ARYTAB bis STREND
   773                          
   774  c71a 8550               ARRAYS	STA PTR		; A/X von Variablendurchlauf
   775  c71c 8651               	STX PTR+1	; Start Array-Array-Bereich
   776  c71e a001               	LDY #STAT_ARY
   777  c720 8457               	STY STAT	; Status: Arrays
   778                          ISARREND
   779  c722 a550               	LDA PTR
   780  c724 a651               	LDX PTR+1
   781  c726 e432               CHKAEND	CPX STREND+1	; Ende des Array-Bereichs
   782  c728 d004                       BNE NEXTARR	; erreicht?
   783  c72a c531               	CMP STREND
   784  c72c f04f               	BEQ NOSTRING	; Arrays fertig -> kein String
   785                          NEXTARR
   786  c72e 8522               	STA STRDP	; immer C=0
   787  c730 8623               	STX STRDP+1
   788  c732 a000               	LDY #0
   789  c734 b122               	LDA (STRDP),Y	; Array-Name
   790  c736 aa                 	TAX		; Array-Typ merken
   791  c737 c8                 	INY
   792  c738 b122               	LDA (STRDP),Y
   793  c73a 08                 	PHP		; Array-Typ merken
   794  c73b c8                 	INY
   795  c73c b122               	LDA (STRDP),Y	; Offset nächstes Array
   796  c73e 6550               	ADC PTR		; C-Flag ist bereits 0 (CMP/CPX)
   797  c740 8550               	STA PTR		; Start Folge-Array
   798  c742 c8                 	INY
   799  c743 b122               	LDA (STRDP),Y
   800  c745 6551               	ADC PTR+1
   801  c747 8551               	STA PTR+1
   802  c749 28                 	PLP		; Var.-Typ holen
   803  c74a 10d6               	BPL ISARREND	; kein String-Array
   804  c74c 8a                 	TXA		; Var.-Typ holen
   805  c74d 30d3               	BMI ISARREND	; kein String-Array
   806  c74f c8                 	INY
   807  c750 b122               	LDA (STRDP),Y	; Anzahl der Dimensionen
   808  c752 0a                 	ASL		; *2
   809  c753 6905               	ADC #5		; Offset = Dimensionen*2+5
   810                          			; C=0 solange Dim.. <= 125
   811  c755 d003               	BNE ADVDESC	; immer verzweigen
   812                          ARRAY			; Einstieg bei Fortsetzung
   813                          NEXTASTR
   814  c757 18                 	CLC
   815  c758 a903               	LDA #3		; String-Descriptor-Länge
   816  c75a 6522               ADVDESC	ADC STRDP	; nächten String
   817  c75c 8522               	STA STRDP
   818  c75e 9002               	BCC +
   819  c760 e623               	INC STRDP+1	; Überlauf High-Byte
   820  c762 c550               +	CMP PTR		; Array durch?
   821  c764 d006               	BNE IS0ASTR
   822  c766 a623               	LDX STRDP+1
   823  c768 e451               	CPX PTR+1
   824  c76a f0ba               	BEQ CHKAEND	; A/X = PTR, Array-Ende prüfen
   825                          IS0ASTR
   826  c76c a000               	LDY #0
   827  c76e b122               	LDA (STRDP),Y	; String-Länge
   828  c770 f0e5               	BEQ NEXTASTR	; weiter im Array
   829  c772 8552               	STA LEN		; Rückgabevariable
   830  c774 c8                 	INY
   831  c775 b122               	LDA (STRDP),Y	; String-Adresse low
   832  c777 aa                 	TAX
   833  c778 c8                 	INY
   834  c779 b122               	LDA (STRDP),Y	; String-Adresse high
   835  c77b a8                 	TAY		; immer ungleich 0, Z=0
   836  c77c 60                 	RTS		; Adresse in X/Y retour
   837                          
   838                          NOSTRING
   839  c77d a900               	LDA #0		; Länge 0 
   840  c77f 8552               	STA LEN		; kein String gefunden
   841  c781 60                 	RTS		; Z=1
   842                          
   843                          ;
   844                          ; CORR - String-Adresse im Descriptor korrigieren
   845                          ;
   846                          ; ( STRADR, STAT -> )
   847                          ;
   848  c782 a557               CORR	LDA STAT	; String-Status
   849  c784 2903               	AND #%011	; nur 2 Bits
   850  c786 a8                 	TAY		; Lage des Descriptors
   851  c787 a54e               	LDA NEWPTR	;
   852  c789 9122               	STA (STRDP),Y	; ... bei SDS
   853  c78b c8                 	INY		; ... und Array verschieden!
   854  c78c a54f               	LDA NEWPTR+1
   855  c78e 9122               	STA (STRDP),Y
   856  c790 60                 	RTS
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
   882  c791 38                         SEC
   883  c792 a55a                       LDA $5A       ; Quelle Endadresse low
   884  c794 e55f                       SBC $5F       ; minus Quelle Startadresse low
   885  c796 a8                         TAY           ; Länge low
   886  c797 a55b                       LDA $5B       ; Endadresse high
   887  c799 e560                       SBC $60       ; minus Startadresse high
   888  c79b aa                         TAX           ; Länge high, als DEC-DEC-Counter
   889  c79c 98                         TYA           ; Länge low
   890  c79d f019                       BEQ copy      ; wenn 0, dann geht es einfach ...
   891  c79f 18                         CLC           ; Länge in A
   892  c7a0 655f                       ADC $5F       ; Quellstartadresse um Low-Byte-Offset
   893  c7a2 855f                       STA $5F       ; korrigieren: -(-Länge) -> +Länge
   894  c7a4 b002                       BCS +         ; da eigentlich Subtraktion, Überlauf
   895  c7a6 c660                       DEC $60       ; entspr. in Startadresse high korr.
   896                          +
   897  c7a8 98                         TYA           ; Länge low
   898  c7a9 18                         CLC
   899  c7aa 6558                       ADC $58       ; Zielstartadresse um Low-Bye-Offset
   900  c7ac 8558                       STA $58       ; korrigieren: -(-Länge) -> +Länge
   901  c7ae b002                       BCS +         ; da eigentlich Subtraktion, Überlauf
   902  c7b0 c659                       DEC $59       ; entspr. in Startadresse high korr.
   903                          +
   904  c7b2 e8                 	INX           ; Page-Zähler (alle ganzen und einen teilweise)
   905  c7b3 98                 	TYA           ; Länge low
   906  c7b4 49ff               	EOR #$FF      ; negieren (2er-Komplement):
   907  c7b6 a8                 	TAY           ; NOT(X)+1
   908  c7b7 c8                 	INY
   909  c7b8 b15f               copy    LDA ($5F),Y   ; Quelle auf
   910  c7ba 9158                       STA ($58),Y   ; Ziel kopieren, aufsteigend
   911  c7bc c8                         INY
   912  c7bd d0f9                       BNE copy
   913  c7bf e660                       INC $60       ; Quelle high
   914  c7c1 e659                       INC $59       ; Ziel high
   915  c7c3 ca                         DEX           ; Blockzähler 
   916  c7c4 d0f2                       BNE copy
   917  c7c6 60                         RTS
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
   984  c7c7 00                 ORIGVID !byte 0		; originale Video der Markenposition
   985  c7c8 00                 ORIGCOL !byte 0		; originale Farbe der Markenposition
   986                          !ifndef basic_patch {
   987  c7c9 0000               ORIGIRQ !byte 0,0	; originaler IRQ-Vektor
   988                          }
   989  c7cb 00                 SAVE	!byte 0		; gesicherte ZP-Variablen
   990                          *=*+ZPLEN-1
   991                          
   992                          
