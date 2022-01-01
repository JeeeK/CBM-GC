
; ******** Source: garbcol.asm
     1                          !to "garbcol.o",cbm	
     2                          ;
     3                          ;  **** Garbage Collection ****
     4                          ;
     5                          ; 64'er, Oct. 1988
     6                          ;
     7                          ; In zwei Schritten wird der String-Speicher kompaktiert:
     8                          ;   1) Alle Strings im String-Descriptor-Stack (SDS),
     9                          ;      in Variablen und Arrays erhalten im
    10                          ;      Backlink den Verweis auf den Descriptor.
    11                          ;      Nicht mehr referenzierte Strings bleiben als
    12                          ;      ungenutzt markiert und verweisen auf den
    13                          ;      nächsten String Richtung niedriger Adressen.
    14                          ;   2) Nun wird der String-Speicher absteigend durchgegangen,
    15                          ;      wobei nur die aktiven Strings nach "oben" über
    16                          ;      etwaige Lücken hinweg kopiert werden. Die ungenutzten
    17                          ;      Lücken werden dabei übergangen.
    18                          ;      Beim Kopieren wird der Backlink wieder entfernt
    19                          ;      (als ungenutzt markiert), da ja bis zur nächsten
    20                          ;      Kompaktierung der Speicher aufgegeben werden könnte.
    21                          ;
    22                          ; Im Vergleich zu der von CBM eingesetzten Routine, wird
    23                          ; hier auf eine platzraubende Optimierung verzichtet,
    24                          ; wenn ein Teil oder der gesamt String-Speicher schon
    25                          ; kompaktiert sein sollte. Es werden dann die Strings
    26                          ; im schlimmsten Fall wieder über sich selbst kopiert.
    27                          ;
    28                          ; Ein gleichwertiger Ansatz wäre eine einfache Abfrage
    29                          ; bei opt_no_copy, ob ptr (Arbeitszeiger auf den alten
    30                          ; Heap) gleich newptr (neuer Heap) ist. Solange diese
    31                          ; der Fall ist, ist keine Kopieraktion (und auch
    32                          ; keine Descriptor-Korrektur) notwendig.
    33                          ; In diesem Fall kann allerdings die Optimierung #3 nicht
    34                          ; verwendet werden, da sonst die Backlink-Markierung
    35                          ; nicht in allen Fällen passieren würde!
    36                          ;
    37                          ; Überarbeitetet und korrigiert:
    38                          ;	2013-11-15 Johann E. Klasek, johann at klasek at
    39                          ; Optimiert:
    40                          ;	2019-03-20 Johann E. Klasek, johann at klasek at
    41                          ;
    42                          ; Bugfixes:
    43                          ;
    44                          ;	1) in backlinkarr:
    45                          ;	   das C-Flag ist beim Errechnen des Folge-Arrays
    46                          ;	   definiert zu löschen, sonst werden ev. nicht 
    47                          ;	   alle Elemente aller Arrays mit einem korrekten
    48                          ;	   Backlink versehen und der String-Heap wird 
    49                          ;	   durch die GC korrumpiert!
    50                          ;
    51                          ;	2) in backlinkarr bei blanext:
    52                          ;	   Muss zum Aufrufer immer mit Z=0 rückkehren,
    53                          ;	   und erkennt sonst immer nur das 1. Array!
    54                          ;	   Damit liegen die anderen Strings dann im 
    55                          ;	   freien Bereich und werden nach und nach 
    56                          ;	   überschrieben!
    57                          ;
    58                          ; Optimierungen:
    59                          ;    
    60                          ;    #1 Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
    61                          ;	Wegen des längeren Codes und und einem deswegen länger werdenden
    62                          ;	Branch-Offset, wurde der Code ab cfinish zurückversetzt.
    63                          ;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
    64                          ;	noch an anderer Stelle eingespart werden.
    65                          ;	Dabei wird unmittelbar vor dem Kopieren des Strings nicht
    66                          ;	mehr explizit der Fall eines Strings mit Länge 0 betrachtet,
    67                          ;	der an dieser Stelle auch nicht auftreten kann, da
    68                          ;	der erste Durchgang durch alle Strings am SDS, bei den
    69                          ;	Variablen und den Arrays Strings der Länge 0 übergeht. 
    70                          ;	Selbst, wenn jemand böswilligerweise via allocate-Routine
    71                          ;	0-Längen-Speicher anfordert (was immer 2 Link-Bytes kostet),
    72                          ;	können diese Leerstring nicht referenziert werden. Im Zuge
    73                          ;	des zweiten Durchlaufs (Collection) würden diese degenerieren
    74                          ;	0-Längen-Strings auch wieder verschwinden.
    75                          ;
    76                          ;	Aktivierbar via use_fast_copy-Variable.
    77                          ;
    78                          ;    #2 allocate etwas kompakter/schneller (-2 Byte Code, -3 T)
    79                          ;       Der Backlink wird via strptr/strptr+1 gesetzt, wobei
    80                          ;       bei einem String länger 253 Bytes das High-Byte in strptr+1
    81                          ;	erhöht wird, statt dies mit fretop+1 zu machen, welches
    82                          ;	dann restauriert werden muss.
    83                          ;       
    84                          ;	Aktivierbar via alternate_stralloc-Variable.
    85                          ;
    86                          ;    #3 Die Lückenmarkierung (Low-Byte mit der Länge) wird beim
    87                          ;	Kopieren des Strings mitgemacht. (-4 Byte Code, -5 T/String)
    88                          ;	Siehe no_opt_3-Abfrage bei Label cw3.
    89                          ;
    90                          ;    #4 Kein String-Kopieren durchführen, solange der String-Heap
    91                          ;	geordnet ist (also solange ptr = newptr ist). Sobald
    92                          ;	eine Lücke eliminiert wurde, laufen ptr und newptr auseinander.
    93                          ;
    94                          
    95                          ; Optimierung #1: Die optimierte Kopierroutine verwenden
    96                          ; aktiv
    97                          !set use_fast_copy=1
    98                          
    99                          ; Optimierung #2: etwas kürzere und schnellere stralloc-Routine
   100                          ; inaktiv
   101                          ;!set alternate_stralloc=1
   102                          
   103                          ; Optimierung #3: Lückmarkierung teilweise mit String-Kopieren mitmachen.
   104                          ; ist immer aktiv
   105                          
   106                          ; Optimierung #4: Kein String-Kopieren, solange Heap geordnet ist.
   107                          ; Wenn aktiv (passt aber nicht ins ROM!), dann darf nicht Optimierung #3
   108                          ; aktiv sein!
   109                          ; inaktiv
   110                          ;!set opt_no_copy=1
   111                          
   112                          !ifdef opt_no_copy {
   113                          !set no_opt_3=1
   114                          }
   115                          
   116                          
   117                          ; Basic-Zeiger und -konstanten
   118                          
   119                          collected = $0f
   120                          
   121                          sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
   122                          			; wächst nach oben, max. 3 Elemente
   123                          			; zu je 3 Bytes.
   124                          sdsptr   = $16		; Zeiger auf nächstes freie Element
   125                          			; des String-Descriptor-Stacks (SDS)
   126                          
   127                          vartab   = $2d		; Basicprogrammende = Variablenanfang
   128                          arytab   = $2f		; Variablenende = Array-Bereichanfang
   129                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
   130                          fretop   = $33		; aktuelle String-Heap-Adresse
   131                          strptr	 = $35		; temporärer Stringzeiger
   132                          memsiz   = $37		; höchste RAM-Adresse für Basic, Start
   133                          			; des nach unten wachsenden String-Heaps
   134                          ; Hilfsvariablen
   135                          
   136                          ptr	 = $22		; Arbeitszeiger, alter Heap
   137                          newptr	 = $4e		; Neuer Stringzeiger, neuer Heap
   138                          desclen	 = $53		; akt. Länge eines Stringdescriptors
   139                          aryptr	 = $58		; Array-Zeiger
   140                          descptr	 = $5f		; Descriptor-Zeiger
   141                          
   142                          garcoll  = $b526
   143                          
   144                          ; Vorbelegung der Speicherplätze
   145                          
   146                          romsize  = $2000	; ROM Länge 8K
   147                          
   148                          prozport = $01		; Prozessorport
   149                          memrom = %00110111	; Basic+Kernal ROM
   150                          membas = %00110110	; Basic RAM+kernal ROM
   151                          memram = %00110101	; Basic+Kernal RAM
   152                          
   153                          
   154                          ; Datenstrukturen
   155                          ;
   156                          ; String am Heap:
   157                          ;
   158                          ;   +--------------------------------------+
   159                          ;   |       +--------------+               |
   160                          ;   V       |              V               |
   161                          ;   +---+---+---+          +-----------+---+---+
   162                          ;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
   163                          ;   +---+---+---+          +-----------+---+---+
   164                          ;   ^    *******           ^            *******
   165                          ;   |       String.-Adr.   |               Descriptor-Adr.
   166                          ;   +-Descriptor-Adresse   +-String-Adresse
   167                          ;
   168                          ; Lücken am Heap:
   169                          ;                      
   170                          ;   +-------------+   +--------------+
   171                          ;   V             |   V              |
   172                          ;    +-----------+---+---+---------+---+---+
   173                          ;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
   174                          ;    +-----------+---+---+---------+---+---+
   175                          ;                  ^  ***            ^  ***
   176                          ;                  |   Lückenmark.   |   Lückenmarkierung
   177                          ;                  Backlink-Adresse  Backlink-Adresse
   178                          
   179                          
   180                          

; ******** Source: loader.asm
     1                          ;
     2                          ; *********** Loader
     3                          ;
     4                          ;       2013 11 10 johann e. klasek, johann at klasek at
     5                          ;
     6                          
     7                          ; --- Temporäre Variablen:
     8                          
     9                          ptr		= $22		; Zeropage, frei
    10                          ptr_l		= ptr
    11                          ptr_h		= ptr+1
    12                          
    13                          len		= $4f		; Zeropage, temp. frei
    14                          len_l		= len
    15                          len_h		= len+1
    16                          
    17                          dest		= $51		; Zeropage, temp. frei
    18                          dest_l		= dest
    19                          dest_h		= dest+1
    20                          
    21                          
    22                          ; --- Konstanten:
    23                          
    24                          basicrom	= $a000		; Startadresse
    25                          kernalrom	= $e000		; Startadresse
    26                          
    27                          ; --- Ein-/Ausgabe:
    28                          
    29                          prozport 	= $01		; Prozessorport
    30                          
    31                          memrom		= %00110111	; Basic+Kernal ROM
    32                          membas		= %00110110	; Basic ram+Kernal ROM
    33                          memram		= %00110101	; Basic+Kernal ROM
    34                          
    35                          
    36                          *= $0801
    37                          basic_start
    38                          ;       2013 sys2061
    39  0801 0b08dd079e         	!by <EOP,>EOP,<(2013),>(2013),$9E
    40  0806 32303631           	!tx "2061"
    41  080a 00                 	!by 0 			; End of Line
    42  080b 0000               EOP	!by 0, 0		; Basic-Programmende
    43                          
    44                          loader
    45                          !if loader != 2061 {
    46                          	!error "Loader-Adresse stimmt nicht mit SYS-Adresse überein!"
    47                          }
    48  080d a000               	ldy #0
    49  080f a937               	lda #memrom
    50  0811 8501               	sta prozport		; ROM einblenden
    51  0813 8422               	sty ptr_l
    52                          
    53                          	; Basic und Kernal ins RAM kopieren
    54                          
    55  0815 a9a0               	lda #>basicrom		; Basic ROM Start
    56  0817 206a08             	jsr copyram
    57  081a a9e0               	lda #>kernalrom		; Kernal ROM Start
    58  081c 206a08             	jsr copyram
    59                          
    60                          	; Patchliste abarbeiten (Basic und Kernal betreffend)
    61                          
    62  081f a200               	ldx #0
    63  0821 bd7b08             nextp	lda patchlist,x
    64  0824 8522               	sta ptr
    65  0826 a8                        	tay
    66                          
    67  0827 bd7c08             	lda patchlist+1,x
    68  082a 8523               	sta ptr_h
    69  082c d003               	bne patch
    70  082e 98                 	tya
    71  082f f034               	beq pend
    72                          
    73                          patch	
    74  0831 bd7e08             	lda patchlist+3,x
    75  0834 8550               	sta len_h
    76  0836 bd7d08             	lda patchlist+2,x
    77  0839 854f               	sta len_l
    78  083b f002               	beq nohighcorr		; dec 0/0 Korrektur
    79  083d e650               	inc len_h
    80                          nohighcorr
    81                          
    82  083f bd7f08             	lda patchlist+4,x
    83  0842 8551               	sta dest_l
    84                          
    85  0844 bd8008             	lda patchlist+5,x
    86  0847 8552               	sta dest_h
    87                          
    88  0849 a000               	ldy #0
    89  084b b122               ploop	lda (ptr),y		; Patch an richtige Adresse
    90  084d 9151               	sta (dest),y		; übertragen
    91  084f c8                 	iny
    92  0850 d004               	bne nohigh
    93  0852 e623               	inc ptr_h		; High Byte bei Überlauf
    94  0854 e652               	inc dest_h
    95                          nohigh
    96  0856 c64f               	dec len_l		; Länge herunter
    97  0858 d0f1               	bne ploop		; zählen nach
    98  085a c650               	dec len_h		; dec 0/0 Methode
    99  085c d0ed               	bne ploop
   100  085e 8a                 	txa			; Index auf nächsten Patch
   101  085f 18                 	clc			; positionieren ...
   102  0860 6906               	adc #6
   103  0862 aa                 	tax
   104  0863 d0bc               	bne nextp		; immer
   105                          
   106                          pend
   107  0865 a935               	lda #memram		; BASIC, KERNAL RAM aktivieren
   108  0867 8501               	sta prozport
   109  0869 60                 	rts
   110                          
   111                          
   112                          ; 8kByte Block an geleiche Stelle kopieren
   113                          
   114                          copyram
   115  086a 8523               	sta ptr_h		; Startadresse
   116  086c a220               	ldx #$20		; Pages: 8K
   117  086e b122               toram	lda (ptr),y		; ROM lesen
   118  0870 9122               	sta (ptr),y		; RAM schreiben
   119  0872 c8                 	iny
   120  0873 d0f9               	bne toram
   121  0875 e623               	inc ptr_h		; nächste "Page"
   122  0877 ca                 	dex
   123  0878 d0f4               	bne toram
   124  087a 60                 	rts
   125                          

; ******** Source: garbcol.asm
   182                          
   183                          ;
   184                          ; Patch-Liste für "loader"
   185                          ;
   186                          
   187                          patchlist:
   188                          
   189  087b 95084501f4b4       !wo part1_real,part1_real_end-part1_real,part1
   190  0881 250a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
   191  0887 ef09360074e4       !wo part3_real,part3_real_end-part3_real,part3
   192  088d da091500c1b6       !wo part4_real,part4_real_end-part4_real,part4
   193  0893 0000               !wo 0  ; Endemarkierung
   194                          
   195                          
   196                          ; ******************************* part 1 *************************************
   197                          
   198                          part1_real:
   199                          
   200                          !pseudopc $b4f4 {
   201                          
   202                          part1:
   203                          
   204                          ;***** Speicher von String-Heap anfordern
   205                          ;
   206                          ;	in:	A		; Länge anforderung
   207                          ;		fretop
   208                          ;	mod:	collected	; "GC aufgerufen"-Flag
   209                          ;		strptr		; temp. Zeiger
   210                          ;	out:	fretop		; Adresse auf String
   211                          ;		X,Y		; Adresse auf String
   212                          ;
   213                          ; Der String wird im Backlink stets als ungebrauchte Lücke
   214                          ; markiert! Dann muss die GC nur noch die Backlinks
   215                          ; der aktiven Strings setzen und kann die ungebrauchten
   216                          ; Strings überspringen.
   217                          
   218                          
   219                          basicerror = $b4d2		; Basic-Fehlermeldung
   220                          
   221                          allocate:
   222  0895 460f               	lsr collected		; Flag löschen
   223  0897 48                 retry	pha			; Länge der Anforderung,
   224                          				; für 2. Teil
   225                          				; Länge 0 möglich, verbraucht aber 2 Bytes
   226  0898 49ff               	eor #$ff		; negieren
   227  089a 38                 	sec
   228  089b 6533               	adc fretop		; A/X = fretop; A/X -= Länge
   229  089d a634               	ldx fretop+1
   230  089f b002               	bcs l1
   231  08a1 ca                 	dex
   232  08a2 38                 	sec
   233  08a3 e902               l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
   234  08a5 b001               	bcs l2
   235  08a7 ca                 	dex
   236  08a8 e432               l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
   237  08aa 9006               	bcc checkcollect
   238  08ac d013               	bne alloc		; nein, Bereich anfordern
   239  08ae c531               	cmp strend 
   240  08b0 b00f               	bcs alloc		; nein, Bereich anfordern
   241                          checkcollect
   242  08b2 a210               	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
   243  08b4 a50f               	lda collected
   244  08b6 30bb               	bmi basicerror		; Collection schon gelaufen?
   245  08b8 2026b5             	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
   246  08bb 660f               	ror collected		; Flag setzen (Bit 7) setzen
   247  08bd 68                 	pla			; Länge angeforderter Bereich
   248  08be 4cf6b4             	jmp retry		; nochmal versuchen (ob durch GC Platz frei wurde)
   249                          
   250  08c1 2067b5             alloc	jsr setfretop		; FRETOP = A/X
   251  08c4 4cbae4             	jmp stralloc		; zum 2. Teil: Allokation abschließen
   252                          
   253                          
   254                          ;***** garbage collection
   255                          
   256                          ;	in:	-
   257                          ;	mod:	ptr		; Zeiger auf alten String-Heap
   258                          ;		newptr		; Zeiger auf neuen String-Heap
   259                          ;		descptr		; Zeiger auf Descriptor
   260                          ;		desclen		; Descriptor-Schrittweite
   261                          ;	out:	fretop		; Neue String-Heap-Position
   262                          ;		C=1
   263                          
   264                          docollect
   265                          
   266                          
   267                          ; Backlink aller temporären Strings am String-Descriptor-Stack setzen
   268                          
   269  08c7 a919               sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   270  08c9 a200               	ldx #>sdsbase		; da in 0-Page, immer 0
   271  08cb 20a5e4             	jsr setptr		; damit ptr setzen
   272                          
   273  08ce c516               sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   274  08d0 f005               	beq vars		; Ja, SDS durch, weiter mit Variablen
   275  08d2 20e6b5             	jsr backlink		; sonst Backlink setzen
   276  08d5 f0f7               	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descriptor
   277                          
   278                          ; Backlink aller String-Variablen setzen
   279                          
   280  08d7 a905               vars:	lda #5			; Descriptor-Schritt für Variablen
   281  08d9 8553               	sta desclen
   282  08db a52d               	lda vartab		; Variablenbeginn
   283  08dd a62e               	ldx vartab+1
   284  08df 20a5e4             	jsr setptr		; ptr = A/X
   285                          
   286  08e2 e430               varnext	cpx arytab+1		; Variablenende?
   287  08e4 d004               	bne varbl
   288  08e6 c52f               	cmp arytab
   289  08e8 f005               	beq arrays		; ja, weiter mit Arrays
   290  08ea 201db6             varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
   291  08ed d0f3               	bne varnext		; immer; nächsten Var.-Descriptor
   292                          
   293                          ; Backlink bei allen String-Arrays setzen
   294                          
   295                          arrays:
   296  08ef 8558               	sta aryptr		; Variablenbereichende = Array-Bereichanfang
   297  08f1 8659               	stx aryptr+1 
   298  08f3 a003               	ldy #3			; Descriptor-Schritt bei String-Arrays
   299  08f5 8453               	sty desclen
   300                          
   301  08f7 e432               arrnext	cpx strend+1		; Array-Bereichende?
   302  08f9 d004               	bne arrbl
   303  08fb c531               	cmp strend
   304  08fd f00e               	beq cleanwalk
   305  08ff 20c4b6             arrbl	jsr backlinkarr		; Backlinks für nächstes String-Array setzen -> Z=0!
   306  0902 d0f3               	bne arrnext		; immer; nächstes Array-Element
   307                          
   308                          
   309                          ; Ende, Zeiger zum neuen String-Heap übernehmen
   310                          
   311                          cfinish
   312  0904 a54e               	lda newptr		; Aufgeräumtzeiger ist ..
   313  0906 a64f               	ldx newptr+1
   314                          setfretop
   315  0908 8533               	sta fretop		; neues FRETOP
   316  090a 8634               	stx fretop+1 
   317  090c 60                 	rts			; fertig!
   318                          
   319                          ; Nachdem nun alle Backlinks gesetzt sind
   320                          ; den String-Heap von oben nach unten durchgehen
   321                          ; und zusammenschieben ...
   322                          
   323                          cleanwalk:
   324  090d a537               	lda memsiz		; beim Basic-Speicherende
   325  090f a638               	ldx memsiz+1
   326  0911 854e               	sta newptr		; ... beginnen
   327  0913 864f               	stx newptr+1 
   328                          
   329                          ; Aufräumschleife
   330                          
   331  0915 e434               cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   332  0917 d004               	bne cwclean		; dann Heap durch und fertig.
   333  0919 c533               	cmp fretop		; andernfalls aufräumen ...
   334  091b f0e7               	beq cfinish		; fertig, weil A/X = FRETOP
   335                          
   336                          ; nächsten String "aufräumen" ...
   337                          
   338  091d 38                 cwclean	sec			; Aufgeräumtzeiger auf Backlink
   339  091e e902               	sbc #2
   340  0920 b001               	bcs cw1
   341  0922 ca                 	dex			; A/X -> Backlink
   342                          
   343  0923 20a5e4             cw1	jsr setptr		; A/X -> ptr (Alt-String-Zeiger)
   344                          
   345  0926 a000               	ldy #0
   346  0928 b122               	lda (ptr),y		; Backlink low oder Lückenlänge
   347  092a c8                 	iny			; Y=1
   348  092b aa                 	tax			; -> X
   349  092c b122               	lda (ptr),y		; Backlink high
   350  092e c9ff               	cmp #$ff		; "String-nicht gebraucht"-Markierung
   351  0930 900c               	bcc cwactive		; aktiver String
   352                          
   353  0932 8a                 	txa			; Lückenlänge
   354  0933 49ff               	eor #$ff		; negieren, C=1 (Komplement, +1)
   355  0935 6522               	adc ptr			; (ptr - Lückenlänge)
   356  0937 a623               	ldx ptr+1 
   357  0939 b0da               	bcs cwnext		; weiter mit nächstem/r String/Lücke
   358  093b ca                 	dex			; High Byte
   359                          
   360  093c d0d7               cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   361                          				; weiter mit nächstem/r String/Lücke
   362                          
   363                          ; einen aktiven String nach oben schieben
   364                          
   365                          cwactive			; immer mit Y=1 angesprungen
   366  093e 8560               	sta descptr+1		; Descriptor-Adresse
   367  0940 865f               	stx descptr 
   368                          
   369  0942 a54e               	lda newptr		; Aufgeräumtzeiger -= 2
   370  0944 e901               	sbc #1			; weil bereits C=0!
   371  0946 854e               	sta newptr		; newptr -= 2
   372  0948 b003               	bcs cw3
   373  094a c64f               	dec newptr+1
   374  094c 38                 	sec			; für SBC unten
   375                          
   376  094d a9ff               cw3	lda #$ff		; Backlink h: als Lücke markieren
   377  094f 914e               	sta (newptr),y		; Y=1
   378  0951 88                 	dey			; Y=0
   379                          !ifdef no_opt_3 {
   380                          	lda (descptr),y		; Descriptor: String-Länge
   381                          	sta (newptr),y		; Backlink l: Lückenlänge
   382                          } else {
   383                          				; Backlink l: Lückenlänge später beim
   384                          				; Kopieren ...
   385                          }
   386  0952 a54e               	lda newptr		; Aufgeräumtzeiger -= String-Länge
   387  0954 f15f               	sbc (descptr),y		; minus String-Länge, immer C=1, Y=0
   388  0956 854e               	sta newptr
   389  0958 b003               	bcs cw4
   390  095a c64f               	dec newptr+1
   391  095c 38                 	sec			; für SBC unten
   392                          
   393  095d a522               cw4	lda ptr			; Alt-String-Zeiger -= String-Länge
   394  095f f15f               	sbc (descptr),y		; immer C=1
   395  0961 8522               	sta ptr			; Arbeitszeiger = alte String-Adresse
   396  0963 b002               	bcs cw5
   397  0965 c623               	dec ptr+1
   398                          cw5
   399                          	; An dieser Stelle wäre eine Optimierung möglich, um das
   400                          	; Kopieren zu verhindern, wenn der String an der gleichen
   401                          	; Stelle bleibt - dabei darf die Optimierung #3 nicht
   402                          	; in Verwendung sein und es würden zusätzlich 10 Bytes gebraucht!
   403                          !ifdef opt_no_copy {
   404                          	cmp newptr		; ptr bereits in A
   405                          	bne cw6			; ptr != newptr, also kopieren
   406                          	lda ptr+1		; High Byte ...
   407                          	cmp newptr+1
   408                          	beq cwheapordered	; ptr = newptr, nicht kopieren
   409                          cw6
   410                          }
   411                          
   412  0967 b15f               	lda (descptr),y		; String-Länge
   413                          !ifndef use_fast_copy {
   414                          
   415                          				; immer, da Länge >0
   416                          !ifdef no_opt_3 {
   417                          	beq cwnocopy		; falls doch Länge 0, kein Kopieren,
   418                          				; Descriptor trotzdem anpassen ...
   419                          	tay			; als Index, mit Dekrementieren beginnen
   420                          } else { ; mit Optimierung #3
   421                          	tay			; Länge als Index
   422                          	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   423                          				; mit Lückenlänge belegen
   424                          }
   425                          cwloop	dey			; -> Startindex fürs Kopieren
   426                          	lda (ptr),y		; Arbeitszeiger mit altem String
   427                          cwbllen sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   428                          	tya			; Test auf Z-Flag!
   429                          	bne cwloop		; Index = 0 -> fertig kopiert
   430                          
   431                          } else { ; use_fast_copy!
   432                          
   433                          				; + 3 Byte, -2 T/Zeichen 
   434  0969 a8                 	tay			; Länge als Index
   435                          !ifdef no_opt_3 {
   436                          	bne cwentry		; immer, da Länge in Y>0, bei
   437                          				; Dekrementieren beginnen!
   438                          } else { ; mit Optimierung #3
   439  096a d002               	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   440                          				; mit Lückenlänge belegen
   441                          }
   442                          				; -> Startindex fürs Kopieren
   443  096c b122               cwloop	lda (ptr),y		; Arbeitszeiger mit altem String
   444  096e 914e               cwbllen	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   445  0970 88                 cwentry	dey			; Test auf Z-Flag!
   446  0971 d0f9               	bne cwloop		; Index = 0 -> fertig kopiert
   447  0973 b122               cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   448  0975 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   449                          }
   450                          
   451                          cwnocopy:
   452                          				; Y=0
   453  0977 c8                 	iny			; Y=1
   454  0978 a54e               	lda newptr		; im Descriptor:
   455  097a 915f               	sta (descptr),y		; String-Adresse L: neue Adresse
   456  097c c8                 	iny			; Y=2
   457  097d a54f               	lda newptr+1
   458  097f 915f               	sta (descptr),y		; String-Adresse H: neue Adresse
   459                          
   460                          cwheapordered:
   461  0981 a522               	lda ptr
   462  0983 a623               	ldx ptr+1		; High-Byte immer !=0
   463  0985 d08e               	bne cwnext		; immer; weiter in Schleife
   464                          
   465                          
   466                          ;**** Backlink setzen
   467                          ;
   468                          ; 	in:		ptr	Descriptor-Adresse
   469                          ; 	out:		ptr	Descriptor-Adresse
   470                          ;			A/X
   471                          ;			Z=0	wenn nicht am SDS
   472                          ;			Z=1	wenn am SDS
   473                          ;	destroy:	newptr
   474                          ;	called:		blaset, backlinkvar
   475                          
   476                          backlink:
   477  0987 a000               	ldy #0
   478  0989 b122               	lda (ptr),y		; String-Länge
   479  098b f023               	beq blnext		; fertig, wenn =0
   480  098d c8                 	iny
   481  098e 18                 	clc
   482  098f 7122               	adc (ptr),y		; Backlink-Position (am String-Ende)
   483  0991 854e               	sta newptr		; Backlink-Zeiger L
   484  0993 aa                 	tax
   485  0994 c8                 	iny
   486  0995 b122               	lda (ptr),y
   487  0997 6900               	adc #0
   488  0999 854f               	sta newptr+1		; Backlink-Zeiger H
   489  099b c532               	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
   490  099d 9011               	bcc blnext		; ja, denn nächsten String
   491  099f d004               	bne blsetdesc
   492  09a1 e431               	cpx strend 
   493  09a3 900b               	bcc blnext		; < Array-Bereichende (außerhalb Heap)?
   494                          
   495                          blsetdesc:
   496  09a5 a001               	ldy #1
   497  09a7 a523               	lda ptr+1
   498  09a9 914e               	sta (newptr),y		; Descriptor-Adresse ...
   499  09ab 88                 	dey
   500  09ac a522               	lda ptr
   501  09ae 914e               	sta (newptr),y		; in den Backlink übertragen
   502                          
   503  09b0 a553               blnext	lda desclen		; nächster String/nächste Variable
   504  09b2 18                 	clc			; Schrittweite zum nächsten Descriptor
   505  09b3 6522               	adc ptr			; ptr += desclen
   506  09b5 8522               	sta ptr
   507  09b7 9002               	bcc +
   508  09b9 e623               	inc ptr+1
   509  09bb a623               +	ldx ptr+1		; immer != 0 -> Z=0 (außer bei SDS, Z=1)
   510  09bd 60                 	rts
   511                          
   512                          ;**** Nächste String-Variable und Backlink setzen
   513                          ;
   514                          ; 	in:		ptr	Variablenadresse
   515                          ; 	out:		ptr	Variablenadresse
   516                          ;			A/X
   517                          ;			Z=0
   518                          ;	destroy:	newptr
   519                          ;	called:		varbl (vars)
   520                          
   521                          backlinkvar:
   522  09be a000               	ldy #0
   523  09c0 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   524  09c2 aa                 	tax			; Typstatus merken
   525  09c3 c8                 	iny
   526  09c4 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   527  09c6 a8                 	tay			; Typstatus merken
   528                          
   529  09c7 a902               	lda #2			; Descriptor-Adresse (in Variable)
   530  09c9 18                 	clc
   531  09ca 6522               	adc ptr			; ptr += 2
   532  09cc 8522               	sta ptr
   533  09ce 9002               	bcc +
   534  09d0 e623               	inc ptr+1
   535                          +
   536  09d2 8a                 	txa			; Variablentyp prüfen
   537  09d3 30db               	bmi blnext		; keine String, nächste Variable
   538  09d5 98                 	tya
   539  09d6 30af               	bmi backlink		; Backlink setzen
   540  09d8 10d6               	bpl blnext		; keine String-Var., nächste Variable
   541                          
   542                          }
   543                          part1_real_end
   544                          
   545                          ; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!
   546                          
   547                          !set part1_end = (part1_real_end-part1_real)+part1
   548                          !if ( part1_end > $B63D ) {
   549                          	!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   550                          }
   551                          
   552                          
   553                          ; ******************************* part 4 *************************************
   554                          
   555                          part4_real
   556                          !pseudopc $b6c1 {
   557                          
   558                          part4:
   559                          
   560                          part4_continue = $b6d6
   561  09da 4cd6b6             	jmp part4_continue
   562                          
   563                          ;**** Nächste Array-Variable und Backlink setzen
   564                          ;
   565                          ; 	in: 		ptr	Arrayadresse
   566                          ; 	out:		ptr	Adresse Folge-array
   567                          ;			aryptr	Adresse Folge-array
   568                          ;			A/X	Adresse Folge-array
   569                          ;			Z=0
   570                          ;	destroy:	newptr
   571                          ;	called:		arrbl (arrays)
   572                          
   573                          backlinkarr:
   574  09dd a000               	ldy #0
   575  09df b122               	lda (ptr),y		; Variablenname 1. Zeichen
   576  09e1 08                 	php			; für später
   577  09e2 c8                 	iny
   578  09e3 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   579  09e5 aa                 	tax			; für später
   580                          
   581  09e6 c8                 	iny
   582  09e7 b122               	lda (ptr),y		; Offset nächstes Array
   583  09e9 18                 	clc			; Bugfix 1: C=0 definiert setzen
   584  09ea 6558               	adc aryptr
   585  09ec 4c75e4             	jmp backlinkarr2
   586                          				; weiter an anderer Stelle!
   587                          blapast
   588                          !if blapast > part4_continue {
   589                          	!error "part4 ist zu lang!"
   590                          }
   591                          }
   592                          part4_real_end
   593                          
   594                          
   595                          ; ******************************* part 3 *************************************
   596                          
   597                          part3_real
   598                          !pseudopc $e474 {
   599                          
   600                          part3:
   601                          
   602  09ef 00                 	!byte  0 		; Einschaltmeldung kürzen
   603                          
   604                          backlinkarr2:
   605  09f0 8558               	sta aryptr		; Folge-Array L
   606  09f2 c8                 	iny
   607  09f3 b122               	lda (ptr),y
   608  09f5 6559               	adc aryptr+1 
   609  09f7 8559               	sta aryptr+1		; Folge-Array H
   610                          
   611  09f9 28                 	plp			; Arraytyp:
   612  09fa 3020               	bmi blaskip		; kein String-Array
   613  09fc 8a                 	txa
   614  09fd 101d               	bpl blaskip		; kein String-Array
   615                          
   616  09ff c8                 	iny			; Y=4
   617  0a00 b122               	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
   618  0a02 0a                 	asl 			; *2
   619  0a03 6905               	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
   620  0a05 6522               	adc ptr			; auf 1. Element ...
   621  0a07 8522               	sta ptr 
   622  0a09 9002               	bcc bla1
   623  0a0b e623               	inc ptr+1 
   624  0a0d a623               bla1	ldx ptr+1		; positionieren
   625                          
   626  0a0f e459               blanext	cpx aryptr+1		; Array-Ende erreicht?
   627  0a11 d004               	bne blaset		; nein, Backlink setzen
   628  0a13 c558               	cmp aryptr
   629  0a15 f007               	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
   630                          blaset
   631  0a17 20e6b5             	jsr backlink		; Backlink setzen
   632  0a1a d0f3               	bne blanext		; immer (High-Byte != 0)
   633                          
   634                          blaskip
   635  0a1c a558               	lda aryptr		; Zeiger auf Folge-Array
   636                          blafinish
   637  0a1e a659               	ldx aryptr+1 		; Z=0 sicherstellen
   638                          
   639  0a20 8522               setptr	sta ptr			; Arbeitszeiger setzen
   640  0a22 8623               	stx ptr+1
   641  0a24 60                 	rts			; immer Z=0
   642                          
   643                          ;--- $e4b7 - $e4d2 unused ($aa)
   644                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   645                          ;----              sonst Patch für andere Zwecke
   646                          
   647                          }
   648                          part3_real_end
   649                          
   650                          
   651                          ; ******************************* part 2 *************************************
   652                          
   653                          part2_real
   654                          !pseudopc $e4ba {
   655                          
   656                          part2:
   657                          
   658                          ;**** String Allocation (Fortsetzung)
   659                          ;
   660                          ;	in: 	TOS		; Länge
   661                          ;		fretop		; String-Adresse
   662                          ;	out:	fretop		; String-Adresse
   663                          ;		strptr		; String-Adresse (wird nicht verwendet)
   664                          ;				; (bei alternate_stralloc eventuell mit
   665                          ;				; inkrementiertem High-Byte)
   666                          ;		A		; Länge
   667                          ;		X,Y		; String-Adresse (L,H)
   668                          ;	called:	allocate (in Fortsetzung)
   669                          
   670                            !ifndef alternate_stralloc {
   671                          stralloc:
   672  0a25 8535               	sta strptr		; strptr = A/X = FRETOP
   673  0a27 8636               	stx strptr+1
   674  0a29 aa                 	tax			; A in X aufheben
   675  0a2a 68                 	pla			; Länge temp. vom Stack 
   676  0a2b 48                 	pha			; wieder auf Stack, nun auch in A
   677  0a2c a8                 	tay			; Index=Länge (Backlink-position)
   678  0a2d 9133               	sta (fretop),y		; Backlink L = String-/Lückenlänge
   679  0a2f c8                 	iny			; Y=Länge+1
   680  0a30 d002               	bne sa1			; wenn Länge=255, dann
   681  0a32 e634               	inc fretop+1		; Überlauf, aber nur temporär!
   682                          
   683  0a34 a9ff               sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   684  0a36 9133               	sta (fretop),y
   685  0a38 a436               	ldy strptr+1
   686  0a3a 8434               	sty fretop+1		; Überlaufkorr. rückgängig
   687  0a3c 68                 	pla			; Länge vom Stack nehmen
   688  0a3d 60                 	rts
   689                          
   690                            } else {
   691                          ; alternative, etwas kürzere Variante (-3 T, -2 B)
   692                          
   693                          stralloc:
   694                          	sta strptr		; strptr = A/X = FRETOP
   695                          	stx strptr+1
   696                          	tax			; A in X aufheben
   697                          	pla			; Länge temp. vom Stack 
   698                          	pha			; wieder auf Stack, nun auch in A
   699                          	tay			; Index=Länge (Backlink-position)
   700                          	sta (strptr),y		; Backlink L = String-/Lückenlänge
   701                          	iny			; Y=Länge+1
   702                          	bne sa1			; wenn Länge=255, dann
   703                          	inc strptr+1		; Überlauf, aber nur temporär!
   704                          
   705                          sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   706                          	sta (strptr),y
   707                          	ldy fretop+1		; in Y String-Adresse High-Byte
   708                          	pla			; Länge vom Stack nehmen
   709                          	rts
   710                          				; Hier weicht strptr+1 u.U. von fretop+1 ab,
   711                          				; was aber kein Problem darstellt, da
   712                          				; es im BASIC-Interpreter keine Stellt gibt,
   713                          				; die nach einem allocate-Aufruf den
   714                          				; Pointer strptr/strptr+1 verwendet!
   715                            }
   716                          }
   717                          
   718                          part2_real_end
   719                          
   720                          
   721                          ; Einsprungspunkt an korrekter Position?
   722                          
   723                          ; Kann erst nach dem Label docollect gemacht werden!
   724                          
   725                          !if (garcoll != docollect) {
   726                          	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
   727                          }
