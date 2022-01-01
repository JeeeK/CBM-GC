
; ******** Source: garbcol.asm
     1                          !to "garbcol.o",cbm	
     2                          ;
     3                          ;  **** Garbage Collection ****
     4                          ;
     5                          ; 64'er, Oct. 1988
     6                          ;
     7                          ; In zwei Schritten wird der String-Speicher kompaktiert:
     8                          ;   1) Alle Strings im String-Descriptor-Stack (SDS),
     9                          ;      in Variablen und Arrays werden erhalten im
    10                          ;      Back-Link den Verweis auf den Descriptor.
    11                          ;      Nicht mehr referenzierte Strings bleiben als
    12                          ;      ungenutzt markiert und verweisen auf den
    13                          ;      nächsten String.
    14                          ;   2) Nun wird der String-Speicher absteigend durchgegangen,
    15                          ;      wobei nur die aktiven Strings nach "oben" über
    16                          ;      etwaige Lücken hinweg kopiert werden. Die ungenutzten
    17                          ;      Lücken werden dabei übergangen.
    18                          ;      Beim Kopieren wird die Back-Link-Markierung wieder
    19                          ;      entfernt, da ja bis zur nächsten Kompaktierung
    20                          ;      der Speicher aufgegeben werden könnte.
    21                          ; Im Vergleich zu der von CBM eingesetzten Routine, wird
    22                          ; hier auf eine Code-intensive Optimierung verzichtet,
    23                          ; wenn ein Teil oder der gesamt String-Speicher schon
    24                          ; kompaktiert sein sollte. Es werden dann die Strings
    25                          ; im schlimmsten Fall wieder über sich selbst kopiert.
    26                          ;
    27                          ; Überarbeitetet und korrigiert:
    28                          ;	2013-11-15 Johann E. Klasek, johann at klasek at
    29                          ;
    30                          ; Bugfixes:
    31                          ;	1) in backlinkarr:
    32                          ;	   C-Flag ist beim Errechnen des Folge-Arrays
    33                          ;	   definiert gelöscht. 
    34                          ;          Sonst werden ev. nicht alle Elemente
    35                          ;          aller Arrays mit einem korrekten
    36                          ;          Backlink versehen und der
    37                          ;          String-Heap wird durch die GC
    38                          ;          korrumpiert!
    39                          ;	2) in backlinkarr bei blanext:
    40                          ;	   Muss zum Aufrufer immer mit Z=0
    41                          ;	   rückkehren, und erkennt
    42                          ;	   sonst immer nur das 1. Array!
    43                          ;	   Damit liegen die anderen Strings
    44                          ;	   dann im freien Bereich und
    45                          ;	   werden nach und nach überschrieben!
    46                          ;
    47                          ; Optimierungen:
    48                          ;    
    49                          ;     * Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
    50                          ;	Wegen des längeren Codes und und einem deswegen länger werdenden
    51                          ;	Branch-Offset, wurde der Code ab cfinish zurückversetzt.
    52                          ;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
    53                          ;	noch an anderer Stelle eingespart werden.
    54                          ;	Dabei wird unmittelbar vor dem Kopieren des Strings nicht
    55                          ;	mehr explizit der Fall eines Strings mit Länge 0 betrachtet,
    56                          ;	der an dieser Stelle auch nicht auftreten kann, da
    57                          ;	der erste Durchgang durch alle Strings am SDS, bei den
    58                          ;	Variablen und den Arrays Strings der Länge 0 übergeht. 
    59                          ;	Selbst, wenn jemand böswilligerweise via allocate-Routine
    60                          ;	0-Längen-Speicher anfordert (was immer 2 Link-Bytes kostet),
    61                          ;	können diese Leerstring nicht referenziert werden. Im Zuge
    62                          ;	des zweiten Durchlaufs (Collection) würden diese degenerieren
    63                          ;	0-Längen-Strings auch wieder verschwinden.
    64                          ;
    65                          ;	Aktivierbar via use_fast_copy-Variable.
    66                          ;
    67                          ;     * allocate etwas kompakter/schneller (-2 Byte Code, -3 T)
    68                          ;       Der Back-Link wird via strptr/strptr+1 gesetzt, wobei
    69                          ;       bei einem String länger 253 Bytes das High-Byte in strptr+1
    70                          ;	erhöht wird, statt dies mit fretop+1 zu machen, welches
    71                          ;	dann restauriert werden muss.
    72                          ;       
    73                          ;	Aktivierbar via alternate_stralloc-Variable.
    74                          ;
    75                          
    76                          
    77                          ; Die optimierte Kopierroutine verwenden (siehe oben "Optimierungen"):
    78                          !set use_fast_copy=1
    79                          
    80                          
    81                          
    82                          ; Basic-Zeiger und -konstanten
    83                          
    84                          collected = $0f
    85                          
    86                          sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
    87                          			; wächst nach oben, max. 3 Elemente
    88                          			; zu je 3 Bytes.
    89                          sdsptr   = $16		; Zeiger auf nächstes freie Element
    90                          			; des String-Descriptor-Stacks (SDS)
    91                          
    92                          vartab   = $2d		; Basicprogrammende = Variablenanfang
    93                          arytab   = $2f		; Variablenende = Array-Bereichanfang
    94                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
    95                          fretop   = $33		; aktuelle String-Heap-Adresse
    96                          strptr	 = $35		; temporärer Stringzeiger
    97                          memsiz   = $37		; höchste RAM-Adresse für Basic, Start
    98                          			; des nach unten wachsenden String-Heaps
    99                          ; Hilfsvariablen
   100                          
   101                          ptr	 = $22		; Arbeitszeiger
   102                          newptr	 = $4e		; Neuer Stringzeiger
   103                          desclen	 = $53		; akt. Länge eines Stringdescriptors
   104                          aryptr	 = $58		; Array-Zeiger
   105                          descptr	 = $5f		; Descriptor-Zeiger
   106                          
   107                          garcoll  = $b526
   108                          
   109                          ; Vorbelegung der Speicherplätze
   110                          
   111                          romsize  = $2000	; ROM Länge 8K
   112                          
   113                          prozport = $01		; Prozessorport
   114                          memrom = %00110111	; Basic+Kernal ROM
   115                          membas = %00110110	; Basic RAM+kernal ROM
   116                          memram = %00110101	; Basic+Kernal RAM
   117                          
   118                          
   119                          ; Datenstrukturen
   120                          ;
   121                          ; String am Heap:
   122                          ;
   123                          ;   +--------------------------------------+
   124                          ;   |       +--------------+               |
   125                          ;   V       |              V               |
   126                          ;   +---+---+---+          +-----------+---+---+
   127                          ;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
   128                          ;   +---+---+---+          +-----------+---+---+
   129                          ;   ^    *******           ^            *******
   130                          ;   |       String.adr.    |               Descriptor-Adr.
   131                          ;   +-Descriptor-Adresse   +-Stringadresse
   132                          ;
   133                          ; Lücken am Heap:
   134                          ;                      
   135                          ;   +-------------+ +----------------+
   136                          ;   V             | V                |
   137                          ;    +-----------+---+---+---------+---+---+
   138                          ;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
   139                          ;    +-----------+---+---+---------+---+---+
   140                          ;                  ^  ***            ^  ***
   141                          ;                  |   Lückenmark.   |   Lückenmarkierung
   142                          ;                  Backlink-Adresse  Backlink-Adresse
   143                          
   144                          
   145                          

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
   147                          
   148                          ;
   149                          ; Patch-Liste für "loader"
   150                          ;
   151                          
   152                          patchlist:
   153                          
   154  087b 95084901f4b4       !wo part1_real,part1_real_end-part1_real,part1
   155  0881 290a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
   156  0887 f309360074e4       !wo part3_real,part3_real_end-part3_real,part3
   157  088d de091500c1b6       !wo part4_real,part4_real_end-part4_real,part4
   158  0893 0000               !wo 0  ; Endemarkierung
   159                          
   160                          
   161                          ; ******************************* part 1 *************************************
   162                          
   163                          part1_real:
   164                          
   165                          !pseudopc $b4f4 {
   166                          
   167                          part1:
   168                          
   169                          ;***** Speicher von String-Heap anfordern
   170                          ;
   171                          ;	in:	A		; Länge anforderung
   172                          ;		fretop
   173                          ;	mod:	collected	; "GC aufgerufen" Flag
   174                          ;		strptr		; temp. Zeiger
   175                          ;	out:	fretop		; Adresse auf String
   176                          ;		X,Y		; Adresse auf String
   177                          ;
   178                          ; Der String wird im Backlink stets als ungebrauchte Lücke
   179                          ; markiert! Dann muss die GC nur noch die Backlinks
   180                          ; der aktiven Strings setzen und kann die ungebrauchten
   181                          ; Strings überspringen.
   182                          
   183                          
   184                          basicerror = $b4d2		; Basic-Fehlermeldung
   185                          
   186                          allocate:
   187  0895 460f               	lsr collected		; Flag löschen
   188  0897 48                 retry	pha			; Länge der Anforderung,
   189                          				; für 2. Teil
   190                          				; Länge 0 möglich, verbraucht aber 2 Bytes
   191  0898 49ff               	eor #$ff		; negieren
   192  089a 38                 	sec
   193  089b 6533               	adc fretop		; A/X = fretop; A/X -= Länge
   194  089d a634               	ldx fretop+1
   195  089f b002               	bcs l1
   196  08a1 ca                 	dex
   197  08a2 38                 	sec
   198  08a3 e902               l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
   199  08a5 b001               	bcs l2
   200  08a7 ca                 	dex
   201  08a8 e432               l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
   202  08aa 9006               	bcc checkcollect
   203  08ac d013               	bne alloc		; nein, Bereich anfordern
   204  08ae c531               	cmp strend 
   205  08b0 b00f               	bcs alloc		; nein, Bereich anfordern
   206                          checkcollect
   207  08b2 a210               	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
   208  08b4 a50f               	lda collected
   209  08b6 30bb               	bmi basicerror		; Collection schon gelaufen?
   210  08b8 2026b5             	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
   211  08bb 660f               	ror collected		; Flag setzen (Bit 7) setzen
   212  08bd 68                 	pla			; Länge angeforderter Bereich
   213  08be 4cf6b4             	jmp retry		; nochmal versuchen (ob durch GC Platz frei wurde)
   214                          
   215  08c1 2067b5             alloc	jsr setfretop		; FRETOP = A/X
   216  08c4 4cbae4             	jmp stralloc		; zum 2. Teil: Allokation abschließen
   217                          
   218                          
   219                          ;***** garbage collection
   220                          
   221                          ;	in:	-
   222                          ;	mod:	ptr		; Zeiger auf alten String-Heap
   223                          ;		newptr		; Zeiger auf neuen String-Heap
   224                          ;		descptr		; Zeiger auf Descriptor
   225                          ;		desclen		; Descriptor-Schrittweite
   226                          ;	out:	fretop		; Neue String-Heap-Position
   227                          ;		C=1
   228                          
   229                          docollect
   230                          
   231                          
   232                          ; Backlink aller temporären Strings am String-Descriptor-Stack setzen
   233                          
   234  08c7 a919               sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   235  08c9 a200               	ldx #>sdsbase		; da in 0-Page, immer 0
   236  08cb 20a5e4             	jsr setptr		; damit ptr setzen
   237                          
   238  08ce c516               sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   239  08d0 f005               	beq vars		; Ja, SDS durch, weiter mit Variablen
   240  08d2 20eab5             	jsr backlink		; sonst Backlink setzen
   241  08d5 f0f7               	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descriptor
   242                          
   243                          ; Backlink aller String-Variablen setzen
   244                          
   245  08d7 a905               vars:	lda #5			; Descriptor-Schritt für Variablen
   246  08d9 8553               	sta desclen
   247  08db a52d               	lda vartab		; Variablenbeginn
   248  08dd a62e               	ldx vartab+1
   249  08df 20a5e4             	jsr setptr		; ptr = A/X
   250                          
   251  08e2 e430               varnext	cpx arytab+1		; Variablenende?
   252  08e4 d004               	bne varbl
   253  08e6 c52f               	cmp arytab
   254  08e8 f005               	beq arrays		; ja, weiter mit Arrays
   255  08ea 2021b6             varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
   256  08ed d0f3               	bne varnext		; immer; nächsten Var.-Descriptor
   257                          
   258                          ; Backlink bei allen String-Arrays setzen
   259                          
   260                          arrays:
   261  08ef 8558               	sta aryptr		; Variablenbereichende = Array-Bereichanfang
   262  08f1 8659               	stx aryptr+1 
   263  08f3 a003               	ldy #3			; Descriptor-Schritt bei String-Arrays
   264  08f5 8453               	sty desclen
   265                          
   266  08f7 e432               arrnext	cpx strend+1		; Array-Bereichende?
   267  08f9 d004               	bne arrbl
   268  08fb c531               	cmp strend
   269  08fd f00e               	beq cleanwalk
   270  08ff 20c4b6             arrbl	jsr backlinkarr		; Backlinks für nächstes String-Array setzen -> Z=0!
   271  0902 d0f3               	bne arrnext		; immer; nächstes Array-Element
   272                          
   273                          
   274                          ; Ende, Zeiger zum neuen String-Heap übernehmen
   275                          
   276                          cfinish
   277  0904 a54e               	lda newptr		; Aufgeräumtzeiger ist ..
   278  0906 a64f               	ldx newptr+1
   279                          setfretop
   280  0908 8533               	sta fretop		; neues FRETOP
   281  090a 8634               	stx fretop+1 
   282  090c 60                 	rts			; fertig!
   283                          
   284                          ; Nachdem nun alle Backlinks gesetzt sind
   285                          ; den String-Heap von oben nach unten durchgehen
   286                          ; und zusammenschieben ...
   287                          
   288                          cleanwalk:
   289  090d a537               	lda memsiz		; beim Basic-Speicherende
   290  090f a638               	ldx memsiz+1
   291  0911 854e               	sta newptr		; ... beginnen
   292  0913 864f               	stx newptr+1 
   293                          
   294                          ; Aufräumschleife
   295                          
   296  0915 e434               cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   297  0917 d004               	bne cwclean		; dann Heap durch und fertig.
   298  0919 c533               	cmp fretop		; andernfalls aufräumen ...
   299  091b f0e7               	beq cfinish		; fertig, weil A/X = FRETOP
   300                          
   301                          ; nächsten String "aufräumen" ...
   302                          
   303  091d 38                 cwclean	sec			; Aufräumtzeiger auf backlink
   304  091e e902               	sbc #2
   305  0920 b001               	bcs cw1
   306  0922 ca                 	dex			; A/X -> Backlink
   307                          
   308  0923 20a5e4             cw1	jsr setptr		; A/X -> ptr (Arbeitszeiger)
   309                          
   310  0926 a000               	ldy #0
   311  0928 b122               	lda (ptr),y		; Backlink low oder Lückenlänge
   312  092a c8                 	iny			; Y=1
   313  092b aa                 	tax			; -> X
   314  092c b122               	lda (ptr),y		; Backlink high
   315  092e c9ff               	cmp #$ff		; String "nicht gebraucht" Markierung
   316  0930 900c               	bcc cwactive		; aktiver String
   317                          
   318  0932 8a                 	txa			; Lückenlänge
   319  0933 49ff               	eor #$ff		; negieren
   320  0935 6522               	adc ptr			; (ptr - Lückenlänge)
   321  0937 a623               	ldx ptr+1 
   322  0939 b0da               	bcs cwnext		; weiter ...
   323  093b ca                 	dex			; High Byte
   324                          
   325  093c d0d7               cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   326                          
   327                          ; einen aktiven String nach oben schieben
   328                          
   329                          cwactive			; immer mit Y=1 angesprungen
   330  093e 8560               	sta descptr+1		; Descriptor-Adresse
   331  0940 865f               	stx descptr 
   332                          
   333  0942 a54e               	lda newptr		; Aufgeräumtzeiger -= 2
   334  0944 e901               	sbc #1			; weil bereits C=0!
   335  0946 854e               	sta newptr		; newptr -= 2
   336  0948 b003               	bcs cw3
   337  094a c64f               	dec newptr+1
   338  094c 38                 	sec			; für SBC unten
   339                          
   340  094d a9ff               cw3	lda #$ff		; Backlink h: als Lücke markieren
   341  094f 914e               	sta (newptr),y		; Y=1
   342  0951 88                 	dey			; Y=0
   343  0952 b15f               	lda (descptr),y		; Descriptor: String-länge
   344  0954 914e               	sta (newptr),y		; Backlink l: Lückenlänge
   345                          
   346  0956 a54e               	lda newptr		; Aufgeräumtzeiger -= String-Länge
   347  0958 f15f               	sbc (descptr),y		; immer C=1
   348  095a 854e               	sta newptr
   349  095c b003               	bcs cw4
   350  095e c64f               	dec newptr+1
   351  0960 38                 	sec			; für SBC unten
   352                          
   353  0961 c8                 cw4	iny			; Y=1
   354  0962 915f               	sta (descptr),y		; String-Adresse L: neue Adresse
   355  0964 c8                 	iny			; Y=2
   356  0965 a54f               	lda newptr+1
   357  0967 915f               	sta (descptr),y		; String-Adresse H: neue Adresse
   358  0969 a000               	ldy #0
   359  096b a522               	lda ptr
   360  096d f15f               	sbc (descptr),y		; immer C=1
   361  096f 8522               	sta ptr			; Arbeitszeiger = alte String-Adresse
   362  0971 b002               	bcs cw5
   363  0973 c623               	dec ptr+1
   364                          cw5
   365  0975 b15f               	lda (descptr),y		; String-Länge
   366                          
   367                          !ifndef use_fast_copy {
   368                          
   369                          	beq cwnocopy		; wenn =0, dann nicht kopieren
   370                          	tay			; Länge
   371                          cwloop	dey			; -> Startindex fürs Kopieren
   372                          	lda (ptr),y		; Arbeitszeiger mit altem String
   373                          	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   374                          	tya			; Test auf Z-Flag!
   375                          	bne cwloop		; Index = 0 -> fertig kopiert
   376                          
   377                          } else {
   378                          
   379                          				; + 3 Byte, -2 T/Zeichen 
   380  0977 a8                 	tay			; Länge
   381  0978 d004               	bne cwentry		; immer, da Länge in Y>0
   382                          cwloop				; -> Startindex fürs Kopieren
   383  097a b122               	lda (ptr),y		; Arbeitszeiger mit altem String
   384  097c 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   385  097e 88                 cwentry	dey			; Test auf Z-Flag!
   386  097f d0f9               	bne cwloop		; Index = 0 -> fertig kopiert
   387  0981 b122               cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   388  0983 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   389                          
   390                          }
   391                          
   392                          cwnocopy
   393  0985 a522               	lda ptr
   394  0987 a623               	ldx ptr+1		; High-Byte immer !=0
   395  0989 d08a               	bne cwnext		; immer; weiter in Schleife
   396                          
   397                          
   398                          ;**** Backlink setzen
   399                          ;
   400                          ; 	in:		ptr	Descriptor-Adresse
   401                          ; 	out:		ptr	Descriptor-Adresse
   402                          ;			A/X
   403                          ;			Z=0	wenn nicht am SDS
   404                          ;			Z=1	wenn am SDS
   405                          ;	destroy:	newptr
   406                          ;	called:		blaset, backlinkvar
   407                          
   408                          backlink:
   409  098b a000               	ldy #0
   410  098d b122               	lda (ptr),y		; String-Länge
   411  098f f023               	beq blnext		; fertig, wenn =0
   412  0991 c8                 	iny
   413  0992 18                 	clc
   414  0993 7122               	adc (ptr),y		; Backlink-Position (am String-Ende)
   415  0995 854e               	sta newptr		; Backlink-Zeiger L
   416  0997 aa                 	tax
   417  0998 c8                 	iny
   418  0999 b122               	lda (ptr),y
   419  099b 6900               	adc #0
   420  099d 854f               	sta newptr+1		; Backlink-Zeiger H
   421  099f c532               	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
   422  09a1 9011               	bcc blnext		; ja, denn nächsten String
   423  09a3 d004               	bne blsetdesc
   424  09a5 e431               	cpx strend 
   425  09a7 900b               	bcc blnext		; < Array-Bereichende (außerhalb Heap)?
   426                          
   427                          blsetdesc
   428  09a9 a001               	ldy #1
   429  09ab a523               	lda ptr+1
   430  09ad 914e               	sta (newptr),y		; Descriptor-Adresse ...
   431  09af 88                 	dey
   432  09b0 a522               	lda ptr
   433  09b2 914e               	sta (newptr),y		; in den Backlink übertragen
   434                          
   435  09b4 a553               blnext	lda desclen		; nächster String/nächste Variable
   436  09b6 18                 	clc			; Schrittweite zum nächsten Descriptor
   437  09b7 6522               	adc ptr			; ptr += desclen
   438  09b9 8522               	sta ptr
   439  09bb 9002               	bcc +
   440  09bd e623               	inc ptr+1
   441  09bf a623               +	ldx ptr+1		; immer != 0 -> Z=0 (außer bei SDS, Z=1)
   442  09c1 60                 	rts
   443                          
   444                          ;**** Nächste String-Variable und Backlink setzen
   445                          ;
   446                          ; 	in:		ptr	Variablenadresse
   447                          ; 	out:		ptr	Variablenadresse
   448                          ;			A/X
   449                          ;			Z=0
   450                          ;	destroy:	newptr
   451                          ;	called:		varbl (vars)
   452                          
   453                          backlinkvar:
   454  09c2 a000               	ldy #0
   455  09c4 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   456  09c6 aa                 	tax			; Typstatus merken
   457  09c7 c8                 	iny
   458  09c8 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   459  09ca a8                 	tay			; Typstatus merken
   460                          
   461  09cb a902               	lda #2			; Descriptor-Adresse (in Variable)
   462  09cd 18                 	clc
   463  09ce 6522               	adc ptr			; ptr += 2
   464  09d0 8522               	sta ptr
   465  09d2 9002               	bcc +
   466  09d4 e623               	inc ptr+1
   467                          +
   468  09d6 8a                 	txa			; Variablentyp prüfen
   469  09d7 30db               	bmi blnext		; keine String, nächste Variable
   470  09d9 98                 	tya
   471  09da 30af               	bmi backlink		; Backlink setzen
   472  09dc 10d6               	bpl blnext		; keine String-Var., nächste Variable
   473                          
   474                          }
   475                          part1_real_end
   476                          
   477                          ; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!
   478                          
   479                          !set part1_end = (part1_real_end-part1_real)+part1
   480                          !if ( part1_end > $B63D ) {
   481                          	!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   482                          }
   483                          
   484                          
   485                          ; ******************************* part 4 *************************************
   486                          
   487                          part4_real
   488                          !pseudopc $b6c1 {
   489                          
   490                          part4:
   491                          
   492                          part4_continue = $b6d6
   493  09de 4cd6b6             	jmp part4_continue
   494                          
   495                          ;**** Nächste Array-Variable und Backlink setzen
   496                          ;
   497                          ; 	in: 		ptr	Arrayadresse
   498                          ; 	out:		ptr	Adresse Folge-array
   499                          ;			aryptr	Adresse Folge-array
   500                          ;			A/X	Adresse Folge-array
   501                          ;			Z=0
   502                          ;	destroy:	newptr
   503                          ;	called:		arrbl (arrays)
   504                          
   505                          backlinkarr:
   506  09e1 a000               	ldy #0
   507  09e3 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   508  09e5 08                 	php			; für später
   509  09e6 c8                 	iny
   510  09e7 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   511  09e9 aa                 	tax			; für später
   512                          
   513  09ea c8                 	iny
   514  09eb b122               	lda (ptr),y		; Offset nächstes Array
   515  09ed 18                 	clc			; Bugfix 1: C=0 definiert setzen
   516  09ee 6558               	adc aryptr
   517  09f0 4c75e4             	jmp backlinkarr2
   518                          				; weiter an anderer Stelle!
   519                          blapast
   520                          !if blapast > part4_continue {
   521                          	!error "part4 ist zu lang!"
   522                          }
   523                          }
   524                          part4_real_end
   525                          
   526                          
   527                          ; ******************************* part 3 *************************************
   528                          
   529                          part3_real
   530                          !pseudopc $e474 {
   531                          
   532                          part3:
   533                          
   534  09f3 00                 	!byte  0 		; Einschaltmeldung kürzen
   535                          
   536                          backlinkarr2:
   537  09f4 8558               	sta aryptr		; Folge-Array L
   538  09f6 c8                 	iny
   539  09f7 b122               	lda (ptr),y
   540  09f9 6559               	adc aryptr+1 
   541  09fb 8559               	sta aryptr+1		; Folge-Array H
   542                          
   543  09fd 28                 	plp			; Arraytyp:
   544  09fe 3020               	bmi blaskip		; kein String-Array
   545  0a00 8a                 	txa
   546  0a01 101d               	bpl blaskip		; kein String-Array
   547                          
   548  0a03 c8                 	iny			; Y=4
   549  0a04 b122               	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
   550  0a06 0a                 	asl 			; *2
   551  0a07 6905               	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
   552  0a09 6522               	adc ptr			; auf 1. Element ...
   553  0a0b 8522               	sta ptr 
   554  0a0d 9002               	bcc bla1
   555  0a0f e623               	inc ptr+1 
   556  0a11 a623               bla1	ldx ptr+1		; positionieren
   557                          
   558  0a13 e459               blanext	cpx aryptr+1		; Array-Ende erreicht?
   559  0a15 d004               	bne blaset		; nein, Backlink setzen
   560  0a17 c558               	cmp aryptr
   561  0a19 f007               	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
   562                          blaset
   563  0a1b 20eab5             	jsr backlink		; Backlink setzen
   564  0a1e d0f3               	bne blanext		; immer (High-Byte != 0)
   565                          
   566                          blaskip
   567  0a20 a558               	lda aryptr		; Zeiger auf Folge-Array
   568                          blafinish
   569  0a22 a659               	ldx aryptr+1 		; Z=0 sicherstellen
   570                          
   571  0a24 8522               setptr	sta ptr			; Arbeitszeiger setzen
   572  0a26 8623               	stx ptr+1
   573  0a28 60                 	rts			; immer Z=0
   574                          
   575                          ;--- $e4b7 - $e4d2 unused ($aa)
   576                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   577                          ;----              sonst Patch für andere Zwecke
   578                          
   579                          }
   580                          part3_real_end
   581                          
   582                          
   583                          ; ******************************* part 2 *************************************
   584                          
   585                          part2_real
   586                          !pseudopc $e4ba {
   587                          
   588                          part2:
   589                          
   590                          ;**** String Allocation (Fortsetzung)
   591                          ;
   592                          ;	in: 	TOS		; Länge
   593                          ;		fretop		; String-Adresse
   594                          ;	out:	fretop		; String-Adresse
   595                          ;		strptr		; String-Adresse (wird nicht verwendet)
   596                          ;				; (bei alternate_stralloc eventuell mit
   597                          ;				; inkrementiertem High-Byte)
   598                          ;		A		; Länge
   599                          ;		X,Y		; String-Adresse (L,H)
   600                          ;	called:	allocate (in Fortsetzung)
   601                          
   602                            !ifndef alternate_stralloc {
   603                          stralloc:
   604  0a29 8535               	sta strptr		; strptr = A/X = FRETOP
   605  0a2b 8636               	stx strptr+1
   606  0a2d aa                 	tax			; A in X aufheben
   607  0a2e 68                 	pla			; Länge temp. vom Stack 
   608  0a2f 48                 	pha			; wieder auf Stack, nun auch in A
   609  0a30 a8                 	tay			; Index=Länge (Backlink-position)
   610  0a31 9133               	sta (fretop),y		; Backlink L = String-/Lückenlänge
   611  0a33 c8                 	iny			; Y=Länge+1
   612  0a34 d002               	bne sa1			; wenn Länge=255, dann
   613  0a36 e634               	inc fretop+1		; Überlauf, aber nur temporär!
   614                          
   615  0a38 a9ff               sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   616  0a3a 9133               	sta (fretop),y
   617  0a3c a436               	ldy strptr+1
   618  0a3e 8434               	sty fretop+1		; Überlaufkorr. rückgängig
   619  0a40 68                 	pla			; Länge vom Stack nehmen
   620  0a41 60                 	rts
   621                          
   622                            } else {
   623                          ; alternative, etwas kürzere Varainte
   624                          
   625                          stralloc:
   626                          	sta strptr		; strptr = A/X = FRETOP
   627                          	stx strptr+1
   628                          	tax			; A in X aufheben
   629                          	pla			; Länge temp. vom Stack 
   630                          	pha			; wieder auf Stack, nun auch in A
   631                          	tay			; Index=Länge (Backlink-position)
   632                          	sta (strptr),y		; Backlink L = String-/Lückenlänge
   633                          	iny			; Y=Länge+1
   634                          	bne sa1			; wenn Länge=255, dann
   635                          	inc strptr+1		; Überlauf, aber nur temporär!
   636                          
   637                          sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   638                          	sta (strptr),y
   639                          	ldy fretop+1		; in Y String-Adresse High-Byte
   640                          	pla			; Länge vom Stack nehmen
   641                          	rts
   642                          				; Hier weicht strptr+1 u.U. von fretop+1 ab,
   643                          				; was aber kein Problem darstellt, da
   644                          				; es im BASIC-Interpreter keine Stellt gibt,
   645                          				; die nach einem allocate-Aufruf den
   646                          				; Pointer strptr/strptr+1 verwendet!
   647                            }
   648                          }
   649                          
   650                          part2_real_end
   651                          
   652                          
   653                          ; Einsprungspunkt an korrekter Position?
   654                          
   655                          ; Kann erst nach dem Label docollect gemacht werden!
   656                          
   657                          !if (garcoll != docollect) {
   658                          	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
   659                          }
