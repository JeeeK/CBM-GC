
; ******** Source: garbcol.asm
     1                          !to "garbcol.o",cbm	
     2                          ;
     3                          ;  **** Garbage Collection ****
     4                          ;
     5                          ; 64'er, Oct. 1988
     6                          
     7                          ;
     8                          ; Überarbeitetet und korrigiert:
     9                          ;	2013-11-15 Johann E. Klasek, johann at klasek at
    10                          ;
    11                          ; Bugfixes:
    12                          ;	1) in backlinkarr:
    13                          ;	   C-Flag ist beim Errechnen des Folge-Arrays
    14                          ;	   definiert gelöscht. 
    15                          ;          Sonst werden ev. nicht alle Elemente
    16                          ;          aller Arrays mit einem korrekten
    17                          ;          Backlink versehen und der
    18                          ;          String-Heap wird durch die GC
    19                          ;          korrumpiert!
    20                          ;	2) in backlinkarr bei blanext:
    21                          ;	   Muss zum Aufrufer immer mit Z=0
    22                          ;	   rückkehren, und erkennt
    23                          ;	   sonst immer nur das 1. Array!
    24                          ;	   Damit liegen die anderen Strings
    25                          ;	   dann im freien Bereich und
    26                          ;	   werden nach und nach überschrieben!
    27                          ;
    28                          ; Optimierungen:
    29                          ;	Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
    30                          ;	Wegen des längeren Codes und und einem deswegen länger werdenden
    31                          ;	Branch-Offset, wurde der Code ab cfinish zurückversetzt.
    32                          ;	Es sind aber in Teilbereich 1 nur noch 3 Bytes frei -> nicht verwendbar.
    33                          ;!set use_fast_copy=1
    34                          
    35                          
    36                          
    37                          ; Basic-Zeiger und -konstanten
    38                          
    39                          collected = $0f
    40                          
    41                          sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
    42                          			; wächst nach oben, max. 3 Elemente
    43                          			; zu je 3 Bytes.
    44                          sdsptr   = $16		; Zeiger auf nächstes freie Element
    45                          			; des String-Descriptor-Stacks (SDS)
    46                          
    47                          vartab   = $2d		; Basicprogrammende = Variablenanfang
    48                          arytab   = $2f		; Variablenende = Array-Bereichanfang
    49                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
    50                          fretop   = $33		; aktuelle String-Heap-Adresse
    51                          strptr	 = $35		; temporärer Stringzeiger
    52                          memsiz   = $37		; höchste RAM-Adresse für Basic, Start
    53                          			; des nach unten wachsenden String-Heaps
    54                          ; Hilfsvariablen
    55                          
    56                          ptr	 = $22		; Arbeitszeiger
    57                          newptr	 = $4e		; Neuer Stringzeiger
    58                          desclen	 = $53		; akt. Länge eines Stringdescriptors
    59                          aryptr	 = $58		; Array-Zeiger
    60                          descptr	 = $5f		; Descriptor-Zeiger
    61                          
    62                          garcoll  = $b526
    63                          
    64                          ; Vorbelegung der Speicherplätze
    65                          
    66                          romsize  = $2000	; ROM Länge 8K
    67                          
    68                          prozport = $01		; Prozessorport
    69                          memrom = %00110111	; Basic+Kernal ROM
    70                          membas = %00110110	; Basic RAM+kernal ROM
    71                          memram = %00110101	; Basic+Kernal RAM
    72                          
    73                          
    74                          ; Datenstrukturen
    75                          ;
    76                          ; String am Heap:
    77                          ;
    78                          ;   +--------------------------------------+
    79                          ;   |       +--------------+               |
    80                          ;   V       |              V               |
    81                          ;   +---+---+---+          +-----------+---+---+
    82                          ;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
    83                          ;   +---+---+---+          +-----------+---+---+
    84                          ;   ^    *******           ^            *******
    85                          ;   |       String.adr.    |               Descriptor-Adr.
    86                          ;   +-Descriptor-Adresse   +-Stringadresse
    87                          ;
    88                          ; Lücken am Heap:
    89                          ;                      
    90                          ;   +-------------+ +----------------+
    91                          ;   V             | V                |
    92                          ;    +-----------+---+---+---------+---+---+
    93                          ;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
    94                          ;    +-----------+---+---+---------+---+---+
    95                          ;                  ^  ***            ^  ***
    96                          ;                  |   Lückenmark.   |   Lückenmarkierung
    97                          ;                  Backlink-Adresse  Backlink-Adresse
    98                          
    99                          
   100                          

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
   102                          
   103                          ;
   104                          ; Patch-Liste für "loader"
   105                          ;
   106                          
   107                          patchlist:
   108                          
   109  087b 95084601f4b4       !wo part1_real,part1_real_end-part1_real,part1
   110  0881 260a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
   111  0887 f009360074e4       !wo part3_real,part3_real_end-part3_real,part3
   112  088d db091500c1b6       !wo part4_real,part4_real_end-part4_real,part4
   113  0893 0000               !wo 0  ; Endemarkierung
   114                          
   115                          
   116                          ; ******************************* part 1 *************************************
   117                          
   118                          part1_real:
   119                          
   120                          !pseudopc $b4f4 {
   121                          
   122                          part1:
   123                          
   124                          ;***** Speicher von String-Heap anfordern
   125                          ;
   126                          ;	in:	A		; Länge anforderung
   127                          ;		fretop
   128                          ;	mod:	collected	; "GC aufgerufen" Flag
   129                          ;		strptr		; temp. Zeiger
   130                          ;	out:	fretop		; Adresse auf String
   131                          ;		X,Y		; Adresse auf String
   132                          ;
   133                          ; Der String wird im Backlink stets als ungebrauchte Lücke
   134                          ; markiert! Dann muss die GC nur noch die Backlinks
   135                          ; der aktiven Strings setzen und kann die ungebrauchten
   136                          ; Strings überspringen.
   137                          
   138                          
   139                          basicerror = $b4d2		; Basic-Fehlermeldung
   140                          
   141                          allocate:
   142  0895 460f               	lsr collected		; Flag löschen
   143  0897 48                 retry	pha			; Länge der Anforderung,
   144                          				; für 2. Teil
   145                          				; Länge 0 möglich, verbraucht aber 2 Bytes
   146  0898 49ff               	eor #$ff		; negieren
   147  089a 38                 	sec
   148  089b 6533               	adc fretop		; A/X = fretop; A/X -= Länge
   149  089d a634               	ldx fretop+1
   150  089f b002               	bcs l1
   151  08a1 ca                 	dex
   152  08a2 38                 	sec
   153  08a3 e902               l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
   154  08a5 b001               	bcs l2
   155  08a7 ca                 	dex
   156  08a8 e432               l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
   157  08aa 9006               	bcc checkcollect
   158  08ac d013               	bne alloc		; nein, Bereich anfordern
   159  08ae c531               	cmp strend 
   160  08b0 b00f               	bcs alloc		; nein, Bereich anfordern
   161                          checkcollect
   162  08b2 a210               	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
   163  08b4 a50f               	lda collected
   164  08b6 30bb               	bmi basicerror		; Collection schon gelaufen?
   165  08b8 2026b5             	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
   166  08bb 660f               	ror collected		; Flag setzen (Bit 7) setzen
   167  08bd 68                 	pla			; Länge angeforderter Bereich
   168  08be 4cf6b4             	jmp retry		; nochmal versuchen (ob durch GC Platz frei wurde)
   169                          
   170  08c1 2067b5             alloc	jsr setfretop		; FRETOP = A/X
   171  08c4 4cbae4             	jmp stralloc		; zum 2. Teil: Allokation abschließen
   172                          
   173                          
   174                          ;***** garbage collection
   175                          
   176                          ;	in:	-
   177                          ;	mod:	ptr		; Zeiger auf alten String-Heap
   178                          ;		newptr		; Zeiger auf neuen String-Heap
   179                          ;		descptr		; Zeiger auf Descriptor
   180                          ;		desclen		; Descriptor-Schrittweite
   181                          ;	out:	fretop		; Neue String-Heap-Position
   182                          ;		C=1
   183                          
   184                          docollect
   185                          
   186                          
   187                          ; Backlink aller temporären Strings am String-Descriptor-Stack setzen
   188                          
   189  08c7 a919               sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   190  08c9 a200               	ldx #>sdsbase		; da in 0-Page, immer 0
   191  08cb 20a5e4             	jsr setptr		; damit ptr setzen
   192                          
   193  08ce c516               sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   194  08d0 f005               	beq vars		; Ja, SDS durch, weiter mit Variablen
   195  08d2 20e7b5             	jsr backlink		; sonst Backlink setzen
   196  08d5 f0f7               	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descriptor
   197                          
   198                          ; Backlink aller String-Variablen setzen
   199                          
   200  08d7 a905               vars:	lda #5			; Descriptor-Schritt für Variablen
   201  08d9 8553               	sta desclen
   202  08db a52d               	lda vartab		; Variablenbeginn
   203  08dd a62e               	ldx vartab+1
   204  08df 20a5e4             	jsr setptr		; ptr = A/X
   205                          
   206  08e2 e430               varnext	cpx arytab+1		; Variablenende?
   207  08e4 d004               	bne varbl
   208  08e6 c52f               	cmp arytab
   209  08e8 f005               	beq arrays		; ja, weiter mit Arrays
   210  08ea 201eb6             varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
   211  08ed d0f3               	bne varnext		; immer; nächsten Var.-Descriptor
   212                          
   213                          ; Backlink bei allen String-Arrays setzen
   214                          
   215                          arrays:
   216  08ef 8558               	sta aryptr		; Variablenbereichende = Array-Bereichanfang
   217  08f1 8659               	stx aryptr+1 
   218  08f3 a003               	ldy #3			; Descriptor-Schritt bei String-Arrays
   219  08f5 8453               	sty desclen
   220                          
   221  08f7 e432               arrnext	cpx strend+1		; Array-Bereichende?
   222  08f9 d004               	bne arrbl
   223  08fb c531               	cmp strend
   224  08fd f00e               	beq cleanwalk
   225  08ff 20c4b6             arrbl	jsr backlinkarr		; Backlinks für nächstes String-Array setzen -> Z=0!
   226  0902 d0f3               	bne arrnext		; immer; nächstes Array-Element
   227                          
   228                          
   229                          ; Ende, Zeiger zum neuen String-Heap übernehmen
   230                          
   231                          cfinish
   232  0904 a54e               	lda newptr		; Aufgeräumtzeiger ist ..
   233  0906 a64f               	ldx newptr+1
   234                          setfretop
   235  0908 8533               	sta fretop		; neues FRETOP
   236  090a 8634               	stx fretop+1 
   237  090c 60                 	rts			; fertig!
   238                          
   239                          ; Nachdem nun alle Backlinks gesetzt sind
   240                          ; den String-Heap von oben nach unten durchgehen
   241                          ; und zusammenschieben ...
   242                          
   243                          cleanwalk:
   244  090d a537               	lda memsiz		; beim Basic-Speicherende
   245  090f a638               	ldx memsiz+1
   246  0911 854e               	sta newptr		; ... beginnen
   247  0913 864f               	stx newptr+1 
   248                          
   249                          ; Aufräumschleife
   250                          
   251  0915 e434               cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   252  0917 d004               	bne cwclean		; dann Heap durch und fertig.
   253  0919 c533               	cmp fretop		; andernfalls aufräumen ...
   254  091b f0e7               	beq cfinish		; fertig, weil A/X = FRETOP
   255                          
   256                          ; nächsten String "aufräumen" ...
   257                          
   258  091d 38                 cwclean	sec			; Aufräumtzeiger auf backlink
   259  091e e902               	sbc #2
   260  0920 b001               	bcs cw1
   261  0922 ca                 	dex			; A/X -> Backlink
   262                          
   263  0923 20a5e4             cw1	jsr setptr		; A/X -> ptr (Arbeitszeiger)
   264                          
   265  0926 a000               	ldy #0
   266  0928 b122               	lda (ptr),y		; Backlink low oder Lückenlänge
   267  092a c8                 	iny			; Y=1
   268  092b aa                 	tax			; -> X
   269  092c b122               	lda (ptr),y		; Backlink high
   270  092e c9ff               	cmp #$ff		; String "nicht gebraucht" Markierung
   271  0930 900c               	bcc cwactive		; aktiver String
   272                          
   273  0932 8a                 	txa			; Lückenlänge
   274  0933 49ff               	eor #$ff		; negieren
   275  0935 6522               	adc ptr			; (ptr - Lückenlänge)
   276  0937 a623               	ldx ptr+1 
   277  0939 b0da               	bcs cwnext		; weiter ...
   278  093b ca                 	dex			; High Byte
   279                          
   280  093c d0d7               cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   281                          
   282                          ; einen aktiven String nach oben schieben
   283                          
   284                          cwactive			; immer mit Y=1 angesprungen
   285  093e 8560               	sta descptr+1		; Descriptor-Adresse
   286  0940 865f               	stx descptr 
   287                          
   288  0942 a54e               	lda newptr		; Aufgeräumtzeiger -= 2
   289  0944 e901               	sbc #1			; weil bereits C=0!
   290  0946 854e               	sta newptr		; newptr -= 2
   291  0948 b003               	bcs cw3
   292  094a c64f               	dec newptr+1
   293  094c 38                 	sec			; für SBC unten
   294                          
   295  094d a9ff               cw3	lda #$ff		; Backlink h: als Lücke markieren
   296  094f 914e               	sta (newptr),y		; Y=1
   297  0951 88                 	dey			; Y=0
   298  0952 b15f               	lda (descptr),y		; Descriptor: String-länge
   299  0954 914e               	sta (newptr),y		; Backlink l: Lückenlänge
   300                          
   301  0956 a54e               	lda newptr		; Aufgeräumtzeiger -= String-Länge
   302  0958 f15f               	sbc (descptr),y		; immer C=1
   303  095a 854e               	sta newptr
   304  095c b003               	bcs cw4
   305  095e c64f               	dec newptr+1
   306  0960 38                 	sec			; für SBC unten
   307                          
   308  0961 c8                 cw4	iny			; Y=1
   309  0962 915f               	sta (descptr),y		; String-Adresse L: neue Adresse
   310  0964 c8                 	iny			; Y=2
   311  0965 a54f               	lda newptr+1
   312  0967 915f               	sta (descptr),y		; String-Adresse H: neue Adresse
   313  0969 a000               	ldy #0
   314  096b a522               	lda ptr
   315  096d f15f               	sbc (descptr),y		; immer C=1
   316  096f 8522               	sta ptr			; Arbeitszeiger = alte String-Adresse
   317  0971 b002               	bcs cw5
   318  0973 c623               	dec ptr+1
   319                          cw5
   320  0975 b15f               	lda (descptr),y		; String-Länge=0?
   321  0977 f009               	beq cwnocopy		; ja, dann nicht kopieren
   322  0979 a8                 	tay			; Länge-1
   323                          
   324                          !ifndef use_fast_copy {
   325                          
   326  097a 88                 cwloop	dey			; -> Startindex fürs Kopieren
   327  097b b122               	lda (ptr),y		; Arbeitszeiger mit altem String
   328  097d 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   329  097f 98                 	tya			; Test auf Z-Flag!
   330  0980 d0f8               	bne cwloop		; Index = 0 -> fertig kopiert
   331                          
   332                          } else {
   333                          
   334                          				; + 5 Byte, -2 T/Zeichen 
   335                          	beq cwone		; nur 1 Byte
   336                          cwloop				; -> Startindex fürs Kopieren
   337                          	lda (ptr),y		; Arbeitszeiger mit altem String
   338                          	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   339                          	dey			; Test auf Z-Flag!
   340                          	bne cwloop		; Index = 0 -> fertig kopiert
   341                          cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   342                          	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   343                          
   344                          }
   345                          
   346                          cwnocopy
   347  0982 a522               	lda ptr
   348  0984 a623               	ldx ptr+1		; High-Byte immer !=0
   349  0986 d08d               	bne cwnext		; immer; weiter in Schleife
   350                          
   351                          
   352                          ;**** Backlink setzen
   353                          ;
   354                          ; 	in:		ptr	Descriptor-Adresse
   355                          ; 	out:		ptr	Descriptor-Adresse
   356                          ;			A/X
   357                          ;			Z=0	wenn nicht am SDS
   358                          ;			Z=1	wenn am SDS
   359                          ;	destroy:	newptr
   360                          ;	called:		blaset, backlinkvar
   361                          
   362                          backlink:
   363  0988 a000               	ldy #0
   364  098a b122               	lda (ptr),y		; String-Länge
   365  098c f023               	beq blnext		; fertig, wenn =0
   366  098e c8                 	iny
   367  098f 18                 	clc
   368  0990 7122               	adc (ptr),y		; Backlink-Position (am String-Ende)
   369  0992 854e               	sta newptr		; Backlink-Zeiger L
   370  0994 aa                 	tax
   371  0995 c8                 	iny
   372  0996 b122               	lda (ptr),y
   373  0998 6900               	adc #0
   374  099a 854f               	sta newptr+1		; Backlink-Zeiger H
   375  099c c532               	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
   376  099e 9011               	bcc blnext		; ja, denn nächsten String
   377  09a0 d004               	bne blsetdesc
   378  09a2 e431               	cpx strend 
   379  09a4 900b               	bcc blnext		; < Array-Bereichende (außerhalb Heap)?
   380                          
   381                          blsetdesc
   382  09a6 a001               	ldy #1
   383  09a8 a523               	lda ptr+1
   384  09aa 914e               	sta (newptr),y		; Descriptor-Adresse ...
   385  09ac 88                 	dey
   386  09ad a522               	lda ptr
   387  09af 914e               	sta (newptr),y		; in den Backlink übertragen
   388                          
   389  09b1 18                 blnext	clc			; nächster String/nächste Variable
   390  09b2 a553               	lda desclen		; Schrittweite zum nächsten
   391  09b4 6522               	adc ptr			; Descriptor ...
   392  09b6 8522               	sta ptr
   393  09b8 9002               	bcc bl1
   394  09ba e623               	inc ptr+1
   395  09bc a623               bl1	ldx ptr+1		; immer != 0 -> Z=0
   396  09be 60                 	rts
   397                          
   398                          ;**** Nächste String-Variable und Backlink setzen
   399                          ;
   400                          ; 	in:		ptr	Variablenadresse
   401                          ; 	out:		ptr	Variablenadresse
   402                          ;			A/X
   403                          ;			Z=0
   404                          ;	destroy:	newptr
   405                          ;	called:		varbl (vars)
   406                          
   407                          backlinkvar:
   408  09bf a000               	ldy #0
   409  09c1 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   410  09c3 aa                 	tax			; Typstatus merken
   411  09c4 c8                 	iny
   412  09c5 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   413  09c7 a8                 	tay			; Typstatus merken
   414                          
   415  09c8 18                 	clc
   416  09c9 a522               	lda ptr			; Descriptor-Adresse (in Variable)
   417  09cb 6902               	adc #$02		; erreichnen
   418  09cd 8522               	sta ptr
   419  09cf 9002               	bcc blv1
   420  09d1 e623               	inc ptr+1
   421                          
   422  09d3 8a                 blv1	txa			; Variablentyp prüfen
   423  09d4 30db               	bmi blnext		; keine String, nächste Variable
   424  09d6 98                 	tya
   425  09d7 30af               	bmi backlink		; Backlink setzen
   426  09d9 10d6               	bpl blnext		; keine String-Var., nächste Variable
   427                          
   428                          }
   429                          part1_real_end
   430                          
   431                          ; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!
   432                          
   433                          !set part1_end = (part1_real_end-part1_real)+part1
   434                          !if ( part1_end > $B63D ) {
   435                          	!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   436                          }
   437                          
   438                          
   439                          ; ******************************* part 4 *************************************
   440                          
   441                          part4_real
   442                          !pseudopc $b6c1 {
   443                          
   444                          part4:
   445                          
   446                          part4_continue = $b6d6
   447  09db 4cd6b6             	jmp part4_continue
   448                          
   449                          ;**** Nächste Array-Variable und Backlink setzen
   450                          ;
   451                          ; 	in: 		ptr	Arrayadresse
   452                          ; 	out:		ptr	Adresse Folge-array
   453                          ;			aryptr	Adresse Folge-array
   454                          ;			A/X	Adresse Folge-array
   455                          ;			Z=0
   456                          ;	destroy:	newptr
   457                          ;	called:		arrbl (arrays)
   458                          
   459                          backlinkarr:
   460  09de a000               	ldy #0
   461  09e0 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   462  09e2 08                 	php			; für später
   463  09e3 c8                 	iny
   464  09e4 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   465  09e6 aa                 	tax			; für später
   466                          
   467  09e7 c8                 	iny
   468  09e8 b122               	lda (ptr),y		; Offset nächstes Array
   469  09ea 18                 	clc			; Bugfix 1: C=0 definiert setzen
   470  09eb 6558               	adc aryptr
   471  09ed 4c75e4             	jmp backlinkarr2
   472                          				; weiter an anderer Stelle!
   473                          blapast
   474                          !if blapast > part4_continue {
   475                          	!error "part4 ist zu lang!"
   476                          }
   477                          }
   478                          part4_real_end
   479                          
   480                          
   481                          ; ******************************* part 3 *************************************
   482                          
   483                          part3_real
   484                          !pseudopc $e474 {
   485                          
   486                          part3:
   487                          
   488  09f0 00                 	!byte  0 		; Einschaltmeldung kürzen
   489                          
   490                          backlinkarr2:
   491  09f1 8558               	sta aryptr		; Folge-Array L
   492  09f3 c8                 	iny
   493  09f4 b122               	lda (ptr),y
   494  09f6 6559               	adc aryptr+1 
   495  09f8 8559               	sta aryptr+1		; Folge-Array H
   496                          
   497  09fa 28                 	plp			; Arraytyp:
   498  09fb 3020               	bmi blaskip		; kein String-Array
   499  09fd 8a                 	txa
   500  09fe 101d               	bpl blaskip		; kein String-Array
   501                          
   502  0a00 c8                 	iny			; Y=4
   503  0a01 b122               	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
   504  0a03 0a                 	asl 			; *2
   505  0a04 6905               	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
   506  0a06 6522               	adc ptr			; auf 1. Element ...
   507  0a08 8522               	sta ptr 
   508  0a0a 9002               	bcc bla1
   509  0a0c e623               	inc ptr+1 
   510  0a0e a623               bla1	ldx ptr+1		; positionieren
   511                          
   512  0a10 e459               blanext	cpx aryptr+1		; Array-Ende erreicht?
   513  0a12 d004               	bne blaset		; nein, Backlink setzen
   514  0a14 c558               	cmp aryptr
   515  0a16 f007               	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
   516                          blaset
   517  0a18 20e7b5             	jsr backlink		; Backlink setzen
   518  0a1b d0f3               	bne blanext		; immer (High-Byte != 0)
   519                          
   520                          blaskip
   521  0a1d a558               	lda aryptr		; Zeiger auf Folge-Array
   522                          blafinish
   523  0a1f a659               	ldx aryptr+1 		; Z=0 sicherstellen
   524                          
   525  0a21 8522               setptr	sta ptr			; Arbeitszeiger setzen
   526  0a23 8623               	stx ptr+1
   527  0a25 60                 	rts			; immer Z=0
   528                          
   529                          ;--- $e4b7 - $e4d2 unused ($aa)
   530                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   531                          ;----              sonst Patch für andere Zwecke
   532                          
   533                          }
   534                          part3_real_end
   535                          
   536                          
   537                          ; ******************************* part 2 *************************************
   538                          
   539                          part2_real
   540                          !pseudopc $e4ba {
   541                          
   542                          part2:
   543                          
   544                          ;**** String Allocation (Fortsetzung)
   545                          ;
   546                          ;	in: 	TOS		; Länge
   547                          ;		fretop		; String-Adresse
   548                          ;	out:	fretop		; String-Adresse
   549                          ;		strptr		; String-Adresse (wird nicht verwendet)
   550                          ;		A		; Länge
   551                          ;		X,Y		; String-Adresse
   552                          ;	called:	allocate
   553                          
   554                          stralloc:
   555  0a26 8535               	sta strptr		; strptr = A/X = FRETOP
   556  0a28 8636               	stx strptr+1
   557  0a2a aa                 	tax			; A in X aufheben
   558  0a2b 68                 	pla			; Länge temp. vom Stack 
   559  0a2c 48                 	pha			; wieder auf Stack, nun auch in A
   560  0a2d a8                 	tay			; Index=Länge (Backlink-position)
   561  0a2e 9133               	sta (fretop),y		; Backlink L = String-/Lückenlänge
   562  0a30 c8                 	iny			; Y=Länge+1
   563  0a31 d002               	bne sa1			; wenn Länge=255, dann
   564  0a33 e634               	inc fretop+1		; Überlauf, aber nur temporär!
   565                          
   566  0a35 a9ff               sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   567  0a37 9133               	sta (fretop),y
   568  0a39 a436               	ldy strptr+1
   569  0a3b 8434               	sty fretop+1		; Überlaufkorr. rückgängig
   570  0a3d 68                 	pla			; Länge vom Stack nehmen
   571  0a3e 60                 	rts
   572                          
   573                          }
   574                          part2_real_end
   575                          
   576                          
   577                          ; Einsprungspunkt an korrekter Position?
   578                          
   579                          ; Kann erst nach dem Label docollect gemacht werden!
   580                          
   581                          !if (garcoll != docollect) {
   582                          	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
   583                          }
