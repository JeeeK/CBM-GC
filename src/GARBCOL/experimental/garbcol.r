
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
    32                          ;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
    33                          ;	noch an anderer Stelle eingespart werden und zwar durch
    34                          ; 	Faktorisieren in die Routine incptr. Dies ergibt exakt
    35                          ;	die Ersparnis von 2 Bytes. Die Optimierung wirkt sich
    36                          ;	allerdings immer nur dann positiv aus, wenn ein String
    37                          ;	mehr als 12 Zeichen aufweist, sonst aber bei einer Länge
    38                          ;	weniger als 12 Zeichen *negativ* aus, weil der Aufruf 
    39                          ;	des faktorisieren Codes per JSR/RTS immer 24 Takte kostet!
    40                          ;	Es hängt also vom konkreten Anwendungsfall ab, ob
    41                          ;	sie ein besseres oder schlechteres Laufzeitverhalten ergibt.
    42                          ;	Hier ist wohl jene Variante ohne dieser speziellen "Optimierung"
    43                          ;	im Allgemeinen die bessere Wahl.
    44                          
    45                          !set use_fast_copy=1
    46                          
    47                          
    48                          
    49                          ; Basic-Zeiger und -konstanten
    50                          
    51                          collected = $0f
    52                          
    53                          sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
    54                          			; wächst nach oben, max. 3 Elemente
    55                          			; zu je 3 Bytes.
    56                          sdsptr   = $16		; Zeiger auf nächstes freie Element
    57                          			; des String-Descriptor-Stacks (SDS)
    58                          
    59                          vartab   = $2d		; Basicprogrammende = Variablenanfang
    60                          arytab   = $2f		; Variablenende = Array-Bereichanfang
    61                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
    62                          fretop   = $33		; aktuelle String-Heap-Adresse
    63                          strptr	 = $35		; temporärer Stringzeiger
    64                          memsiz   = $37		; höchste RAM-Adresse für Basic, Start
    65                          			; des nach unten wachsenden String-Heaps
    66                          ; Hilfsvariablen
    67                          
    68                          ptr	 = $22		; Arbeitszeiger
    69                          newptr	 = $4e		; Neuer Stringzeiger
    70                          desclen	 = $53		; akt. Länge eines Stringdescriptors
    71                          aryptr	 = $58		; Array-Zeiger
    72                          descptr	 = $5f		; Descriptor-Zeiger
    73                          
    74                          garcoll  = $b526
    75                          
    76                          ; Vorbelegung der Speicherplätze
    77                          
    78                          romsize  = $2000	; ROM Länge 8K
    79                          
    80                          prozport = $01		; Prozessorport
    81                          memrom = %00110111	; Basic+Kernal ROM
    82                          membas = %00110110	; Basic RAM+kernal ROM
    83                          memram = %00110101	; Basic+Kernal RAM
    84                          
    85                          
    86                          ; Datenstrukturen
    87                          ;
    88                          ; String am Heap:
    89                          ;
    90                          ;   +--------------------------------------+
    91                          ;   |       +--------------+               |
    92                          ;   V       |              V               |
    93                          ;   +---+---+---+          +-----------+---+---+
    94                          ;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
    95                          ;   +---+---+---+          +-----------+---+---+
    96                          ;   ^    *******           ^            *******
    97                          ;   |       String.adr.    |               Descriptor-Adr.
    98                          ;   +-Descriptor-Adresse   +-Stringadresse
    99                          ;
   100                          ; Lücken am Heap:
   101                          ;                      
   102                          ;   +-------------+ +----------------+
   103                          ;   V             | V                |
   104                          ;    +-----------+---+---+---------+---+---+
   105                          ;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
   106                          ;    +-----------+---+---+---------+---+---+
   107                          ;                  ^  ***            ^  ***
   108                          ;                  |   Lückenmark.   |   Lückenmarkierung
   109                          ;                  Backlink-Adresse  Backlink-Adresse
   110                          
   111                          
   112                          

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
   114                          
   115                          ;
   116                          ; Patch-Liste für "loader"
   117                          ;
   118                          
   119                          patchlist:
   120                          
   121  087b 95084901f4b4       !wo part1_real,part1_real_end-part1_real,part1
   122  0881 290a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
   123  0887 f309360074e4       !wo part3_real,part3_real_end-part3_real,part3
   124  088d de091500c1b6       !wo part4_real,part4_real_end-part4_real,part4
   125  0893 0000               !wo 0  ; Endemarkierung
   126                          
   127                          
   128                          ; ******************************* part 1 *************************************
   129                          
   130                          part1_real:
   131                          
   132                          !pseudopc $b4f4 {
   133                          
   134                          part1:
   135                          
   136                          ;***** Speicher von String-Heap anfordern
   137                          ;
   138                          ;	in:	A		; Länge anforderung
   139                          ;		fretop
   140                          ;	mod:	collected	; "GC aufgerufen" Flag
   141                          ;		strptr		; temp. Zeiger
   142                          ;	out:	fretop		; Adresse auf String
   143                          ;		X,Y		; Adresse auf String
   144                          ;
   145                          ; Der String wird im Backlink stets als ungebrauchte Lücke
   146                          ; markiert! Dann muss die GC nur noch die Backlinks
   147                          ; der aktiven Strings setzen und kann die ungebrauchten
   148                          ; Strings überspringen.
   149                          
   150                          
   151                          basicerror = $b4d2		; Basic-Fehlermeldung
   152                          
   153                          allocate:
   154  0895 460f               	lsr collected		; Flag löschen
   155  0897 48                 retry	pha			; Länge der Anforderung,
   156                          				; für 2. Teil
   157                          				; Länge 0 möglich, verbraucht aber 2 Bytes
   158  0898 49ff               	eor #$ff		; negieren
   159  089a 38                 	sec
   160  089b 6533               	adc fretop		; A/X = fretop; A/X -= Länge
   161  089d a634               	ldx fretop+1
   162  089f b002               	bcs l1
   163  08a1 ca                 	dex
   164  08a2 38                 	sec
   165  08a3 e902               l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
   166  08a5 b001               	bcs l2
   167  08a7 ca                 	dex
   168  08a8 e432               l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
   169  08aa 9006               	bcc checkcollect
   170  08ac d013               	bne alloc		; nein, Bereich anfordern
   171  08ae c531               	cmp strend 
   172  08b0 b00f               	bcs alloc		; nein, Bereich anfordern
   173                          checkcollect
   174  08b2 a210               	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
   175  08b4 a50f               	lda collected
   176  08b6 30bb               	bmi basicerror		; Collection schon gelaufen?
   177  08b8 2026b5             	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
   178  08bb 660f               	ror collected		; Flag setzen (Bit 7) setzen
   179  08bd 68                 	pla			; Länge angeforderter Bereich
   180  08be 4cf6b4             	jmp retry		; nochmal versuchen (ob durch GC Platz frei wurde)
   181                          
   182  08c1 2067b5             alloc	jsr setfretop		; FRETOP = A/X
   183  08c4 4cbae4             	jmp stralloc		; zum 2. Teil: Allokation abschließen
   184                          
   185                          
   186                          ;***** garbage collection
   187                          
   188                          ;	in:	-
   189                          ;	mod:	ptr		; Zeiger auf alten String-Heap
   190                          ;		newptr		; Zeiger auf neuen String-Heap
   191                          ;		descptr		; Zeiger auf Descriptor
   192                          ;		desclen		; Descriptor-Schrittweite
   193                          ;	out:	fretop		; Neue String-Heap-Position
   194                          ;		C=1
   195                          
   196                          docollect
   197                          
   198                          
   199                          ; Backlink aller temporären Strings am String-Descriptor-Stack setzen
   200                          
   201  08c7 a919               sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   202  08c9 a200               	ldx #>sdsbase		; da in 0-Page, immer 0
   203  08cb 20a5e4             	jsr setptr		; damit ptr setzen
   204                          
   205  08ce c516               sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   206  08d0 f005               	beq vars		; Ja, SDS durch, weiter mit Variablen
   207  08d2 20ecb5             	jsr backlink		; sonst Backlink setzen
   208  08d5 f0f7               	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descriptor
   209                          
   210                          ; Backlink aller String-Variablen setzen
   211                          
   212  08d7 a905               vars:	lda #5			; Descriptor-Schritt für Variablen
   213  08d9 8553               	sta desclen
   214  08db a52d               	lda vartab		; Variablenbeginn
   215  08dd a62e               	ldx vartab+1
   216  08df 20a5e4             	jsr setptr		; ptr = A/X
   217                          
   218  08e2 e430               varnext	cpx arytab+1		; Variablenende?
   219  08e4 d004               	bne varbl
   220  08e6 c52f               	cmp arytab
   221  08e8 f005               	beq arrays		; ja, weiter mit Arrays
   222  08ea 201db6             varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
   223  08ed d0f3               	bne varnext		; immer; nächsten Var.-Descriptor
   224                          
   225                          ; Backlink bei allen String-Arrays setzen
   226                          
   227                          arrays:
   228  08ef 8558               	sta aryptr		; Variablenbereichende = Array-Bereichanfang
   229  08f1 8659               	stx aryptr+1 
   230  08f3 a003               	ldy #3			; Descriptor-Schritt bei String-Arrays
   231  08f5 8453               	sty desclen
   232                          
   233  08f7 e432               arrnext	cpx strend+1		; Array-Bereichende?
   234  08f9 d004               	bne arrbl
   235  08fb c531               	cmp strend
   236  08fd f00e               	beq cleanwalk
   237  08ff 20c4b6             arrbl	jsr backlinkarr		; Backlinks für nächstes String-Array setzen -> Z=0!
   238  0902 d0f3               	bne arrnext		; immer; nächstes Array-Element
   239                          
   240                          
   241                          ; Ende, Zeiger zum neuen String-Heap übernehmen
   242                          
   243                          cfinish
   244  0904 a54e               	lda newptr		; Aufgeräumtzeiger ist ..
   245  0906 a64f               	ldx newptr+1
   246                          setfretop
   247  0908 8533               	sta fretop		; neues FRETOP
   248  090a 8634               	stx fretop+1 
   249  090c 60                 	rts			; fertig!
   250                          
   251                          ; Nachdem nun alle Backlinks gesetzt sind
   252                          ; den String-Heap von oben nach unten durchgehen
   253                          ; und zusammenschieben ...
   254                          
   255                          cleanwalk:
   256  090d a537               	lda memsiz		; beim Basic-Speicherende
   257  090f a638               	ldx memsiz+1
   258  0911 854e               	sta newptr		; ... beginnen
   259  0913 864f               	stx newptr+1 
   260                          
   261                          ; Aufräumschleife
   262                          
   263  0915 e434               cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   264  0917 d004               	bne cwclean		; dann Heap durch und fertig.
   265  0919 c533               	cmp fretop		; andernfalls aufräumen ...
   266  091b f0e7               	beq cfinish		; fertig, weil A/X = FRETOP
   267                          
   268                          ; nächsten String "aufräumen" ...
   269                          
   270  091d 38                 cwclean	sec			; Aufräumtzeiger auf backlink
   271  091e e902               	sbc #2
   272  0920 b001               	bcs cw1
   273  0922 ca                 	dex			; A/X -> Backlink
   274                          
   275  0923 20a5e4             cw1	jsr setptr		; A/X -> ptr (Arbeitszeiger)
   276                          
   277  0926 a000               	ldy #0
   278  0928 b122               	lda (ptr),y		; Backlink low oder Lückenlänge
   279  092a c8                 	iny			; Y=1
   280  092b aa                 	tax			; -> X
   281  092c b122               	lda (ptr),y		; Backlink high
   282  092e c9ff               	cmp #$ff		; String "nicht gebraucht" Markierung
   283  0930 900c               	bcc cwactive		; aktiver String
   284                          
   285  0932 8a                 	txa			; Lückenlänge
   286  0933 49ff               	eor #$ff		; negieren
   287  0935 6522               	adc ptr			; (ptr - Lückenlänge)
   288  0937 a623               	ldx ptr+1 
   289  0939 b0da               	bcs cwnext		; weiter ...
   290  093b ca                 	dex			; High Byte
   291                          
   292  093c d0d7               cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   293                          
   294                          ; einen aktiven String nach oben schieben
   295                          
   296                          cwactive			; immer mit Y=1 angesprungen
   297  093e 8560               	sta descptr+1		; Descriptor-Adresse
   298  0940 865f               	stx descptr 
   299                          
   300  0942 a54e               	lda newptr		; Aufgeräumtzeiger -= 2
   301  0944 e901               	sbc #1			; weil bereits C=0!
   302  0946 854e               	sta newptr		; newptr -= 2
   303  0948 b003               	bcs cw3
   304  094a c64f               	dec newptr+1
   305  094c 38                 	sec			; für SBC unten
   306                          
   307  094d a9ff               cw3	lda #$ff		; Backlink h: als Lücke markieren
   308  094f 914e               	sta (newptr),y		; Y=1
   309  0951 88                 	dey			; Y=0
   310  0952 b15f               	lda (descptr),y		; Descriptor: String-länge
   311  0954 914e               	sta (newptr),y		; Backlink l: Lückenlänge
   312                          
   313  0956 a54e               	lda newptr		; Aufgeräumtzeiger -= String-Länge
   314  0958 f15f               	sbc (descptr),y		; immer C=1
   315  095a 854e               	sta newptr
   316  095c b003               	bcs cw4
   317  095e c64f               	dec newptr+1
   318  0960 38                 	sec			; für SBC unten
   319                          
   320  0961 c8                 cw4	iny			; Y=1
   321  0962 915f               	sta (descptr),y		; String-Adresse L: neue Adresse
   322  0964 c8                 	iny			; Y=2
   323  0965 a54f               	lda newptr+1
   324  0967 915f               	sta (descptr),y		; String-Adresse H: neue Adresse
   325  0969 a000               	ldy #0
   326  096b a522               	lda ptr
   327  096d f15f               	sbc (descptr),y		; immer C=1
   328  096f 8522               	sta ptr			; Arbeitszeiger = alte String-Adresse
   329  0971 b002               	bcs cw5
   330  0973 c623               	dec ptr+1
   331                          cw5
   332  0975 b15f               	lda (descptr),y		; String-Länge=0?
   333  0977 f00e               	beq cwnocopy		; ja, dann nicht kopieren
   334  0979 a8                 	tay			; Länge
   335                          
   336                          !ifndef use_fast_copy {
   337                          
   338                          cwloop	dey			; -> Startindex fürs Kopieren
   339                          	lda (ptr),y		; Arbeitszeiger mit altem String
   340                          	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   341                          	tya			; Test auf Z-Flag!
   342                          	bne cwloop		; Index = 0 -> fertig kopiert
   343                          
   344                          } else {
   345                          
   346                          				; + 5 Byte, -2 T/Zeichen 
   347  097a d004               	bne cwentry		; immer, da Länge in Y>0
   348                          cwloop				; -> Startindex fürs Kopieren
   349  097c b122               	lda (ptr),y		; Arbeitszeiger mit altem String
   350  097e 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   351  0980 88                 cwentry	dey			; Test auf Z-Flag!
   352  0981 d0f9               	bne cwloop		; Index = 0 -> fertig kopiert
   353  0983 b122               cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   354  0985 914e               	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   355                          
   356                          }
   357                          
   358                          cwnocopy
   359  0987 a522               	lda ptr
   360  0989 a623               	ldx ptr+1		; High-Byte immer !=0
   361  098b d088               	bne cwnext		; immer; weiter in Schleife
   362                          
   363                          
   364                          ;**** Backlink setzen
   365                          ;
   366                          ; 	in:		ptr	Descriptor-Adresse
   367                          ; 	out:		ptr	Descriptor-Adresse
   368                          ;			A/X
   369                          ;			Z=0	wenn nicht am SDS
   370                          ;			Z=1	wenn am SDS
   371                          ;	destroy:	newptr
   372                          ;	called:		blaset, backlinkvar
   373                          
   374                          backlink:
   375  098d a000               	ldy #0
   376  098f b122               	lda (ptr),y		; String-Länge
   377  0991 f023               	beq blnext		; fertig, wenn =0
   378  0993 c8                 	iny
   379  0994 18                 	clc
   380  0995 7122               	adc (ptr),y		; Backlink-Position (am String-Ende)
   381  0997 854e               	sta newptr		; Backlink-Zeiger L
   382  0999 aa                 	tax
   383  099a c8                 	iny
   384  099b b122               	lda (ptr),y
   385  099d 6900               	adc #0
   386  099f 854f               	sta newptr+1		; Backlink-Zeiger H
   387  09a1 c532               	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
   388  09a3 9011               	bcc blnext		; ja, denn nächsten String
   389  09a5 d004               	bne blsetdesc
   390  09a7 e431               	cpx strend 
   391  09a9 900b               	bcc blnext		; < Array-Bereichende (außerhalb Heap)?
   392                          
   393                          blsetdesc
   394  09ab a001               	ldy #1
   395  09ad a523               	lda ptr+1
   396  09af 914e               	sta (newptr),y		; Descriptor-Adresse ...
   397  09b1 88                 	dey
   398  09b2 a522               	lda ptr
   399  09b4 914e               	sta (newptr),y		; in den Backlink übertragen
   400                          
   401  09b6 a553               blnext	lda desclen		; nächster String/nächste Variable
   402  09b8 2033b6             	jsr incptr		; Schrittweite zum nächsten Descriptor
   403  09bb a623               	ldx ptr+1		; immer != 0 -> Z=0 (außer bei SDS, Z=1)
   404  09bd 60                 	rts
   405                          
   406                          ;**** Nächste String-Variable und Backlink setzen
   407                          ;
   408                          ; 	in:		ptr	Variablenadresse
   409                          ; 	out:		ptr	Variablenadresse
   410                          ;			A/X
   411                          ;			Z=0
   412                          ;	destroy:	newptr
   413                          ;	called:		varbl (vars)
   414                          
   415                          backlinkvar:
   416  09be a000               	ldy #0
   417  09c0 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   418  09c2 aa                 	tax			; Typstatus merken
   419  09c3 c8                 	iny
   420  09c4 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   421  09c6 a8                 	tay			; Typstatus merken
   422                          
   423  09c7 a902               	lda #2			; Descriptor-Adresse (in Variable)
   424  09c9 2033b6             	jsr incptr		; errechnen
   425                          
   426  09cc 8a                 	txa			; Variablentyp prüfen
   427  09cd 30e7               	bmi blnext		; keine String, nächste Variable
   428  09cf 98                 	tya
   429  09d0 30bb               	bmi backlink		; Backlink setzen
   430  09d2 10e2               	bpl blnext		; keine String-Var., nächste Variable
   431                          
   432                          ; **** ptr inkrementieren
   433                          
   434                          incptr
   435  09d4 18                 	clc
   436  09d5 6522               	adc ptr			; ptr += A
   437  09d7 8522               	sta ptr
   438  09d9 9002               	bcc +
   439  09db e623               	inc ptr+1
   440  09dd 60                 +	rts
   441                          
   442                          }
   443                          part1_real_end
   444                          
   445                          ; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!
   446                          
   447                          !set part1_end = (part1_real_end-part1_real)+part1
   448                          !if ( part1_end > $B63D ) {
   449                          	!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   450                          }
   451                          
   452                          
   453                          ; ******************************* part 4 *************************************
   454                          
   455                          part4_real
   456                          !pseudopc $b6c1 {
   457                          
   458                          part4:
   459                          
   460                          part4_continue = $b6d6
   461  09de 4cd6b6             	jmp part4_continue
   462                          
   463                          ;**** Nächste Array-Variable und Backlink setzen
   464                          ;
   465                          ; 	in: 		ptr	Arrayadresse
   466                          ; 	out:		ptr	Adresse Folge-array
   467                          ;			aryptr	Adresse Folge-array
   468                          ;			A/X	Adresse Folge-array
   469                          ;			Z=0
   470                          ;	destroy:	newptr
   471                          ;	called:		arrbl (arrays)
   472                          
   473                          backlinkarr:
   474  09e1 a000               	ldy #0
   475  09e3 b122               	lda (ptr),y		; Variablenname 1. Zeichen
   476  09e5 08                 	php			; für später
   477  09e6 c8                 	iny
   478  09e7 b122               	lda (ptr),y		; Variablenname 2. Zeichen
   479  09e9 aa                 	tax			; für später
   480                          
   481  09ea c8                 	iny
   482  09eb b122               	lda (ptr),y		; Offset nächstes Array
   483  09ed 18                 	clc			; Bugfix 1: C=0 definiert setzen
   484  09ee 6558               	adc aryptr
   485  09f0 4c75e4             	jmp backlinkarr2
   486                          				; weiter an anderer Stelle!
   487                          blapast
   488                          !if blapast > part4_continue {
   489                          	!error "part4 ist zu lang!"
   490                          }
   491                          }
   492                          part4_real_end
   493                          
   494                          
   495                          ; ******************************* part 3 *************************************
   496                          
   497                          part3_real
   498                          !pseudopc $e474 {
   499                          
   500                          part3:
   501                          
   502  09f3 00                 	!byte  0 		; Einschaltmeldung kürzen
   503                          
   504                          backlinkarr2:
   505  09f4 8558               	sta aryptr		; Folge-Array L
   506  09f6 c8                 	iny
   507  09f7 b122               	lda (ptr),y
   508  09f9 6559               	adc aryptr+1 
   509  09fb 8559               	sta aryptr+1		; Folge-Array H
   510                          
   511  09fd 28                 	plp			; Arraytyp:
   512  09fe 3020               	bmi blaskip		; kein String-Array
   513  0a00 8a                 	txa
   514  0a01 101d               	bpl blaskip		; kein String-Array
   515                          
   516  0a03 c8                 	iny			; Y=4
   517  0a04 b122               	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
   518  0a06 0a                 	asl 			; *2
   519  0a07 6905               	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
   520  0a09 6522               	adc ptr			; auf 1. Element ...
   521  0a0b 8522               	sta ptr 
   522  0a0d 9002               	bcc bla1
   523  0a0f e623               	inc ptr+1 
   524  0a11 a623               bla1	ldx ptr+1		; positionieren
   525                          
   526  0a13 e459               blanext	cpx aryptr+1		; Array-Ende erreicht?
   527  0a15 d004               	bne blaset		; nein, Backlink setzen
   528  0a17 c558               	cmp aryptr
   529  0a19 f007               	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
   530                          blaset
   531  0a1b 20ecb5             	jsr backlink		; Backlink setzen
   532  0a1e d0f3               	bne blanext		; immer (High-Byte != 0)
   533                          
   534                          blaskip
   535  0a20 a558               	lda aryptr		; Zeiger auf Folge-Array
   536                          blafinish
   537  0a22 a659               	ldx aryptr+1 		; Z=0 sicherstellen
   538                          
   539  0a24 8522               setptr	sta ptr			; Arbeitszeiger setzen
   540  0a26 8623               	stx ptr+1
   541  0a28 60                 	rts			; immer Z=0
   542                          
   543                          ;--- $e4b7 - $e4d2 unused ($aa)
   544                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   545                          ;----              sonst Patch für andere Zwecke
   546                          
   547                          }
   548                          part3_real_end
   549                          
   550                          
   551                          ; ******************************* part 2 *************************************
   552                          
   553                          part2_real
   554                          !pseudopc $e4ba {
   555                          
   556                          part2:
   557                          
   558                          ;**** String Allocation (Fortsetzung)
   559                          ;
   560                          ;	in: 	TOS		; Länge
   561                          ;		fretop		; String-Adresse
   562                          ;	out:	fretop		; String-Adresse
   563                          ;		strptr		; String-Adresse (wird nicht verwendet)
   564                          ;		A		; Länge
   565                          ;		X,Y		; String-Adresse
   566                          ;	called:	allocate
   567                          
   568                          stralloc:
   569  0a29 8535               	sta strptr		; strptr = A/X = FRETOP
   570  0a2b 8636               	stx strptr+1
   571  0a2d aa                 	tax			; A in X aufheben
   572  0a2e 68                 	pla			; Länge temp. vom Stack 
   573  0a2f 48                 	pha			; wieder auf Stack, nun auch in A
   574  0a30 a8                 	tay			; Index=Länge (Backlink-position)
   575  0a31 9133               	sta (fretop),y		; Backlink L = String-/Lückenlänge
   576  0a33 c8                 	iny			; Y=Länge+1
   577  0a34 d002               	bne sa1			; wenn Länge=255, dann
   578  0a36 e634               	inc fretop+1		; Überlauf, aber nur temporär!
   579                          
   580  0a38 a9ff               sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   581  0a3a 9133               	sta (fretop),y
   582  0a3c a436               	ldy strptr+1
   583  0a3e 8434               	sty fretop+1		; Überlaufkorr. rückgängig
   584  0a40 68                 	pla			; Länge vom Stack nehmen
   585  0a41 60                 	rts
   586                          
   587                          }
   588                          part2_real_end
   589                          
   590                          
   591                          ; Einsprungspunkt an korrekter Position?
   592                          
   593                          ; Kann erst nach dem Label docollect gemacht werden!
   594                          
   595                          !if (garcoll != docollect) {
   596                          	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
   597                          }
