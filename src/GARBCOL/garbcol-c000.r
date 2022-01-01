
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
    33                          ; In diesem Fall kann allerdings die Optimierung #2 nicht
    34                          ; verwendet werden, da sonst die Backlink-Markierung
    35                          ; nicht in allen Fällen passieren würde!
    36                          ;
    37                          ; Überarbeitetet und korrigiert:
    38                          ;	2013-11-15 Johann E. Klasek, johann at klasek at
    39                          ; Optimiert:
    40                          ;	2019-03-20 Johann E. Klasek, johann at klasek at
    41                          ; Überarbeitet:
    42                          ;	2021-04-11 Johann E. Klasek, johann at klasek at
    43                          ;
    44                          ; Bugfixes:
    45                          ;
    46                          ;	1) in backlinkarr:
    47                          ;	   das C-Flag ist beim Errechnen des Folge-Arrays
    48                          ;	   definiert zu löschen, sonst werden ev. nicht 
    49                          ;	   alle Elemente aller Arrays mit einem korrekten
    50                          ;	   Backlink versehen und der String-Heap wird 
    51                          ;	   durch die GC korrumpiert!
    52                          ;
    53                          ;	2) in backlinkarr bei blanext:
    54                          ;	   Muss zum Aufrufer immer mit Z=0 rückkehren,
    55                          ;	   und erkennt sonst immer nur das 1. Array!
    56                          ;	   Damit liegen die anderen Strings dann im 
    57                          ;	   freien Bereich und werden nach und nach 
    58                          ;	   überschrieben!
    59                          ;
    60                          ; Optimierungen:
    61                          ;    
    62                          ;    #1 Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
    63                          ;	Wegen des längeren Codes und und einem deswegen länger werdenden
    64                          ;	Branch-Offset, wurde der Code ab cfinish zurückversetzt.
    65                          ;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
    66                          ;	noch an anderer Stelle eingespart werden.
    67                          ;	Dabei wird unmittelbar vor dem Kopieren des Strings nicht
    68                          ;	mehr explizit der Fall eines Strings mit Länge 0 betrachtet,
    69                          ;	der an dieser Stelle auch nicht auftreten kann, da
    70                          ;	der erste Durchgang durch alle Strings am SDS, bei den
    71                          ;	Variablen und den Arrays Strings der Länge 0 übergeht. 
    72                          ;	Selbst, wenn jemand böswilligerweise via allocate-Routine
    73                          ;	0-Längen-Speicher anfordert (was immer 2 Link-Bytes kostet),
    74                          ;	können diese Leerstring nicht referenziert werden. Im Zuge
    75                          ;	des zweiten Durchlaufs (Collection) würden diese degenerieren
    76                          ;	0-Längen-Strings auch wieder verschwinden.
    77                          ;
    78                          ;	Aktivierbar via use_fast_copy-Variable.
    79                          ;
    80                          ;    #2 Die Lückenmarkierung (Low-Byte mit der Länge) wird beim
    81                          ;	Kopieren des Strings mitgemacht. (-4 Byte Code, -5 T/String)
    82                          ;	Siehe no_opt_2-Abfrage bei Label cw3.
    83                          ;
    84                          ;    #3 Kein String-Kopieren durchführen, solange der String-Heap
    85                          ;	geordnet ist (also solange ptr = newptr ist). Sobald
    86                          ;	eine Lücke eliminiert wurde, laufen ptr und newptr auseinander.
    87                          ;
    88                          
    89                          ; Optimierung #1: Die optimierte Kopierroutine verwenden
    90                          ; aktiv
    91                          !set use_fast_copy=1
    92                          
    93                          ; Optimierung #2: Lückmarkierung teilweise mit String-Kopieren mitmachen.
    94                          ; ist immer aktiv
    95                          
    96                          ; Optimierung #3: Kein String-Kopieren, solange Heap geordnet ist.
    97                          ; Wenn aktiv (passt aber nicht ins ROM!), dann darf nicht Optimierung #3
    98                          ; aktiv sein!
    99                          ; inaktiv
   100                          ;!set opt_no_copy=1
   101                          !ifdef startaddress {
   102                          !set opt_no_copy=1
   103                          }
   104                          
   105                          !ifdef opt_no_copy {
   106                          !set no_opt_2=1
   107                          }
   108                          
   109                          ; Variante: Etwas kürzere und schnellere stralloc-Routine
   110                          ; Inaktiv, weil nicht verwendbar, da damit strptr nicht korrekt
   111                          ; ist und damit die "String concatenation" (String-Addition)
   112                          ; nicht funktionieren würde!
   113                          ;
   114                          ;!set alternate_stralloc=1
   115                          
   116                          
   117                          
   118                          ; Basic-Zeiger und -konstanten
   119                          
   120                          collected = $0f
   121                          
   122                          sdsbase  = $0019	; 1. Element String-Descriptor-Stacks (SDS)
   123                          			; wächst nach oben, max. 3 Elemente
   124                          			; zu je 3 Bytes.
   125                          sdsptr   = $16		; Zeiger auf nächstes freie Element
   126                          			; des String-Descriptor-Stacks (SDS)
   127                          
   128                          vartab   = $2d		; Basicprogrammende = Variablenanfang
   129                          arytab   = $2f		; Variablenende = Array-Bereichanfang
   130                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
   131                          fretop   = $33		; aktuelle String-Heap-Adresse
   132                          strptr	 = $35		; Hilfszeiger Stringzeiger (von stralloc)
   133                          memsiz   = $37		; höchste RAM-Adresse für Basic, Start
   134                          			; des nach unten wachsenden String-Heaps
   135                          ; Hilfsvariablen
   136                          
   137                          ptr	 = $22		; Arbeitszeiger, alter Heap
   138                          newptr	 = $4e		; Neuer Stringzeiger, neuer Heap
   139                          desclen	 = $53		; akt. Länge eines Stringdescriptors
   140                          aryptr	 = $58		; Array-Zeiger
   141                          descptr	 = $5f		; Descriptor-Zeiger
   142                          
   143                          getspa   = $b4f4	; "get space" ROM-Eintrittspunkt
   144                          garcoll  = $b526	; "garbage collect" ROM-Eintrittspunkt
   145                          
   146                          ; Vorbelegung der Speicherplätze
   147                          
   148                          basic    = $A000	; BASIC-ROM-Startadresse
   149                          romsize  = $2000	; ROM Länge 8K
   150                          
   151                          prozport = $01		; Prozessorport
   152                          memrom   = %00110111	; Basic+Kernal ROM, $37
   153                          membas   = %00110110	; Basic RAM+kernal ROM, $34
   154                          memram   = %00110101	; Basic+Kernal RAM, $35
   155                          
   156                          
   157                          ; Datenstrukturen
   158                          ;
   159                          ; String am Heap:
   160                          ;
   161                          ;   +--------------------------------------+
   162                          ;   |       +--------------+               |
   163                          ;   V       |              V               |
   164                          ;   +---+---+---+          +-----------+---+---+
   165                          ;   |LEN|LO |HI |          |STRINGDATEN|LO |HI |
   166                          ;   +---+---+---+          +-----------+---+---+
   167                          ;   ^    *******           ^            *******
   168                          ;   |       String.-Adr.   |               Descriptor-Adr.
   169                          ;   +-Descriptor-Adresse   +-String-Adresse
   170                          ;
   171                          ; Lücken am Heap:
   172                          ;                      
   173                          ;   +-------------+   +--------------+
   174                          ;   V             |   V              |
   175                          ;    +-----------+---+---+---------+---+---+
   176                          ;    |LÜCKE 2    |LEN|$FF|LÜCKE 1  |LEN|$FF|
   177                          ;    +-----------+---+---+---------+---+---+
   178                          ;                  ^  ***            ^  ***
   179                          ;                  |   Lückenmark.   |   Lückenmarkierung
   180                          ;                  Backlink-Adresse  Backlink-Adresse
   181                          
   182                          
   183                          
   184                          
   185                          ; ******************************* part 1 *************************************
   186                          
   187                          !macro part1_code .rom {
   188                          
   189                          ;***** Speicher von String-Heap anfordern
   190                          ;
   191                          ;	in:	A		; Länge anforderung
   192                          ;		fretop
   193                          ;	mod:	collected	; "GC aufgerufen"-Flag
   194                          ;		strptr		; temp. Zeiger
   195                          ;	out:	fretop		; Adresse auf String
   196                          ;		X,Y		; Adresse auf String
   197                          ;
   198                          ; Der String wird im Backlink stets als ungebrauchte Lücke
   199                          ; markiert! Dann muss die GC nur noch die Backlinks
   200                          ; der aktiven Strings setzen und kann die ungebrauchten
   201                          ; Strings überspringen.
   202                          
   203                          !if .rom != 0 {			; ROM-Patch?
   204                          basicerror = $b4d2		; Basic-Fehlermeldung
   205                          } else {
   206                          basicerror:
   207                          	jmp $b4d2
   208                          }
   209                          
   210                          allocate:
   211                          	lsr collected		; Flag löschen
   212                          retry	pha			; Länge der Anforderung,
   213                          				; für 2. Teil
   214                          				; Länge 0 möglich, verbraucht aber 2 Bytes
   215                          	eor #$ff		; negieren
   216                          	sec
   217                          	adc fretop		; A/X = fretop; A/X -= Länge
   218                          	ldx fretop+1
   219                          	bcs l1
   220                          	dex
   221                          	sec
   222                          l1	sbc #2			; A/X -= 2 Platz für Backlink einrechnen
   223                          	bcs l2
   224                          	dex
   225                          l2	cpx strend+1		; String-Heap voll (Array-Bereichende)?
   226                          	bcc checkcollect
   227                          	bne alloc		; nein, Bereich anfordern
   228                          	cmp strend 
   229                          	bcs alloc		; nein, Bereich anfordern
   230                          checkcollect
   231                          	ldx #16			; Basic-Fehler 16: "OUT OF MEMORY"
   232                          	lda collected
   233                          	bmi basicerror		; Collection schon gelaufen?
   234                          	jsr docollect		; nein, dann Garbage Collection, C=1 (immer!)
   235                          	ror collected		; Flag setzen (Bit 7) setzen
   236                          	pla			; Länge angeforderter Bereich
   237                          	jmp retry		; nochmal versuchen (Platz frei von GC?)
   238                          
   239                          alloc	jsr setfretop		; FRETOP = A/X
   240                          !if .rom != 0 {			; ROM-Patch?
   241                          	jmp stralloc		; zum 2. Teil: Allokation abschließen
   242                          } else {
   243                          	+part2_code		; 2. Teil gleich hier anhängen
   244                          }
   245                          
   246                          ;***** garbage collection
   247                          
   248                          ;	in:	-
   249                          ;	mod:	ptr		; Zeiger auf alten String-Heap
   250                          ;		newptr		; Zeiger auf neuen String-Heap
   251                          ;		descptr		; Zeiger auf Descriptor
   252                          ;		desclen		; Descriptor-Schrittweite
   253                          ;	out:	fretop		; Neue String-Heap-Position
   254                          ;		C=1
   255                          
   256                          docollect
   257                          
   258                          
   259                          ; Backlink aller temporären Strings am String-Descriptor-Stack setzen
   260                          
   261                          sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   262                          	ldx #>sdsbase		; da in 0-Page, immer 0
   263                          	jsr setptr		; damit ptr setzen
   264                          
   265                          sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   266                          	beq vars		; Ja, SDS durch, weiter mit Variablen
   267                          	jsr backlink		; sonst Backlink setzen
   268                          	beq sdsnext		; immer, weil High-Byte 0; nächsten SDS-Descr.
   269                          
   270                          ; Backlink aller String-Variablen setzen
   271                          
   272                          vars:	lda #5			; Descriptor-Schritt für Variablen
   273                          	sta desclen
   274                          	lda vartab		; Variablenbeginn
   275                          	ldx vartab+1
   276                          	jsr setptr		; ptr = A/X
   277                          
   278                          varnext	cpx arytab+1		; Variablenende?
   279                          	bne varbl
   280                          	cmp arytab
   281                          	beq arrays		; ja, weiter mit Arrays
   282                          varbl	jsr backlinkvar		; Backlink für nächste String-Variable setzen
   283                          	bne varnext		; immer; nächsten Var.-Descriptor
   284                          
   285                          ; Backlink bei allen String-Arrays setzen
   286                          
   287                          arrays:
   288                          	sta aryptr		; Variablenbereichende = Array-Bereichanfang
   289                          	stx aryptr+1 
   290                          	ldy #3			; Descriptor-Schritt bei String-Arrays
   291                          	sty desclen
   292                          
   293                          arrnext	cpx strend+1		; Array-Bereichende?
   294                          	bne arrbl
   295                          	cmp strend
   296                          	beq cleanwalk
   297                          arrbl	jsr backlinkarr		; Backlinks für nächstes Array setzen -> Z=0!
   298                          	bne arrnext		; immer; nächstes Array-Element
   299                          
   300                          
   301                          ; Ende, Zeiger zum neuen String-Heap übernehmen
   302                          
   303                          cfinish
   304                          	lda newptr		; Aufgeräumtzeiger ist ..
   305                          	ldx newptr+1
   306                          setfretop
   307                          	sta fretop		; neues FRETOP
   308                          	stx fretop+1 
   309                          	rts			; fertig!
   310                          
   311                          ; Nachdem nun alle Backlinks gesetzt sind
   312                          ; den String-Heap von oben nach unten durchgehen
   313                          ; und zusammenschieben ...
   314                          
   315                          cleanwalk:
   316                          	lda memsiz		; beim Basic-Speicherende
   317                          	ldx memsiz+1
   318                          	sta newptr		; ... beginnen
   319                          	stx newptr+1 
   320                          
   321                          ; Aufräumschleife
   322                          
   323                          cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   324                          	bne cwclean		; dann Heap durch und fertig.
   325                          	cmp fretop		; andernfalls aufräumen ...
   326                          	beq cfinish		; fertig, weil A/X = FRETOP
   327                          
   328                          ; nächsten String "aufräumen" ...
   329                          
   330                          cwclean	sec			; Aufgeräumtzeiger auf Backlink
   331                          	sbc #2
   332                          	bcs cw1
   333                          	dex			; A/X -> Backlink
   334                          
   335                          cw1	jsr setptr		; A/X -> ptr (Alt-String-Zeiger)
   336                          
   337                          	ldy #0
   338                          	lda (ptr),y		; Backlink low oder Lückenlänge
   339                          	iny			; Y=1
   340                          	tax			; -> X
   341                          	lda (ptr),y		; Backlink high
   342                          	cmp #$ff		; "String-nicht gebraucht"-Markierung
   343                          	bcc cwactive		; aktiver String
   344                          
   345                          	txa			; Lückenlänge
   346                          	eor #$ff		; negieren, C=1 (Komplement, +1)
   347                          	adc ptr			; (ptr - Lückenlänge)
   348                          	ldx ptr+1 
   349                          	bcs cwnext		; weiter mit nächstem/r String/Lücke
   350                          	dex			; High Byte
   351                          
   352                          cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   353                          				; weiter mit nächstem/r String/Lücke
   354                          
   355                          ; einen aktiven String nach oben schieben
   356                          
   357                          cwactive			; immer mit Y=1 angesprungen
   358                          	sta descptr+1		; Descriptor-Adresse
   359                          	stx descptr 
   360                          
   361                          	lda newptr		; Aufgeräumtzeiger -= 2
   362                          	sbc #1			; weil bereits C=0!
   363                          	sta newptr		; newptr -= 2
   364                          	bcs cw3
   365                          	dec newptr+1
   366                          	sec			; für SBC unten
   367                          
   368                          cw3	lda #$ff		; Backlink h: als Lücke markieren
   369                          	sta (newptr),y		; Y=1
   370                          	dey			; Y=0
   371                          !ifdef no_opt_2 {
   372                          	lda (descptr),y		; Descriptor: String-Länge
   373                          	sta (newptr),y		; Backlink l: Lückenlänge
   374                          } else {
   375                          				; Backlink l: Lückenlänge später beim
   376                          				; Kopieren ...
   377                          }
   378                          	lda newptr		; Aufgeräumtzeiger -= String-Länge
   379                          	sbc (descptr),y		; minus String-Länge, immer C=1, Y=0
   380                          	sta newptr
   381                          	bcs cw4
   382                          	dec newptr+1
   383                          	sec			; für SBC unten
   384                          
   385                          cw4	lda ptr			; Alt-String-Zeiger -= String-Länge
   386                          	sbc (descptr),y		; immer C=1
   387                          	sta ptr			; Arbeitszeiger = alte String-Adresse
   388                          	bcs cw5
   389                          	dec ptr+1
   390                          cw5
   391                          	; An dieser Stelle wäre eine Optimierung möglich, um das
   392                          	; Kopieren zu verhindern, wenn der String an der gleichen
   393                          	; Stelle bleibt - dabei darf die Optimierung #2 nicht
   394                          	; in Verwendung sein und es würden zusätzlich 10 Bytes gebraucht!
   395                          !ifdef opt_no_copy {
   396                          	cmp newptr		; ptr bereits in A
   397                          	bne cw6			; ptr != newptr, also kopieren
   398                          	lda ptr+1		; High Byte ...
   399                          	cmp newptr+1
   400                          	beq cwheapordered	; ptr = newptr, nicht kopieren
   401                          cw6
   402                          }
   403                          
   404                          	lda (descptr),y		; String-Länge
   405                          !ifndef use_fast_copy {
   406                          
   407                          				; immer, da Länge >0
   408                          !ifdef no_opt_2 {
   409                          	beq cwnocopy		; falls doch Länge 0, kein Kopieren,
   410                          				; Descriptor trotzdem anpassen ...
   411                          	tay			; als Index, mit Dekrementieren beginnen
   412                          } else { ; mit Optimierung #2
   413                          	tay			; Länge als Index
   414                          	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   415                          				; mit Lückenlänge belegen
   416                          }
   417                          cwloop	dey			; -> Startindex fürs Kopieren
   418                          	lda (ptr),y		; Arbeitszeiger mit altem String
   419                          cwbllen sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   420                          	tya			; Test auf Z-Flag!
   421                          	bne cwloop		; Index = 0 -> fertig kopiert
   422                          
   423                          } else { ; use_fast_copy!
   424                          
   425                          				; + 3 Byte, -2 T/Zeichen 
   426                          	tay			; Länge als Index
   427                          !ifdef no_opt_2 {
   428                          	bne cwentry		; immer, da Länge in Y>0, bei
   429                          				; Dekrementieren beginnen!
   430                          } else { ; mit Optimierung #2
   431                          	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   432                          				; mit Lückenlänge belegen
   433                          }
   434                          				; -> Startindex fürs Kopieren
   435                          cwloop	lda (ptr),y		; Arbeitszeiger mit altem String
   436                          cwbllen	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   437                          cwentry	dey			; Test auf Z-Flag!
   438                          	bne cwloop		; Index = 0 -> fertig kopiert
   439                          cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   440                          	sta (newptr),y		; Aufgeräumtzeiger mit neuem String-Ort
   441                          }
   442                          
   443                          cwnocopy:
   444                          				; Y=0
   445                          	iny			; Y=1
   446                          	lda newptr		; im Descriptor:
   447                          	sta (descptr),y		; String-Adresse L: neue Adresse
   448                          	iny			; Y=2
   449                          	lda newptr+1
   450                          	sta (descptr),y		; String-Adresse H: neue Adresse
   451                          
   452                          cwheapordered:
   453                          	lda ptr
   454                          	ldx ptr+1		; High-Byte immer !=0
   455                          	bne cwnext		; immer; weiter in Schleife
   456                          
   457                          
   458                          ;**** Backlink setzen
   459                          ;
   460                          ; 	in:		ptr	Descriptor-Adresse
   461                          ; 	out:		ptr	Descriptor-Adresse
   462                          ;			A/X
   463                          ;			Z=0	wenn nicht am SDS
   464                          ;			Z=1	wenn am SDS
   465                          ;	destroy:	newptr
   466                          ;	called:		blaset, backlinkvar
   467                          
   468                          backlink:
   469                          	ldy #0
   470                          	lda (ptr),y		; String-Länge
   471                          	beq blnext		; fertig, wenn =0
   472                          	iny
   473                          	clc
   474                          	adc (ptr),y		; Backlink-Position (am String-Ende)
   475                          	sta newptr		; Backlink-Zeiger L
   476                          	tax
   477                          	iny
   478                          	lda (ptr),y
   479                          	adc #0
   480                          	sta newptr+1		; Backlink-Zeiger H
   481                          	cmp strend+1		; < Array-Bereichende (außerhalb Heap)?
   482                          	bcc blnext		; ja, denn nächsten String
   483                          	bne blsetdesc
   484                          	cpx strend 
   485                          	bcc blnext		; < Array-Bereichende (außerhalb Heap)?
   486                          
   487                          blsetdesc:
   488                          	ldy #1
   489                          	lda ptr+1
   490                          	sta (newptr),y		; Descriptor-Adresse ...
   491                          	dey
   492                          	lda ptr
   493                          	sta (newptr),y		; in den Backlink übertragen
   494                          
   495                          blnext	lda desclen		; nächster String/nächste Variable
   496                          	clc			; Schrittweite zum nächsten Descriptor
   497                          	adc ptr			; ptr += desclen
   498                          	sta ptr
   499                          	bcc +
   500                          	inc ptr+1
   501                          +	ldx ptr+1		; immer != 0 -> Z=0 (außer bei SDS, Z=1)
   502                          	rts
   503                          
   504                          ;**** Nächste String-Variable und Backlink setzen
   505                          ;
   506                          ; 	in:		ptr	Variablenadresse
   507                          ; 	out:		ptr	Variablenadresse
   508                          ;			A/X
   509                          ;			Z=0
   510                          ;	destroy:	newptr
   511                          ;	called:		varbl (vars)
   512                          
   513                          backlinkvar:
   514                          	ldy #0
   515                          	lda (ptr),y		; Variablenname 1. Zeichen
   516                          	tax			; Typstatus merken
   517                          	iny
   518                          	lda (ptr),y		; Variablenname 2. Zeichen
   519                          	tay			; Typstatus merken
   520                          
   521                          	lda #2			; Descriptor-Adresse (in Variable)
   522                          	clc
   523                          	adc ptr			; ptr += 2
   524                          	sta ptr
   525                          	bcc +
   526                          	inc ptr+1
   527                          +
   528                          	txa			; Variablentyp prüfen
   529                          	bmi blnext		; keine String, nächste Variable
   530                          	tya
   531                          	bmi backlink		; Backlink setzen
   532                          	bpl blnext		; keine String-Var., nächste Variable
   533                          
   534                          }	; macro part1_code ende
   535                          
   536                          
   537                          
   538                          ; ******************************* part 4 *************************************
   539                          
   540                          !macro part4_code .rom {
   541                          
   542                          !if .rom != 0 {			; ROM-Patch?
   543                          	jmp part4_continue	; nur für ROM-Patch
   544                          	; Dies überspringt die Heap-Korrektur, für
   545                          	; einen String vom String-Descriptor-Stack,
   546                          	; falls der sich zuoberst am Heap befindet.
   547                          	; von $B6C1 bis $B6D5
   548                          } else {
   549                          ;B6C1: D0 13     BNE $B6D6	; Sring auf SDS?
   550                          ;B6C3: C4 34     CPY $34	; ja, dann String am Heap-Top?
   551                          ;B6C5: D0 0F     BNE $B6D6	; Low-Byte, nein
   552                          ;B6C7: E4 33     CPX $33
   553                          ;B6C9: D0 0B     BNE $B6D6	; High-Byte, nein
   554                          ;B6CB: 48        PHA		; ja, SDS-String am Heap-Top!
   555                          				; also wieder entfernen
   556                          
   557                          	; die Originalroutine entfernt nur den String
   558                          	; (korrigiert den Heap um die Stringlänge), aber
   559                          	; hier muss auch noch der Backlink entfernt
   560                          	; werden:
   561                          	; (Implementierung mit ADC statt INC braucht 1 Byte mehr Platz,
   562                          	;  wenngleich auch etwas schneller.)
   563                          LB6CC:
   564                          	sec			; Länge in A, bereits am Stack
   565                          	adc $33			; $33/34 += A + 1 (Backlink 1. Teil)
   566                          	sta $33
   567                          	bcc +
   568                          	inc $34
   569                          +	inc $33			; $33/34 += 1 (Backlink 2. Teil)
   570                          	bne +
   571                          	inc $34
   572                          +	pla
   573                          	; ersetzt die folgende Heap-Korrektur, die nur
   574                          	; die String-Länge berücksichtigt ...
   575                          ;B6CC: 18        CLC
   576                          ;B6CD: 65 33     ADC $33	; Heap-Top (bottom of string space)
   577                          ;B6CF: 85 33     STA $33	; um String-Länge (in A)
   578                          ;B6D1: 90 02     BCC $B6D5	; erhöhen
   579                          ;B6D3: E6 34     INC $34
   580                          ;B6D5: 68        PLA
   581                          	jmp $B6D6		; zurück ins ROM
   582                          }
   583                          
   584                          ;**** Nächste Array-Variable und Backlink setzen
   585                          ;
   586                          ; 	in: 		ptr	Arrayadresse
   587                          ; 	out:		ptr	Adresse Folge-array
   588                          ;			aryptr	Adresse Folge-array
   589                          ;			A/X	Adresse Folge-array
   590                          ;			Z=0
   591                          ;	destroy:	newptr
   592                          ;	called:		arrbl (arrays)
   593                          
   594                          backlinkarr:
   595                          	ldy #0
   596                          	lda (ptr),y		; Variablenname 1. Zeichen
   597                          	php			; für später
   598                          	iny
   599                          	lda (ptr),y		; Variablenname 2. Zeichen
   600                          	tax			; für später
   601                          
   602                          	iny
   603                          	lda (ptr),y		; Offset nächstes Array
   604                          	clc			; Bugfix 1: C=0 definiert setzen
   605                          	adc aryptr
   606                          !if .rom != 0 {			; ROM-Patch?
   607                          	jmp backlinkarr2	; nur für ROM-Patch
   608                          				; weiter an anderer Stelle!
   609                          }
   610                          }	; macro part4_code
   611                          
   612                          
   613                          ; ******************************* part 3 *************************************
   614                          
   615                          !macro part3_code .rom {
   616                          
   617                          !if .rom != 0 {			; ROM-Patch?
   618                          	!byte  0 		; Einschaltmeldung kürzen
   619                          }
   620                          
   621                          backlinkarr2:
   622                          	sta aryptr		; Folge-Array L
   623                          	iny
   624                          	lda (ptr),y
   625                          	adc aryptr+1 
   626                          	sta aryptr+1		; Folge-Array H
   627                          
   628                          	plp			; Arraytyp:
   629                          	bmi blaskip		; kein String-Array
   630                          	txa
   631                          	bpl blaskip		; kein String-Array
   632                          
   633                          	iny			; Y=4
   634                          	lda (ptr),y		; Anzahl der Dimensionen (< 126 !)
   635                          	asl 			; *2
   636                          	adc #5			; + 5 (Var.Name+Offset+Dimensionen)
   637                          	adc ptr			; auf 1. Element ...
   638                          	sta ptr 
   639                          	bcc bla1
   640                          	inc ptr+1 
   641                          bla1	ldx ptr+1		; positionieren
   642                          
   643                          blanext	cpx aryptr+1		; Array-Ende erreicht?
   644                          	bne blaset		; nein, Backlink setzen
   645                          	cmp aryptr
   646                          	beq blafinish		; Array fertig, Bugfix 2: Z-Flag löschen!
   647                          blaset
   648                          	jsr backlink		; Backlink setzen
   649                          	bne blanext		; immer (High-Byte != 0)
   650                          
   651                          blaskip
   652                          	lda aryptr		; Zeiger auf Folge-Array
   653                          blafinish
   654                          	ldx aryptr+1 		; Z=0 sicherstellen
   655                          
   656                          setptr	sta ptr			; Arbeitszeiger setzen
   657                          	stx ptr+1
   658                          	rts			; immer Z=0
   659                          
   660                          ; für ROM-Patch:
   661                          ;--- $e4b7 - $e4d2 unused ($aa)
   662                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   663                          ;----              sonst Patch für andere Zwecke
   664                          
   665                          }	; macro part3_code ende
   666                          
   667                          
   668                          ; ******************************* part 2 *************************************
   669                          
   670                          !macro part2_code {
   671                          
   672                          ;**** String Allocation (Fortsetzung)
   673                          ;
   674                          ;	in: 	TOS		; Länge
   675                          ;		fretop		; String-Adresse
   676                          ;	out:	fretop		; String-Adresse
   677                          ;		strptr		; String-Adresse (wird nicht verwendet)
   678                          ;				; (bei alternate_stralloc eventuell mit
   679                          ;				; inkrementiertem High-Byte)
   680                          ;		A		; Länge
   681                          ;		X,Y		; String-Adresse (L,H)
   682                          ;	called:	allocate (in Fortsetzung)
   683                          
   684                            !ifndef alternate_stralloc {
   685                          stralloc:
   686                          	sta strptr		; strptr = A/X = FRETOP
   687                          	stx strptr+1
   688                          	tax			; A in X aufheben
   689                          	pla			; Länge temp. vom Stack 
   690                          	pha			; wieder auf Stack, nun auch in A
   691                          	tay			; Index=Länge (Backlink-position)
   692                          	sta (fretop),y		; Backlink L = String-/Lückenlänge
   693                          	iny			; Y=Länge+1
   694                          	bne sa1			; wenn Länge=255, dann
   695                          	inc fretop+1		; Überlauf, aber nur temporär!
   696                          
   697                          sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   698                          	sta (fretop),y
   699                          	ldy strptr+1
   700                          	sty fretop+1		; Überlaufkorr. rückgängig
   701                          	pla			; Länge vom Stack nehmen
   702                          	rts
   703                          
   704                            } else {
   705                          ; alternative, etwas kürzere Variante (-3 T, -2 B),
   706                          ; aber NICHT VERWENDBAR!
   707                          
   708                          stralloc:
   709                          	sta strptr		; strptr = A/X = FRETOP
   710                          	stx strptr+1
   711                          	tax			; A in X aufheben
   712                          	pla			; Länge temp. vom Stack 
   713                          	pha			; wieder auf Stack, nun auch in A
   714                          	tay			; Index=Länge (Backlink-position)
   715                          	sta (strptr),y		; Backlink L = String-/Lückenlänge
   716                          	iny			; Y=Länge+1
   717                          	bne sa1			; wenn Länge=255, dann
   718                          	inc strptr+1		; Überlauf, aber nur temporär!
   719                          
   720                          sa1	lda #$ff		; Backlink H = Markierung "Lücke"
   721                          	sta (strptr),y
   722                          	ldy fretop+1		; in Y String-Adresse High-Byte
   723                          	pla			; Länge vom Stack nehmen
   724                          	rts
   725                          				; Hier weicht strptr+1 u.U. von fretop+1 ab,
   726                          				; was aber ein Problem darstellt, da
   727                          				; es im BASIC-Interpreter ab $B68C eine
   728                          				; Stelle gibt, wo etwa im Zuge der
   729                          				; String-Addition die zusammengefügten
   730                          				; Strings in den Zielbereich kopiert werden.
   731                          				; Dabei dient strptr/strptr+1 als Ziel! 
   732                            }
   733                          }
   734                          
   735                          
   736                          
   737                          !ifndef startaddress {
   738                          
   739                          ;********************************* ROM Version *******************************
   740                          
   741                          !source "loader.asm"
   742                          
   743                          ;
   744                          ; Patch-Liste für "loader"
   745                          ;
   746                          
   747                          patchlist:
   748                          
   749                          !wo part1_real,part1_real_end-part1_real,part1
   750                          !wo part2_real,part2_real_end-part2_real,part2
   751                          !wo part3_real,part3_real_end-part3_real,part3
   752                          !wo part4_real,part4_real_end-part4_real,part4
   753                          !wo 0  ; Endemarkierung
   754                          
   755                          !set part1_rom = $b4f4
   756                          !set part2_rom = $e4ba
   757                          !set part3_rom = $e474
   758                          !set part4_rom = $b6c1
   759                          !set part4_continue = $b6d6
   760                          
   761                          
   762                          part1_real
   763                          !pseudopc part1_rom {
   764                          
   765                          part1:
   766                          	+part1_code 1
   767                          }
   768                          part1_real_end
   769                          
   770                          	; Codebereich 1: darf den zur Verfügung stehenden Bereich nicht überschreiten!
   771                          	!set part1_end = (part1_real_end-part1_real)+part1
   772                          	!if ( part1_end > $B63D ) {
   773                          		!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   774                          	}
   775                          
   776                          part4_real
   777                          !pseudopc part4_rom {
   778                          part4:
   779                          	+part4_code 1
   780                          !if * > part4_continue {
   781                          	!error "part4 ist zu lang!"
   782                          }
   783                          }
   784                          part4_real_end
   785                          
   786                          part3_real
   787                          !pseudopc part3_rom {
   788                          part3:
   789                          	+part3_code 1
   790                          }
   791                          part3_real_end
   792                          
   793                          part2_real
   794                          !pseudopc part2_rom {
   795                          part2:
   796                          	+part2_code
   797                          }
   798                          part2_real_end
   799                          
   800                          
   801                          ; Einsprungspunkt an korrekter Position?
   802                          
   803                          ; Kann erst nach dem Label docollect gemacht werden!
   804                          
   805                          !if (garcoll != docollect) {
   806                          	!error "Einstiegspunkt nicht an richtiger Stelle! ",garcoll,"!=",docollect
   807                          }
   808                          
   809                          } else {
   810                          ;********************************* ROM Version *******************************
   811                          
   812                          	* = startaddress
   813                          
   814                          
   815                          ; Installer
   816                          
   817                          install:
   818                                  ; BASIC ins RAM kopieren, um die GC-Routine
   819                                  ; zu patchen ...
   820  c000 a937                       lda #memrom
   821  c002 8501                       sta prozport		; alles ROM (also vom ROM kopieren)
   822                          
   823  c004 a000                       ldy #<basic		; ROM-Beginn
   824  c006 8422                       sty ptr
   825  c008 a9a0                       lda #>basic     
   826  c00a 8523                       sta ptr+1		; BASIC-ROM Anfang
   827  c00c a220                       ldx #>(romsize)		; BASIC-ROM Länge in Pages
   828  c00e b122               cpyrom  lda (ptr),y		; ROM lesen
   829  c010 9122                       sta (ptr),y		; RAM schreiben
   830  c012 c8                         iny
   831  c013 d0f9                       bne cpyrom
   832  c015 e623                       inc ptr+1		; nächste Page
   833  c017 ca                         dex			; Page-Zähler
   834  c018 d0f4                       bne cpyrom
   835                          
   836  c01a a501                       lda prozport		; auf RAM umschalten
   837  c01c 29fe                       and #%11111110		; "BASIC-ROM aus"-Maske
   838  c01e 8501                       sta prozport
   839                          
   840  c020 a995                       lda #<docollect		; "jmp docollect"
   841  c022 8d27b5                     sta garcoll+1		; patchen ...
   842  c025 a9c0                       lda #>docollect
   843  c027 8d28b5                     sta garcoll+2
   844                          
   845  c02a a94d                       lda #<allocate		; "jmp allocate"
   846  c02c 8df5b4                     sta getspa+1		; patchen ...
   847  c02f a9c0                       lda #>allocate
   848  c031 8df6b4                     sta getspa+2
   849                          
   850  c034 a9b6                       lda #<LB6CC		; "jmp LB6CC"
   851  c036 8dcdb6                     sta $b6cc+1		; patchen ...
   852  c039 a9c1                       lda #>LB6CC
   853  c03b 8dceb6                     sta $b6cc+2
   854                          
   855  c03e a94c                       lda #$4c		; JMP-Opcode
   856  c040 8d26b5                     sta garcoll
   857  c043 8df4b4                     sta getspa
   858  c046 8dccb6                     sta $b6cc
   859  c049 60                 	rts
   860                          
   861                          	; Code-Teile zustammenstellen, sind nun unmittelbar hintereinander ..
   862                          

; ******** Source: garbcol.asm, macro: part1_code
   187                          .rom
   188                          
   189                          
   190                          
   191                          
   192                          
   193                          
   194                          
   195                          
   196                          
   197                          
   198                          
   199                          
   200                          
   201                          
   202                          
   203                          !if .rom != 0 { 
   204                          basicerror = $b4d2 
   205                          
   206                          basicerror
   207  c04a 4cd2b4              jmp $b4d2
   208                          
   209                          
   210                          allocate
   211  c04d 460f                lsr collected 
   212  c04f 48                 retry pha 
   213                           
   214                           
   215  c050 49ff                eor #$ff 
   216  c052 38                  sec
   217  c053 6533                adc fretop 
   218  c055 a634                ldx fretop+1
   219  c057 b002                bcs l1
   220  c059 ca                  dex
   221  c05a 38                  sec
   222  c05b e902               l1 sbc #2 
   223  c05d b001                bcs l2
   224  c05f ca                  dex
   225  c060 e432               l2 cpx strend+1 
   226  c062 9006                bcc checkcollect
   227  c064 d013                bne alloc 
   228  c066 c531                cmp strend 
   229  c068 b00f                bcs alloc 
   230                          checkcollect
   231  c06a a210                ldx #16 
   232  c06c a50f                lda collected
   233  c06e 30da                bmi basicerror 
   234  c070 2095c0              jsr docollect 
   235  c073 660f                ror collected 
   236  c075 68                  pla 
   237  c076 4c4fc0              jmp retry 
   238                          
   239  c079 20d6c0             alloc jsr setfretop 
   240                          !if .rom != 0 { 
   241                           jmp stralloc 
   242                          

; ******** Source: garbcol.asm, macro: part2_code
   670                          
   671                          
   672                          
   673                          
   674                          
   675                          
   676                          
   677                          
   678                          
   679                          
   680                          
   681                          
   682                          
   683                          
   684                           !ifndef alternate_stralloc {
   685                          stralloc
   686  c07c 8535                sta strptr 
   687  c07e 8636                stx strptr+1
   688  c080 aa                  tax 
   689  c081 68                  pla 
   690  c082 48                  pha 
   691  c083 a8                  tay 
   692  c084 9133                sta (fretop),y 
   693  c086 c8                  iny 
   694  c087 d002                bne sa1 
   695  c089 e634                inc fretop+1 
   696                          
   697  c08b a9ff               sa1 lda #$ff 
   698  c08d 9133                sta (fretop),y
   699  c08f a436                ldy strptr+1
   700  c091 8434                sty fretop+1 
   701  c093 68                  pla 
   702  c094 60                  rts
   703                          
   704                           
   705                          
   706                          
   707                          
   708                          stralloc
   709                           sta strptr 
   710                           stx strptr+1
   711                           tax 
   712                           pla 
   713                           pha 
   714                           tay 
   715                           sta (strptr),y 
   716                           iny 
   717                           bne sa1 
   718                           inc strptr+1 
   719                          
   720                          sa1 lda #$ff 
   721                           sta (strptr),y
   722                           ldy fretop+1 
   723                           pla 
   724                           rts
   725                           
   726                           
   727                           
   728                           
   729                           
   730                           
   731                           
   732                           

; ******** Source: garbcol.asm, macro: part1_code
   243                          
   244                          
   245                          
   246                          
   247                          
   248                          
   249                          
   250                          
   251                          
   252                          
   253                          
   254                          
   255                          
   256                          docollect
   257                          
   258                          
   259                          
   260                          
   261  c095 a919               sds
   262  c097 a200                ldx #>sdsbase 
   263  c099 2008c2              jsr setptr 
   264                          
   265  c09c c516               sdsnext cmp sdsptr 
   266  c09e f005                beq vars 
   267  c0a0 2063c1              jsr backlink 
   268  c0a3 f0f7                beq sdsnext 
   269                          
   270                          
   271                          
   272  c0a5 a905               vars
   273  c0a7 8553                sta desclen
   274  c0a9 a52d                lda vartab 
   275  c0ab a62e                ldx vartab+1
   276  c0ad 2008c2              jsr setptr 
   277                          
   278  c0b0 e430               varnext cpx arytab+1 
   279  c0b2 d004                bne varbl
   280  c0b4 c52f                cmp arytab
   281  c0b6 f005                beq arrays 
   282  c0b8 209ac1             varbl jsr backlinkvar 
   283  c0bb d0f3                bne varnext 
   284                          
   285                          
   286                          
   287                          arrays
   288  c0bd 8558                sta aryptr 
   289  c0bf 8659                stx aryptr+1 
   290  c0c1 a003                ldy #3 
   291  c0c3 8453                sty desclen
   292                          
   293  c0c5 e432               arrnext cpx strend+1 
   294  c0c7 d004                bne arrbl
   295  c0c9 c531                cmp strend
   296  c0cb f00e                beq cleanwalk
   297  c0cd 20c9c1             arrbl jsr backlinkarr 
   298  c0d0 d0f3                bne arrnext 
   299                          
   300                          
   301                          
   302                          
   303                          cfinish
   304  c0d2 a54e                lda newptr 
   305  c0d4 a64f                ldx newptr+1
   306                          setfretop
   307  c0d6 8533                sta fretop 
   308  c0d8 8634                stx fretop+1 
   309  c0da 60                  rts 
   310                          
   311                          
   312                          
   313                          
   314                          
   315                          cleanwalk
   316  c0db a537                lda memsiz 
   317  c0dd a638                ldx memsiz+1
   318  c0df 854e                sta newptr 
   319  c0e1 864f                stx newptr+1 
   320                          
   321                          
   322                          
   323  c0e3 e434               cwnext cpx fretop+1 
   324  c0e5 d004                bne cwclean 
   325  c0e7 c533                cmp fretop 
   326  c0e9 f0e7                beq cfinish 
   327                          
   328                          
   329                          
   330  c0eb 38                 cwclean sec 
   331  c0ec e902                sbc #2
   332  c0ee b001                bcs cw1
   333  c0f0 ca                  dex 
   334                          
   335  c0f1 2008c2             cw1 jsr setptr 
   336                          
   337  c0f4 a000                ldy #0
   338  c0f6 b122                lda (ptr),y 
   339  c0f8 c8                  iny 
   340  c0f9 aa                  tax 
   341  c0fa b122                lda (ptr),y 
   342  c0fc c9ff                cmp #$ff 
   343  c0fe 900c                bcc cwactive 
   344                          
   345  c100 8a                  txa 
   346  c101 49ff                eor #$ff 
   347  c103 6522                adc ptr 
   348  c105 a623                ldx ptr+1 
   349  c107 b0da                bcs cwnext 
   350  c109 ca                  dex 
   351                          
   352  c10a d0d7               cw2 bne cwnext 
   353                           
   354                          
   355                          
   356                          
   357                          cwactive 
   358  c10c 8560                sta descptr+1 
   359  c10e 865f                stx descptr 
   360                          
   361  c110 a54e                lda newptr 
   362  c112 e901                sbc #1 
   363  c114 854e                sta newptr 
   364  c116 b003                bcs cw3
   365  c118 c64f                dec newptr+1
   366  c11a 38                  sec 
   367                          
   368  c11b a9ff               cw3 lda #$ff 
   369  c11d 914e                sta (newptr),y 
   370  c11f 88                  dey 
   371                          !ifdef no_opt_2 {
   372  c120 b15f                lda (descptr),y 
   373  c122 914e                sta (newptr),y 
   374                          
   375                           
   376                           
   377                          
   378  c124 a54e                lda newptr 
   379  c126 f15f                sbc (descptr),y 
   380  c128 854e                sta newptr
   381  c12a b003                bcs cw4
   382  c12c c64f                dec newptr+1
   383  c12e 38                  sec 
   384                          
   385  c12f a522               cw4 lda ptr 
   386  c131 f15f                sbc (descptr),y 
   387  c133 8522                sta ptr 
   388  c135 b002                bcs cw5
   389  c137 c623                dec ptr+1
   390                          cw5
   391                           
   392                           
   393                           
   394                           
   395                          !ifdef opt_no_copy {
   396  c139 c54e                cmp newptr 
   397  c13b d006                bne cw6 
   398  c13d a523                lda ptr+1 
   399  c13f c54f                cmp newptr+1
   400  c141 f01a                beq cwheapordered 
   401                          cw6
   402                          
   403                          
   404  c143 b15f                lda (descptr),y 
   405                          !ifndef use_fast_copy {
   406                          
   407                           
   408                          !ifdef no_opt_2 {
   409                           beq cwnocopy 
   410                           
   411                           tay 
   412                          
   413                           tay 
   414                           bne cwbllen 
   415                           
   416                          
   417                          cwloop dey 
   418                           lda (ptr),y 
   419                          cwbllen sta (newptr),y 
   420                           tya 
   421                           bne cwloop 
   422                          
   423                          
   424                          
   425                           
   426  c145 a8                  tay 
   427                          !ifdef no_opt_2 {
   428  c146 d004                bne cwentry 
   429                           
   430                          
   431                           bne cwbllen 
   432                           
   433                          
   434                           
   435  c148 b122               cwloop lda (ptr),y 
   436  c14a 914e               cwbllen sta (newptr),y 
   437  c14c 88                 cwentry dey 
   438  c14d d0f9                bne cwloop 
   439  c14f b122               cwone lda (ptr),y 
   440  c151 914e                sta (newptr),y 
   441                          
   442                          
   443                          cwnocopy
   444                           
   445  c153 c8                  iny 
   446  c154 a54e                lda newptr 
   447  c156 915f                sta (descptr),y 
   448  c158 c8                  iny 
   449  c159 a54f                lda newptr+1
   450  c15b 915f                sta (descptr),y 
   451                          
   452                          cwheapordered
   453  c15d a522                lda ptr
   454  c15f a623                ldx ptr+1 
   455  c161 d080                bne cwnext 
   456                          
   457                          
   458                          
   459                          
   460                          
   461                          
   462                          
   463                          
   464                          
   465                          
   466                          
   467                          
   468                          backlink
   469  c163 a000                ldy #0
   470  c165 b122                lda (ptr),y 
   471  c167 f023                beq blnext 
   472  c169 c8                  iny
   473  c16a 18                  clc
   474  c16b 7122                adc (ptr),y 
   475  c16d 854e                sta newptr 
   476  c16f aa                  tax
   477  c170 c8                  iny
   478  c171 b122                lda (ptr),y
   479  c173 6900                adc #0
   480  c175 854f                sta newptr+1 
   481  c177 c532                cmp strend+1 
   482  c179 9011                bcc blnext 
   483  c17b d004                bne blsetdesc
   484  c17d e431                cpx strend 
   485  c17f 900b                bcc blnext 
   486                          
   487                          blsetdesc
   488  c181 a001                ldy #1
   489  c183 a523                lda ptr+1
   490  c185 914e                sta (newptr),y 
   491  c187 88                  dey
   492  c188 a522                lda ptr
   493  c18a 914e                sta (newptr),y 
   494                          
   495  c18c a553               blnext lda desclen 
   496  c18e 18                  clc 
   497  c18f 6522                adc ptr 
   498  c191 8522                sta ptr
   499  c193 9002                bcc +
   500  c195 e623                inc ptr+1
   501  c197 a623               + ldx ptr+1 
   502  c199 60                  rts
   503                          
   504                          
   505                          
   506                          
   507                          
   508                          
   509                          
   510                          
   511                          
   512                          
   513                          backlinkvar
   514  c19a a000                ldy #0
   515  c19c b122                lda (ptr),y 
   516  c19e aa                  tax 
   517  c19f c8                  iny
   518  c1a0 b122                lda (ptr),y 
   519  c1a2 a8                  tay 
   520                          
   521  c1a3 a902                lda #2 
   522  c1a5 18                  clc
   523  c1a6 6522                adc ptr 
   524  c1a8 8522                sta ptr
   525  c1aa 9002                bcc +
   526  c1ac e623                inc ptr+1
   527                          +
   528  c1ae 8a                  txa 
   529  c1af 30db                bmi blnext 
   530  c1b1 98                  tya
   531  c1b2 30af                bmi backlink 
   532  c1b4 10d6                bpl blnext 
   533                          

; ******** Source: garbcol.asm
   863                           ohne ROM-Spezialitäten, da ist part2_code
   864                          				; inkludiert!

; ******** Source: garbcol.asm, macro: part4_code
   540                          .rom
   541                          
   542                          !if .rom != 0 { 
   543                           jmp part4_continue 
   544                           
   545                           
   546                           
   547                           
   548                          
   549                          
   550                          
   551                          
   552                          
   553                          
   554                          
   555                           
   556                          
   557                           
   558                           
   559                           
   560                           
   561                           
   562                           
   563                          LB6CC
   564  c1b6 38                  sec 
   565  c1b7 6533                adc $33 
   566  c1b9 8533                sta $33
   567  c1bb 9002                bcc +
   568  c1bd e634                inc $34
   569  c1bf e633               + inc $33 
   570  c1c1 d002                bne +
   571  c1c3 e634                inc $34
   572  c1c5 68                 + pla
   573                           
   574                           
   575                          
   576                          
   577                          
   578                          
   579                          
   580                          
   581  c1c6 4cd6b6              jmp $B6D6 
   582                          
   583                          
   584                          
   585                          
   586                          
   587                          
   588                          
   589                          
   590                          
   591                          
   592                          
   593                          
   594                          backlinkarr
   595  c1c9 a000                ldy #0
   596  c1cb b122                lda (ptr),y 
   597  c1cd 08                  php 
   598  c1ce c8                  iny
   599  c1cf b122                lda (ptr),y 
   600  c1d1 aa                  tax 
   601                          
   602  c1d2 c8                  iny
   603  c1d3 b122                lda (ptr),y 
   604  c1d5 18                  clc 
   605  c1d6 6558                adc aryptr
   606                          !if .rom != 0 { 
   607                           jmp backlinkarr2 
   608                           
   609                          

; ******** Source: garbcol.asm
   865                           ohne ROM-Spezialitäten

; ******** Source: garbcol.asm, macro: part3_code
   615                          .rom
   616                          
   617                          !if .rom != 0 { 
   618                           !byte 0 
   619                          
   620                          
   621                          backlinkarr2
   622  c1d8 8558                sta aryptr 
   623  c1da c8                  iny
   624  c1db b122                lda (ptr),y
   625  c1dd 6559                adc aryptr+1 
   626  c1df 8559                sta aryptr+1 
   627                          
   628  c1e1 28                  plp 
   629  c1e2 3020                bmi blaskip 
   630  c1e4 8a                  txa
   631  c1e5 101d                bpl blaskip 
   632                          
   633  c1e7 c8                  iny 
   634  c1e8 b122                lda (ptr),y 
   635  c1ea 0a                  asl 
   636  c1eb 6905                adc #5 
   637  c1ed 6522                adc ptr 
   638  c1ef 8522                sta ptr 
   639  c1f1 9002                bcc bla1
   640  c1f3 e623                inc ptr+1 
   641  c1f5 a623               bla1 ldx ptr+1 
   642                          
   643  c1f7 e459               blanext cpx aryptr+1 
   644  c1f9 d004                bne blaset 
   645  c1fb c558                cmp aryptr
   646  c1fd f007                beq blafinish 
   647                          blaset
   648  c1ff 2063c1              jsr backlink 
   649  c202 d0f3                bne blanext 
   650                          
   651                          blaskip
   652  c204 a558                lda aryptr 
   653                          blafinish
   654  c206 a659                ldx aryptr+1 
   655                          
   656  c208 8522               setptr sta ptr 
   657  c20a 8623                stx ptr+1
   658  c20c 60                  rts 
   659                          
   660                          
   661                          
   662                          
   663                          
   664                          

; ******** Source: garbcol.asm
   866                           ohne ROM-Spezialitäten
   867                          
   868                          
   869                          }
   870                          
