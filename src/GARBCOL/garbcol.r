
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
    13                          ;      n�chsten String Richtung niedriger Adressen.
    14                          ;   2) Nun wird der String-Speicher absteigend durchgegangen,
    15                          ;      wobei nur die aktiven Strings nach "oben" �ber
    16                          ;      etwaige L�cken hinweg kopiert werden. Die ungenutzten
    17                          ;      L�cken werden dabei �bergangen.
    18                          ;      Beim Kopieren wird der Backlink wieder entfernt
    19                          ;      (als ungenutzt markiert), da ja bis zur n�chsten
    20                          ;      Kompaktierung der Speicher aufgegeben werden k�nnte.
    21                          ;
    22                          ; Im Vergleich zu der von CBM eingesetzten Routine, wird
    23                          ; hier auf eine platzraubende Optimierung verzichtet,
    24                          ; wenn ein Teil oder der gesamt String-Speicher schon
    25                          ; kompaktiert sein sollte. Es werden dann die Strings
    26                          ; im schlimmsten Fall wieder �ber sich selbst kopiert.
    27                          ;
    28                          ; Ein gleichwertiger Ansatz w�re eine einfache Abfrage
    29                          ; bei opt_no_copy, ob ptr (Arbeitszeiger auf den alten
    30                          ; Heap) gleich newptr (neuer Heap) ist. Solange diese
    31                          ; der Fall ist, ist keine Kopieraktion (und auch
    32                          ; keine Descriptor-Korrektur) notwendig.
    33                          ; In diesem Fall kann allerdings die Optimierung #2 nicht
    34                          ; verwendet werden, da sonst die Backlink-Markierung
    35                          ; nicht in allen F�llen passieren w�rde!
    36                          ;
    37                          ; �berarbeitetet und korrigiert:
    38                          ;	2013-11-15 Johann E. Klasek, johann at klasek at
    39                          ; Optimiert:
    40                          ;	2019-03-20 Johann E. Klasek, johann at klasek at
    41                          ; �berarbeitet:
    42                          ;	2021-04-11 Johann E. Klasek, johann at klasek at
    43                          ;
    44                          ; Bugfixes:
    45                          ;
    46                          ;	1) in backlinkarr:
    47                          ;	   das C-Flag ist beim Errechnen des Folge-Arrays
    48                          ;	   definiert zu l�schen, sonst werden ev. nicht 
    49                          ;	   alle Elemente aller Arrays mit einem korrekten
    50                          ;	   Backlink versehen und der String-Heap wird 
    51                          ;	   durch die GC korrumpiert!
    52                          ;
    53                          ;	2) in backlinkarr bei blanext:
    54                          ;	   Muss zum Aufrufer immer mit Z=0 r�ckkehren,
    55                          ;	   und erkennt sonst immer nur das 1. Array!
    56                          ;	   Damit liegen die anderen Strings dann im 
    57                          ;	   freien Bereich und werden nach und nach 
    58                          ;	   �berschrieben!
    59                          ;
    60                          ; Optimierungen:
    61                          ;    
    62                          ;    #1 Schnellere Kopierroutine (+5 Byte Code, -2 T/Zeichen):
    63                          ;	Wegen des l�ngeren Codes und und einem deswegen l�nger werdenden
    64                          ;	Branch-Offset, wurde der Code ab cfinish zur�ckversetzt.
    65                          ;	Da im Teilbereich 1 nur noch 3 Bytes frei sind, muss auch
    66                          ;	noch an anderer Stelle eingespart werden.
    67                          ;	Dabei wird unmittelbar vor dem Kopieren des Strings nicht
    68                          ;	mehr explizit der Fall eines Strings mit L�nge 0 betrachtet,
    69                          ;	der an dieser Stelle auch nicht auftreten kann, da
    70                          ;	der erste Durchgang durch alle Strings am SDS, bei den
    71                          ;	Variablen und den Arrays Strings der L�nge 0 �bergeht. 
    72                          ;	Selbst, wenn jemand b�swilligerweise via allocate-Routine
    73                          ;	0-L�ngen-Speicher anfordert (was immer 2 Link-Bytes kostet),
    74                          ;	k�nnen diese Leerstring nicht referenziert werden. Im Zuge
    75                          ;	des zweiten Durchlaufs (Collection) w�rden diese degenerieren
    76                          ;	0-L�ngen-Strings auch wieder verschwinden.
    77                          ;
    78                          ;	Aktivierbar via use_fast_copy-Variable.
    79                          ;
    80                          ;    #2 Die L�ckenmarkierung (Low-Byte mit der L�nge) wird beim
    81                          ;	Kopieren des Strings mitgemacht. (-4 Byte Code, -5 T/String)
    82                          ;	Siehe no_opt_2-Abfrage bei Label cw3.
    83                          ;
    84                          ;    #3 Kein String-Kopieren durchf�hren, solange der String-Heap
    85                          ;	geordnet ist (also solange ptr = newptr ist). Sobald
    86                          ;	eine L�cke eliminiert wurde, laufen ptr und newptr auseinander.
    87                          ;
    88                          
    89                          ; Optimierung #1: Die optimierte Kopierroutine verwenden
    90                          ; aktiv
    91                          !set use_fast_copy=1
    92                          
    93                          ; Optimierung #2: L�ckmarkierung teilweise mit String-Kopieren mitmachen.
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
   109                          ; Variante: Etwas k�rzere und schnellere stralloc-Routine
   110                          ; Inaktiv, weil nicht verwendbar, da damit strptr nicht korrekt
   111                          ; ist und damit die "String concatenation" (String-Addition)
   112                          ; nicht funktionieren w�rde!
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
   123                          			; w�chst nach oben, max. 3 Elemente
   124                          			; zu je 3 Bytes.
   125                          sdsptr   = $16		; Zeiger auf n�chstes freie Element
   126                          			; des String-Descriptor-Stacks (SDS)
   127                          
   128                          vartab   = $2d		; Basicprogrammende = Variablenanfang
   129                          arytab   = $2f		; Variablenende = Array-Bereichanfang
   130                          strend   = $31		; Array-Bereichende = unterste String-Heap-Adresse 
   131                          fretop   = $33		; aktuelle String-Heap-Adresse
   132                          strptr	 = $35		; Hilfszeiger Stringzeiger (von stralloc)
   133                          memsiz   = $37		; h�chste RAM-Adresse f�r Basic, Start
   134                          			; des nach unten wachsenden String-Heaps
   135                          ; Hilfsvariablen
   136                          
   137                          ptr	 = $22		; Arbeitszeiger, alter Heap
   138                          newptr	 = $4e		; Neuer Stringzeiger, neuer Heap
   139                          desclen	 = $53		; akt. L�nge eines Stringdescriptors
   140                          aryptr	 = $58		; Array-Zeiger
   141                          descptr	 = $5f		; Descriptor-Zeiger
   142                          
   143                          getspa   = $b4f4	; "get space" ROM-Eintrittspunkt
   144                          garcoll  = $b526	; "garbage collect" ROM-Eintrittspunkt
   145                          
   146                          ; Vorbelegung der Speicherpl�tze
   147                          
   148                          basic    = $A000	; BASIC-ROM-Startadresse
   149                          romsize  = $2000	; ROM L�nge 8K
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
   171                          ; L�cken am Heap:
   172                          ;                      
   173                          ;   +-------------+   +--------------+
   174                          ;   V             |   V              |
   175                          ;    +-----------+---+---+---------+---+---+
   176                          ;    |L�CKE 2    |LEN|$FF|L�CKE 1  |LEN|$FF|
   177                          ;    +-----------+---+---+---------+---+---+
   178                          ;                  ^  ***            ^  ***
   179                          ;                  |   L�ckenmark.   |   L�ckenmarkierung
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
   191                          ;	in:	A		; L�nge anforderung
   192                          ;		fretop
   193                          ;	mod:	collected	; "GC aufgerufen"-Flag
   194                          ;		strptr		; temp. Zeiger
   195                          ;	out:	fretop		; Adresse auf String
   196                          ;		X,Y		; Adresse auf String
   197                          ;
   198                          ; Der String wird im Backlink stets als ungebrauchte L�cke
   199                          ; markiert! Dann muss die GC nur noch die Backlinks
   200                          ; der aktiven Strings setzen und kann die ungebrauchten
   201                          ; Strings �berspringen.
   202                          
   203                          !if .rom != 0 {			; ROM-Patch?
   204                          basicerror = $b4d2		; Basic-Fehlermeldung
   205                          } else {
   206                          basicerror:
   207                          	jmp $b4d2
   208                          }
   209                          
   210                          allocate:
   211                          	lsr collected		; Flag l�schen
   212                          retry	pha			; L�nge der Anforderung,
   213                          				; f�r 2. Teil
   214                          				; L�nge 0 m�glich, verbraucht aber 2 Bytes
   215                          	eor #$ff		; negieren
   216                          	sec
   217                          	adc fretop		; A/X = fretop; A/X -= L�nge
   218                          	ldx fretop+1
   219                          	bcs l1
   220                          	dex
   221                          	sec
   222                          l1	sbc #2			; A/X -= 2 Platz f�r Backlink einrechnen
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
   236                          	pla			; L�nge angeforderter Bereich
   237                          	jmp retry		; nochmal versuchen (Platz frei von GC?)
   238                          
   239                          alloc	jsr setfretop		; FRETOP = A/X
   240                          !if .rom != 0 {			; ROM-Patch?
   241                          	jmp stralloc		; zum 2. Teil: Allokation abschlie�en
   242                          } else {
   243                          	+part2_code		; 2. Teil gleich hier anh�ngen
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
   259                          ; Backlink aller tempor�ren Strings am String-Descriptor-Stack setzen
   260                          
   261                          sds:	lda #<sdsbase		; Startadr. String-Descriptor-Stack
   262                          	ldx #>sdsbase		; da in 0-Page, immer 0
   263                          	jsr setptr		; damit ptr setzen
   264                          
   265                          sdsnext	cmp sdsptr		; am 1. freien SDS-Element? (nur Low-Byte!)
   266                          	beq vars		; Ja, SDS durch, weiter mit Variablen
   267                          	jsr backlink		; sonst Backlink setzen
   268                          	beq sdsnext		; immer, weil High-Byte 0; n�chsten SDS-Descr.
   269                          
   270                          ; Backlink aller String-Variablen setzen
   271                          
   272                          vars:	lda #5			; Descriptor-Schritt f�r Variablen
   273                          	sta desclen
   274                          	lda vartab		; Variablenbeginn
   275                          	ldx vartab+1
   276                          	jsr setptr		; ptr = A/X
   277                          
   278                          varnext	cpx arytab+1		; Variablenende?
   279                          	bne varbl
   280                          	cmp arytab
   281                          	beq arrays		; ja, weiter mit Arrays
   282                          varbl	jsr backlinkvar		; Backlink f�r n�chste String-Variable setzen
   283                          	bne varnext		; immer; n�chsten Var.-Descriptor
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
   297                          arrbl	jsr backlinkarr		; Backlinks f�r n�chstes Array setzen -> Z=0!
   298                          	bne arrnext		; immer; n�chstes Array-Element
   299                          
   300                          
   301                          ; Ende, Zeiger zum neuen String-Heap �bernehmen
   302                          
   303                          cfinish
   304                          	lda newptr		; Aufger�umtzeiger ist ..
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
   321                          ; Aufr�umschleife
   322                          
   323                          cwnext	cpx fretop+1		; A/X: altes FRETOP erreicht,
   324                          	bne cwclean		; dann Heap durch und fertig.
   325                          	cmp fretop		; andernfalls aufr�umen ...
   326                          	beq cfinish		; fertig, weil A/X = FRETOP
   327                          
   328                          ; n�chsten String "aufr�umen" ...
   329                          
   330                          cwclean	sec			; Aufger�umtzeiger auf Backlink
   331                          	sbc #2
   332                          	bcs cw1
   333                          	dex			; A/X -> Backlink
   334                          
   335                          cw1	jsr setptr		; A/X -> ptr (Alt-String-Zeiger)
   336                          
   337                          	ldy #0
   338                          	lda (ptr),y		; Backlink low oder L�ckenl�nge
   339                          	iny			; Y=1
   340                          	tax			; -> X
   341                          	lda (ptr),y		; Backlink high
   342                          	cmp #$ff		; "String-nicht gebraucht"-Markierung
   343                          	bcc cwactive		; aktiver String
   344                          
   345                          	txa			; L�ckenl�nge
   346                          	eor #$ff		; negieren, C=1 (Komplement, +1)
   347                          	adc ptr			; (ptr - L�ckenl�nge)
   348                          	ldx ptr+1 
   349                          	bcs cwnext		; weiter mit n�chstem/r String/L�cke
   350                          	dex			; High Byte
   351                          
   352                          cw2	bne cwnext		; immer (Heap ist nie in Page 1)
   353                          				; weiter mit n�chstem/r String/L�cke
   354                          
   355                          ; einen aktiven String nach oben schieben
   356                          
   357                          cwactive			; immer mit Y=1 angesprungen
   358                          	sta descptr+1		; Descriptor-Adresse
   359                          	stx descptr 
   360                          
   361                          	lda newptr		; Aufger�umtzeiger -= 2
   362                          	sbc #1			; weil bereits C=0!
   363                          	sta newptr		; newptr -= 2
   364                          	bcs cw3
   365                          	dec newptr+1
   366                          	sec			; f�r SBC unten
   367                          
   368                          cw3	lda #$ff		; Backlink h: als L�cke markieren
   369                          	sta (newptr),y		; Y=1
   370                          	dey			; Y=0
   371                          !ifdef no_opt_2 {
   372                          	lda (descptr),y		; Descriptor: String-L�nge
   373                          	sta (newptr),y		; Backlink l: L�ckenl�nge
   374                          } else {
   375                          				; Backlink l: L�ckenl�nge sp�ter beim
   376                          				; Kopieren ...
   377                          }
   378                          	lda newptr		; Aufger�umtzeiger -= String-L�nge
   379                          	sbc (descptr),y		; minus String-L�nge, immer C=1, Y=0
   380                          	sta newptr
   381                          	bcs cw4
   382                          	dec newptr+1
   383                          	sec			; f�r SBC unten
   384                          
   385                          cw4	lda ptr			; Alt-String-Zeiger -= String-L�nge
   386                          	sbc (descptr),y		; immer C=1
   387                          	sta ptr			; Arbeitszeiger = alte String-Adresse
   388                          	bcs cw5
   389                          	dec ptr+1
   390                          cw5
   391                          	; An dieser Stelle w�re eine Optimierung m�glich, um das
   392                          	; Kopieren zu verhindern, wenn der String an der gleichen
   393                          	; Stelle bleibt - dabei darf die Optimierung #2 nicht
   394                          	; in Verwendung sein und es w�rden zus�tzlich 10 Bytes gebraucht!
   395                          !ifdef opt_no_copy {
   396                          	cmp newptr		; ptr bereits in A
   397                          	bne cw6			; ptr != newptr, also kopieren
   398                          	lda ptr+1		; High Byte ...
   399                          	cmp newptr+1
   400                          	beq cwheapordered	; ptr = newptr, nicht kopieren
   401                          cw6
   402                          }
   403                          
   404                          	lda (descptr),y		; String-L�nge
   405                          !ifndef use_fast_copy {
   406                          
   407                          				; immer, da L�nge >0
   408                          !ifdef no_opt_2 {
   409                          	beq cwnocopy		; falls doch L�nge 0, kein Kopieren,
   410                          				; Descriptor trotzdem anpassen ...
   411                          	tay			; als Index, mit Dekrementieren beginnen
   412                          } else { ; mit Optimierung #2
   413                          	tay			; L�nge als Index
   414                          	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   415                          				; mit L�ckenl�nge belegen
   416                          }
   417                          cwloop	dey			; -> Startindex f�rs Kopieren
   418                          	lda (ptr),y		; Arbeitszeiger mit altem String
   419                          cwbllen sta (newptr),y		; Aufger�umtzeiger mit neuem String-Ort
   420                          	tya			; Test auf Z-Flag!
   421                          	bne cwloop		; Index = 0 -> fertig kopiert
   422                          
   423                          } else { ; use_fast_copy!
   424                          
   425                          				; + 3 Byte, -2 T/Zeichen 
   426                          	tay			; L�nge als Index
   427                          !ifdef no_opt_2 {
   428                          	bne cwentry		; immer, da L�nge in Y>0, bei
   429                          				; Dekrementieren beginnen!
   430                          } else { ; mit Optimierung #2
   431                          	bne cwbllen		; immer, zuerst Backlink-Low-Markierung
   432                          				; mit L�ckenl�nge belegen
   433                          }
   434                          				; -> Startindex f�rs Kopieren
   435                          cwloop	lda (ptr),y		; Arbeitszeiger mit altem String
   436                          cwbllen	sta (newptr),y		; Aufger�umtzeiger mit neuem String-Ort
   437                          cwentry	dey			; Test auf Z-Flag!
   438                          	bne cwloop		; Index = 0 -> fertig kopiert
   439                          cwone	lda (ptr),y		; Arbeitszeiger mit altem String
   440                          	sta (newptr),y		; Aufger�umtzeiger mit neuem String-Ort
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
   470                          	lda (ptr),y		; String-L�nge
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
   481                          	cmp strend+1		; < Array-Bereichende (au�erhalb Heap)?
   482                          	bcc blnext		; ja, denn n�chsten String
   483                          	bne blsetdesc
   484                          	cpx strend 
   485                          	bcc blnext		; < Array-Bereichende (au�erhalb Heap)?
   486                          
   487                          blsetdesc:
   488                          	ldy #1
   489                          	lda ptr+1
   490                          	sta (newptr),y		; Descriptor-Adresse ...
   491                          	dey
   492                          	lda ptr
   493                          	sta (newptr),y		; in den Backlink �bertragen
   494                          
   495                          blnext	lda desclen		; n�chster String/n�chste Variable
   496                          	clc			; Schrittweite zum n�chsten Descriptor
   497                          	adc ptr			; ptr += desclen
   498                          	sta ptr
   499                          	bcc +
   500                          	inc ptr+1
   501                          +	ldx ptr+1		; immer != 0 -> Z=0 (au�er bei SDS, Z=1)
   502                          	rts
   503                          
   504                          ;**** N�chste String-Variable und Backlink setzen
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
   528                          	txa			; Variablentyp pr�fen
   529                          	bmi blnext		; keine String, n�chste Variable
   530                          	tya
   531                          	bmi backlink		; Backlink setzen
   532                          	bpl blnext		; keine String-Var., n�chste Variable
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
   543                          	jmp part4_continue	; nur f�r ROM-Patch
   544                          	; Dies �berspringt die Heap-Korrektur, f�r
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
   558                          	; (korrigiert den Heap um die Stringl�nge), aber
   559                          	; hier muss auch noch der Backlink entfernt
   560                          	; werden:
   561                          	; (Implementierung mit ADC statt INC braucht 1 Byte mehr Platz,
   562                          	;  wenngleich auch etwas schneller.)
   563                          LB6CC:
   564                          	sec			; L�nge in A, bereits am Stack
   565                          	adc $33			; $33/34 += A + 1 (Backlink 1. Teil)
   566                          	sta $33
   567                          	bcc +
   568                          	inc $34
   569                          +	inc $33			; $33/34 += 1 (Backlink 2. Teil)
   570                          	bne +
   571                          	inc $34
   572                          +	pla
   573                          	; ersetzt die folgende Heap-Korrektur, die nur
   574                          	; die String-L�nge ber�cksichtigt ...
   575                          ;B6CC: 18        CLC
   576                          ;B6CD: 65 33     ADC $33	; Heap-Top (bottom of string space)
   577                          ;B6CF: 85 33     STA $33	; um String-L�nge (in A)
   578                          ;B6D1: 90 02     BCC $B6D5	; erh�hen
   579                          ;B6D3: E6 34     INC $34
   580                          ;B6D5: 68        PLA
   581                          	jmp $B6D6		; zur�ck ins ROM
   582                          }
   583                          
   584                          ;**** N�chste Array-Variable und Backlink setzen
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
   597                          	php			; f�r sp�ter
   598                          	iny
   599                          	lda (ptr),y		; Variablenname 2. Zeichen
   600                          	tax			; f�r sp�ter
   601                          
   602                          	iny
   603                          	lda (ptr),y		; Offset n�chstes Array
   604                          	clc			; Bugfix 1: C=0 definiert setzen
   605                          	adc aryptr
   606                          !if .rom != 0 {			; ROM-Patch?
   607                          	jmp backlinkarr2	; nur f�r ROM-Patch
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
   618                          	!byte  0 		; Einschaltmeldung k�rzen
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
   646                          	beq blafinish		; Array fertig, Bugfix 2: Z-Flag l�schen!
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
   660                          ; f�r ROM-Patch:
   661                          ;--- $e4b7 - $e4d2 unused ($aa)
   662                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   663                          ;----              sonst Patch f�r andere Zwecke
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
   674                          ;	in: 	TOS		; L�nge
   675                          ;		fretop		; String-Adresse
   676                          ;	out:	fretop		; String-Adresse
   677                          ;		strptr		; String-Adresse (wird nicht verwendet)
   678                          ;				; (bei alternate_stralloc eventuell mit
   679                          ;				; inkrementiertem High-Byte)
   680                          ;		A		; L�nge
   681                          ;		X,Y		; String-Adresse (L,H)
   682                          ;	called:	allocate (in Fortsetzung)
   683                          
   684                            !ifndef alternate_stralloc {
   685                          stralloc:
   686                          	sta strptr		; strptr = A/X = FRETOP
   687                          	stx strptr+1
   688                          	tax			; A in X aufheben
   689                          	pla			; L�nge temp. vom Stack 
   690                          	pha			; wieder auf Stack, nun auch in A
   691                          	tay			; Index=L�nge (Backlink-position)
   692                          	sta (fretop),y		; Backlink L = String-/L�ckenl�nge
   693                          	iny			; Y=L�nge+1
   694                          	bne sa1			; wenn L�nge=255, dann
   695                          	inc fretop+1		; �berlauf, aber nur tempor�r!
   696                          
   697                          sa1	lda #$ff		; Backlink H = Markierung "L�cke"
   698                          	sta (fretop),y
   699                          	ldy strptr+1
   700                          	sty fretop+1		; �berlaufkorr. r�ckg�ngig
   701                          	pla			; L�nge vom Stack nehmen
   702                          	rts
   703                          
   704                            } else {
   705                          ; alternative, etwas k�rzere Variante (-3 T, -2 B),
   706                          ; aber NICHT VERWENDBAR!
   707                          
   708                          stralloc:
   709                          	sta strptr		; strptr = A/X = FRETOP
   710                          	stx strptr+1
   711                          	tax			; A in X aufheben
   712                          	pla			; L�nge temp. vom Stack 
   713                          	pha			; wieder auf Stack, nun auch in A
   714                          	tay			; Index=L�nge (Backlink-position)
   715                          	sta (strptr),y		; Backlink L = String-/L�ckenl�nge
   716                          	iny			; Y=L�nge+1
   717                          	bne sa1			; wenn L�nge=255, dann
   718                          	inc strptr+1		; �berlauf, aber nur tempor�r!
   719                          
   720                          sa1	lda #$ff		; Backlink H = Markierung "L�cke"
   721                          	sta (strptr),y
   722                          	ldy fretop+1		; in Y String-Adresse High-Byte
   723                          	pla			; L�nge vom Stack nehmen
   724                          	rts
   725                          				; Hier weicht strptr+1 u.U. von fretop+1 ab,
   726                          				; was aber ein Problem darstellt, da
   727                          				; es im BASIC-Interpreter ab $B68C eine
   728                          				; Stelle gibt, wo etwa im Zuge der
   729                          				; String-Addition die zusammengef�gten
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

; ******** Source: loader.asm
     1                          ;
     2                          ; *********** Loader
     3                          ;
     4                          ;       2013 11 10 johann e. klasek, johann at klasek at
     5                          ;
     6                          
     7                          ; --- Tempor�re Variablen:
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
    46                          	!error "Loader-Adresse stimmt nicht mit SYS-Adresse �berein!"
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
    90  084d 9151               	sta (dest),y		; �bertragen
    91  084f c8                 	iny
    92  0850 d004               	bne nohigh
    93  0852 e623               	inc ptr_h		; High Byte bei �berlauf
    94  0854 e652               	inc dest_h
    95                          nohigh
    96  0856 c64f               	dec len_l		; L�nge herunter
    97  0858 d0f1               	bne ploop		; z�hlen nach
    98  085a c650               	dec len_h		; dec 0/0 Methode
    99  085c d0ed               	bne ploop
   100  085e 8a                 	txa			; Index auf n�chsten Patch
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
   121  0875 e623               	inc ptr_h		; n�chste "Page"
   122  0877 ca                 	dex
   123  0878 d0f4               	bne toram
   124  087a 60                 	rts
   125                          

; ******** Source: garbcol.asm
   742                          
   743                          ;
   744                          ; Patch-Liste f�r "loader"
   745                          ;
   746                          
   747                          patchlist:
   748                          
   749  087b 95084501f4b4       !wo part1_real,part1_real_end-part1_real,part1
   750  0881 250a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
   751  0887 ef09360074e4       !wo part3_real,part3_real_end-part3_real,part3
   752  088d da091500c1b6       !wo part4_real,part4_real_end-part4_real,part4
   753  0893 0000               !wo 0  ; Endemarkierung
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
   207                           jmp $b4d2
   208                          
   209                          
   210                          allocate
   211  0895 460f                lsr collected 
   212  0897 48                 retry pha 
   213                           
   214                           
   215  0898 49ff                eor #$ff 
   216  089a 38                  sec
   217  089b 6533                adc fretop 
   218  089d a634                ldx fretop+1
   219  089f b002                bcs l1
   220  08a1 ca                  dex
   221  08a2 38                  sec
   222  08a3 e902               l1 sbc #2 
   223  08a5 b001                bcs l2
   224  08a7 ca                  dex
   225  08a8 e432               l2 cpx strend+1 
   226  08aa 9006                bcc checkcollect
   227  08ac d013                bne alloc 
   228  08ae c531                cmp strend 
   229  08b0 b00f                bcs alloc 
   230                          checkcollect
   231  08b2 a210                ldx #16 
   232  08b4 a50f                lda collected
   233  08b6 30bb                bmi basicerror 
   234  08b8 2026b5              jsr docollect 
   235  08bb 660f                ror collected 
   236  08bd 68                  pla 
   237  08be 4cf6b4              jmp retry 
   238                          
   239  08c1 2067b5             alloc jsr setfretop 
   240                          !if .rom != 0 { 
   241  08c4 4cbae4              jmp stralloc 
   242                          
   243                           +part2_code 
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
   261  08c7 a919               sds
   262  08c9 a200                ldx #>sdsbase 
   263  08cb 20a5e4              jsr setptr 
   264                          
   265  08ce c516               sdsnext cmp sdsptr 
   266  08d0 f005                beq vars 
   267  08d2 20e6b5              jsr backlink 
   268  08d5 f0f7                beq sdsnext 
   269                          
   270                          
   271                          
   272  08d7 a905               vars
   273  08d9 8553                sta desclen
   274  08db a52d                lda vartab 
   275  08dd a62e                ldx vartab+1
   276  08df 20a5e4              jsr setptr 
   277                          
   278  08e2 e430               varnext cpx arytab+1 
   279  08e4 d004                bne varbl
   280  08e6 c52f                cmp arytab
   281  08e8 f005                beq arrays 
   282  08ea 201db6             varbl jsr backlinkvar 
   283  08ed d0f3                bne varnext 
   284                          
   285                          
   286                          
   287                          arrays
   288  08ef 8558                sta aryptr 
   289  08f1 8659                stx aryptr+1 
   290  08f3 a003                ldy #3 
   291  08f5 8453                sty desclen
   292                          
   293  08f7 e432               arrnext cpx strend+1 
   294  08f9 d004                bne arrbl
   295  08fb c531                cmp strend
   296  08fd f00e                beq cleanwalk
   297  08ff 20c4b6             arrbl jsr backlinkarr 
   298  0902 d0f3                bne arrnext 
   299                          
   300                          
   301                          
   302                          
   303                          cfinish
   304  0904 a54e                lda newptr 
   305  0906 a64f                ldx newptr+1
   306                          setfretop
   307  0908 8533                sta fretop 
   308  090a 8634                stx fretop+1 
   309  090c 60                  rts 
   310                          
   311                          
   312                          
   313                          
   314                          
   315                          cleanwalk
   316  090d a537                lda memsiz 
   317  090f a638                ldx memsiz+1
   318  0911 854e                sta newptr 
   319  0913 864f                stx newptr+1 
   320                          
   321                          
   322                          
   323  0915 e434               cwnext cpx fretop+1 
   324  0917 d004                bne cwclean 
   325  0919 c533                cmp fretop 
   326  091b f0e7                beq cfinish 
   327                          
   328                          
   329                          
   330  091d 38                 cwclean sec 
   331  091e e902                sbc #2
   332  0920 b001                bcs cw1
   333  0922 ca                  dex 
   334                          
   335  0923 20a5e4             cw1 jsr setptr 
   336                          
   337  0926 a000                ldy #0
   338  0928 b122                lda (ptr),y 
   339  092a c8                  iny 
   340  092b aa                  tax 
   341  092c b122                lda (ptr),y 
   342  092e c9ff                cmp #$ff 
   343  0930 900c                bcc cwactive 
   344                          
   345  0932 8a                  txa 
   346  0933 49ff                eor #$ff 
   347  0935 6522                adc ptr 
   348  0937 a623                ldx ptr+1 
   349  0939 b0da                bcs cwnext 
   350  093b ca                  dex 
   351                          
   352  093c d0d7               cw2 bne cwnext 
   353                           
   354                          
   355                          
   356                          
   357                          cwactive 
   358  093e 8560                sta descptr+1 
   359  0940 865f                stx descptr 
   360                          
   361  0942 a54e                lda newptr 
   362  0944 e901                sbc #1 
   363  0946 854e                sta newptr 
   364  0948 b003                bcs cw3
   365  094a c64f                dec newptr+1
   366  094c 38                  sec 
   367                          
   368  094d a9ff               cw3 lda #$ff 
   369  094f 914e                sta (newptr),y 
   370  0951 88                  dey 
   371                          !ifdef no_opt_2 {
   372                           lda (descptr),y 
   373                           sta (newptr),y 
   374                          
   375                           
   376                           
   377                          
   378  0952 a54e                lda newptr 
   379  0954 f15f                sbc (descptr),y 
   380  0956 854e                sta newptr
   381  0958 b003                bcs cw4
   382  095a c64f                dec newptr+1
   383  095c 38                  sec 
   384                          
   385  095d a522               cw4 lda ptr 
   386  095f f15f                sbc (descptr),y 
   387  0961 8522                sta ptr 
   388  0963 b002                bcs cw5
   389  0965 c623                dec ptr+1
   390                          cw5
   391                           
   392                           
   393                           
   394                           
   395                          !ifdef opt_no_copy {
   396                           cmp newptr 
   397                           bne cw6 
   398                           lda ptr+1 
   399                           cmp newptr+1
   400                           beq cwheapordered 
   401                          cw6
   402                          
   403                          
   404  0967 b15f                lda (descptr),y 
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
   426  0969 a8                  tay 
   427                          !ifdef no_opt_2 {
   428                           bne cwentry 
   429                           
   430                          
   431  096a d002                bne cwbllen 
   432                           
   433                          
   434                           
   435  096c b122               cwloop lda (ptr),y 
   436  096e 914e               cwbllen sta (newptr),y 
   437  0970 88                 cwentry dey 
   438  0971 d0f9                bne cwloop 
   439  0973 b122               cwone lda (ptr),y 
   440  0975 914e                sta (newptr),y 
   441                          
   442                          
   443                          cwnocopy
   444                           
   445  0977 c8                  iny 
   446  0978 a54e                lda newptr 
   447  097a 915f                sta (descptr),y 
   448  097c c8                  iny 
   449  097d a54f                lda newptr+1
   450  097f 915f                sta (descptr),y 
   451                          
   452                          cwheapordered
   453  0981 a522                lda ptr
   454  0983 a623                ldx ptr+1 
   455  0985 d08e                bne cwnext 
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
   469  0987 a000                ldy #0
   470  0989 b122                lda (ptr),y 
   471  098b f023                beq blnext 
   472  098d c8                  iny
   473  098e 18                  clc
   474  098f 7122                adc (ptr),y 
   475  0991 854e                sta newptr 
   476  0993 aa                  tax
   477  0994 c8                  iny
   478  0995 b122                lda (ptr),y
   479  0997 6900                adc #0
   480  0999 854f                sta newptr+1 
   481  099b c532                cmp strend+1 
   482  099d 9011                bcc blnext 
   483  099f d004                bne blsetdesc
   484  09a1 e431                cpx strend 
   485  09a3 900b                bcc blnext 
   486                          
   487                          blsetdesc
   488  09a5 a001                ldy #1
   489  09a7 a523                lda ptr+1
   490  09a9 914e                sta (newptr),y 
   491  09ab 88                  dey
   492  09ac a522                lda ptr
   493  09ae 914e                sta (newptr),y 
   494                          
   495  09b0 a553               blnext lda desclen 
   496  09b2 18                  clc 
   497  09b3 6522                adc ptr 
   498  09b5 8522                sta ptr
   499  09b7 9002                bcc +
   500  09b9 e623                inc ptr+1
   501  09bb a623               + ldx ptr+1 
   502  09bd 60                  rts
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
   514  09be a000                ldy #0
   515  09c0 b122                lda (ptr),y 
   516  09c2 aa                  tax 
   517  09c3 c8                  iny
   518  09c4 b122                lda (ptr),y 
   519  09c6 a8                  tay 
   520                          
   521  09c7 a902                lda #2 
   522  09c9 18                  clc
   523  09ca 6522                adc ptr 
   524  09cc 8522                sta ptr
   525  09ce 9002                bcc +
   526  09d0 e623                inc ptr+1
   527                          +
   528  09d2 8a                  txa 
   529  09d3 30db                bmi blnext 
   530  09d5 98                  tya
   531  09d6 30af                bmi backlink 
   532  09d8 10d6                bpl blnext 
   533                          

; ******** Source: garbcol.asm
   767                          }
   768                          part1_real_end
   769                          
   770                          	; Codebereich 1: darf den zur Verf�gung stehenden Bereich nicht �berschreiten!
   771                          	!set part1_end = (part1_real_end-part1_real)+part1
   772                          	!if ( part1_end > $B63D ) {
   773                          		!error "Code-Teil 1 ist zu lang! ",part1,"-",part1_end
   774                          	}
   775                          
   776                          part4_real
   777                          !pseudopc part4_rom {
   778                          part4:

; ******** Source: garbcol.asm, macro: part4_code
   540                          .rom
   541                          
   542                          !if .rom != 0 { 
   543  09da 4cd6b6              jmp part4_continue 
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
   564                           sec 
   565                           adc $33 
   566                           sta $33
   567                           bcc +
   568                           inc $34
   569                          + inc $33 
   570                           bne +
   571                           inc $34
   572                          + pla
   573                           
   574                           
   575                          
   576                          
   577                          
   578                          
   579                          
   580                          
   581                           jmp $B6D6 
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
   595  09dd a000                ldy #0
   596  09df b122                lda (ptr),y 
   597  09e1 08                  php 
   598  09e2 c8                  iny
   599  09e3 b122                lda (ptr),y 
   600  09e5 aa                  tax 
   601                          
   602  09e6 c8                  iny
   603  09e7 b122                lda (ptr),y 
   604  09e9 18                  clc 
   605  09ea 6558                adc aryptr
   606                          !if .rom != 0 { 
   607  09ec 4c75e4              jmp backlinkarr2 
   608                           
   609                          

; ******** Source: garbcol.asm
   780                          !if * > part4_continue {
   781                          	!error "part4 ist zu lang!"
   782                          }
   783                          }
   784                          part4_real_end
   785                          
   786                          part3_real
   787                          !pseudopc part3_rom {
   788                          part3:

; ******** Source: garbcol.asm, macro: part3_code
   615                          .rom
   616                          
   617                          !if .rom != 0 { 
   618  09ef 00                  !byte 0 
   619                          
   620                          
   621                          backlinkarr2
   622  09f0 8558                sta aryptr 
   623  09f2 c8                  iny
   624  09f3 b122                lda (ptr),y
   625  09f5 6559                adc aryptr+1 
   626  09f7 8559                sta aryptr+1 
   627                          
   628  09f9 28                  plp 
   629  09fa 3020                bmi blaskip 
   630  09fc 8a                  txa
   631  09fd 101d                bpl blaskip 
   632                          
   633  09ff c8                  iny 
   634  0a00 b122                lda (ptr),y 
   635  0a02 0a                  asl 
   636  0a03 6905                adc #5 
   637  0a05 6522                adc ptr 
   638  0a07 8522                sta ptr 
   639  0a09 9002                bcc bla1
   640  0a0b e623                inc ptr+1 
   641  0a0d a623               bla1 ldx ptr+1 
   642                          
   643  0a0f e459               blanext cpx aryptr+1 
   644  0a11 d004                bne blaset 
   645  0a13 c558                cmp aryptr
   646  0a15 f007                beq blafinish 
   647                          blaset
   648  0a17 20e6b5              jsr backlink 
   649  0a1a d0f3                bne blanext 
   650                          
   651                          blaskip
   652  0a1c a558                lda aryptr 
   653                          blafinish
   654  0a1e a659                ldx aryptr+1 
   655                          
   656  0a20 8522               setptr sta ptr 
   657  0a22 8623                stx ptr+1
   658  0a24 60                  rts 
   659                          
   660                          
   661                          
   662                          
   663                          
   664                          

; ******** Source: garbcol.asm
   790                          }
   791                          part3_real_end
   792                          
   793                          part2_real
   794                          !pseudopc part2_rom {
   795                          part2:

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
   686  0a25 8535                sta strptr 
   687  0a27 8636                stx strptr+1
   688  0a29 aa                  tax 
   689  0a2a 68                  pla 
   690  0a2b 48                  pha 
   691  0a2c a8                  tay 
   692  0a2d 9133                sta (fretop),y 
   693  0a2f c8                  iny 
   694  0a30 d002                bne sa1 
   695  0a32 e634                inc fretop+1 
   696                          
   697  0a34 a9ff               sa1 lda #$ff 
   698  0a36 9133                sta (fretop),y
   699  0a38 a436                ldy strptr+1
   700  0a3a 8434                sty fretop+1 
   701  0a3c 68                  pla 
   702  0a3d 60                  rts
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

; ******** Source: garbcol.asm
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
   820                                  lda #memrom
   821                                  sta prozport		; alles ROM (also vom ROM kopieren)
   822                          
   823                                  ldy #<basic		; ROM-Beginn
   824                                  sty ptr
   825                                  lda #>basic     
   826                                  sta ptr+1		; BASIC-ROM Anfang
   827                                  ldx #>(romsize)		; BASIC-ROM L�nge in Pages
   828                          cpyrom  lda (ptr),y		; ROM lesen
   829                                  sta (ptr),y		; RAM schreiben
   830                                  iny
   831                                  bne cpyrom
   832                                  inc ptr+1		; n�chste Page
   833                                  dex			; Page-Z�hler
   834                                  bne cpyrom
   835                          
   836                                  lda prozport		; auf RAM umschalten
   837                                  and #%11111110		; "BASIC-ROM aus"-Maske
   838                                  sta prozport
   839                          
   840                                  lda #<docollect		; "jmp docollect"
   841                                  sta garcoll+1		; patchen ...
   842                                  lda #>docollect
   843                                  sta garcoll+2
   844                          
   845                                  lda #<allocate		; "jmp allocate"
   846                                  sta getspa+1		; patchen ...
   847                                  lda #>allocate
   848                                  sta getspa+2
   849                          
   850                                  lda #<LB6CC		; "jmp LB6CC"
   851                                  sta $b6cc+1		; patchen ...
   852                                  lda #>LB6CC
   853                                  sta $b6cc+2
   854                          
   855                                  lda #$4c		; JMP-Opcode
   856                                  sta garcoll
   857                                  sta getspa
   858                                  sta $b6cc
   859                          	rts
   860                          
   861                          	; Code-Teile zustammenstellen, sind nun unmittelbar hintereinander ..
   862                          
   863                          	+part1_code 0		; ohne ROM-Spezialit�ten, da ist part2_code
   864                          				; inkludiert!
   865                          	+part4_code 0		; ohne ROM-Spezialit�ten
   866                          	+part3_code 0		; ohne ROM-Spezialit�ten
   867                          
   868                          
   869                          }
   870                          
