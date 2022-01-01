
; ******** Source: garbcol.asm
     1                          !to "garbcol.o",cbm	
     2                          ;
     3                          ;  **** garbage collection ****
     4                          ;
     5                          ; 64er, oct 1988
     6                          
     7                          ;
     8                          ; ueberarbeitetet und korrigiert:
     9                          ;	2013 11 10 johann e. klasek, johann at klasek at
    10                          ;
    11                          ; bugfixes:
    12                          ;
    13                          
    14                          
    15                          ; basic-zeiger und -konstanten
    16                          
    17                          sdsbase  = $0019	; 1. element stringdescriptorstack
    18                          			; waechst nach oben, max. 3 elemente
    19                          			; zu je 3 bytes.
    20                          sdsptr   = $16		; zeiger auf naechstes freie element
    21                          			; des stringdescriptorstacks
    22                          
    23                          vartab   = $2d		; basicprogrammende = variablenanfang
    24                          arytab   = $2f		; variablenende = arraybereichanfang
    25                          strend   = $31		; arraybereichende = unterste stringheap adresse 
    26                          fretop   = $33		; aktuelle stringheap adresse
    27                          memsiz   = $37		; hoechste ram-adresse fuer basic, start
    28                          			; des nach unten wachsenden stringheaps
    29                          
    30                          garcoll  = $b526
    31                          
    32                          ; vorbelegung der speicherplaetze
    33                          
    34                          romsize  = $2000	; rom laenge 8k
    35                          
    36                          prozport = $01		; prozessorport
    37                          memrom = %00110111	; basic+kernal rom
    38                          membas = %00110110	; basic ram+kernal rom
    39                          memram = %00110101	; basic+kernal ram
    40                          
    41                          

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
    43                          
    44                          
    45                          ;
    46                          ; Patch-Liste
    47                          ;
    48                          
    49                          patchlist:
    50                          
    51  087b 95084701f4b4       !wo part1_real,part1_real_end-part1_real,part1
    52  0881 260a1900bae4       !wo part2_real,part2_real_end-part2_real,part2
    53  0887 f009360074e4       !wo part3_real,part3_real_end-part3_real,part3
    54  088d dc091400c1b6       !wo part4_real,part4_real_end-part4_real,part4
    55  0893 0000               !wo 0  ; Endemarkierung
    56                          
    57                          
    58                          
    59                          part1_real:
    60                          
    61                          !pseudopc $b4f4 {
    62                          
    63                          part1:
    64                          
    65                          ;***** speicher von stringheap anfordern
    66                          ;
    67                          ;	in:	a			; länge anforderung
    68                          ;		$33/34			; fretop
    69                          ;	mod:	$0f			; gc aufgerufen flag
    70                          ;		$35/36			; temp. zeiger
    71                          ;	out:	$33/34			; fretop
    72                          
    73  0895 460f                       lsr $0f		; set not collected state
    74  0897 48                         pha		; länge der anforderung,
    75                          			; für 2. teil
    76  0898 49ff                       eor #$ff		; negieren
    77  089a 38                         sec
    78  089b 6533                       adc $33
    79  089d a634                       ldx $34
    80  089f b002                       bcs $b502
    81  08a1 ca                         dex
    82  08a2 38                         sec
    83  08a3 e902                       sbc #$02		; platz für backlink einrechnen
    84  08a5 b001                       bcs $b507
    85  08a7 ca                         dex
    86  08a8 e432                       cpx $32		; stringheap voll (arraybereich ende)?
    87  08aa 9006                       bcc $b511
    88  08ac d013                       bne $b520		; nein, bereich anfordern
    89  08ae c531                       cmp $31
    90  08b0 b00f                       bcs $b520		; nein, bereich anfordern
    91  08b2 a210                       ldx #$10
    92  08b4 a50f                       lda $0f
    93  08b6 30bb                       bmi $b4d2		; collection schon gelaufen?
    94  08b8 2026b5                     jsr $b526		; nein, dann garbage collection, c=1 (immer!)
    95  08bb 660f                       ror $0f		; mark collected state, bit7 gesetzt
    96  08bd 68                         pla		; länge angeforderter bereich
    97  08be 4cf6b4                     jmp $b4f6		; nochmal versuchen
    98                          
    99  08c1 2077b5                     jsr $b577		; fretop = a/x
   100  08c4 4cbae4                     jmp $e4ba		; allocate final
   101                          
   102                          ;***** garbage collection
   103                          
   104                          ; backlink aller stringdescriptorstack strings setzen
   105                          
   106  08c7 a919                       lda #$19		; start adr. string descriptor stack
   107  08c9 a200                       ldx #$00
   108  08cb 20a5e4                     jsr $e4a5		; set $22/$23
   109                          
   110  08ce c516                       cmp $16		; at 1. free sds element?
   111  08d0 f005                       beq $b536		; sds done
   112  08d2 20e8b5                     jsr $b5e8		; backlink setzen
   113  08d5 f0f7                       beq $b52d		; unbedingt
   114                          
   115                          ; backlink aller stringvariablen setzen
   116                          
   117  08d7 a905                       lda #$05		; descriptor-schritt bei variablen
   118  08d9 8553                       sta $53
   119  08db a52d                       lda $2d		; variablenbeginn
   120  08dd a62e                       ldx $2e
   121  08df 20a5e4                     jsr $e4a5		; 22/23 = a/x
   122                          
   123  08e2 e430                       cpx $30		; variablenende?
   124  08e4 d004                       bne $b549
   125  08e6 c52f                       cmp $2f
   126  08e8 f005                       beq $b54e		; ja, weiter mit arrays
   127  08ea 201fb6                     jsr $b61f		; backlink für nächste stringvariable setzen
   128  08ed d0f3                       bne $b541		; unbedingt
   129                          
   130                          ; backlink aller stringarrays setzen
   131                          
   132  08ef 8558                       sta $58		; variablenbereichende = arraybereichanfang
   133  08f1 8659                       stx $59
   134  08f3 a003                       ldy #$03		; descriptor-schritt bei stringarrays
   135  08f5 8453                       sty $53
   136                          
   137  08f7 e432                       cpx $32		; arraybereichende?
   138  08f9 d004                       bne $b55e
   139  08fb c531                       cmp $31
   140  08fd f005                       beq $b563
   141  08ff 20c4b6                     jsr $b6c4		; backlinks für nächstes stringarray setzen
   142  0902 d0f3                       bne $b556		; unbedingt.
   143                          
   144  0904 a537                       lda $37		; memtop
   145  0906 a638                       ldx $38
   146  0908 854e                       sta $4e		; -> aufgeräumtzeiger
   147  090a 864f                       stx $4f
   148                          
   149                          ; aufräumschleife
   150                          
   151  090c e434                       cpx $34		; a/x: altes fretop erreicht,
   152  090e d00d                       bne $b57c		; dann heap durch und fertig.
   153  0910 c533                       cmp $33		; sonst aufräumen ...
   154  0912 d009                       bne $b57c
   155  0914 a54e                       lda $4e		; aufgeräumtzeiger ist
   156  0916 a64f                       ldx $4f
   157  0918 8533                       sta $33		; neues fretop
   158  091a 8634                       stx $34
   159  091c 60                         rts		; fertig!
   160                          
   161                          ; nächsten string "aufräumen" ...
   162                          
   163  091d 38                         sec		; aufräumtzeiger auf backlink
   164  091e e902                       sbc #$02
   165  0920 b001                       bcs $b582
   166  0922 ca                         dex		; a/x -> backlink
   167  0923 20a5e4                     jsr $e4a5		; a/x -> 22/23 (arbeitszeiger)
   168  0926 a000                       ldy #$00
   169  0928 b122                       lda ($22),y	; backlink low oder lückenlänge
   170  092a c8                         iny
   171  092b aa                         tax		; -> x
   172  092c b122                       lda ($22),y	; backlink high
   173  092e c9ff                       cmp #$ff		; string "nicht gebraucht" markierung
   174  0930 900d                       bcc $b59e		; aktiver string
   175  0932 8a                         txa		; lückenlänge
   176  0933 49ff                       eor #$ff		; negieren
   177  0935 6522                       adc $22		; 22/23 - lückenlänge
   178  0937 a623                       ldx $23
   179  0939 b001                       bcs $b59b		; korr: geht auch gleich nach $b56b
   180  093b ca                         dex		; 
   181  093c 4c6bb5                     jmp $b56b		; korr: bne $b56b (sollte nie 0 werden können!)
   182                          
   183                          ; aktiven string nach oben schieben
   184                          
   185  093f 8560                       sta $60		; descriptor-adresse
   186  0941 865f                       stx $5f
   187  0943 a54e                       lda $4e		; aufgeräumtzeiger -= 2
   188  0945 e901                       sbc #$01		; weil c=0!
   189  0947 854e                       sta $4e
   190  0949 b003                       bcs $b5ad
   191  094b c64f                       dec $4f
   192  094d 38                         sec		; y=1
   193                          
   194  094e a9ff                       lda #$ff		; backlink h: als lücke markieren
   195  0950 914e                       sta ($4e),y	; 
   196  0952 88                         dey		; y=0
   197  0953 b15f                       lda ($5f),y	; descriptor: stringlänge
   198  0955 914e                       sta ($4e),y	; backlink l: lückenlänge
   199                          
   200  0957 a54e                       lda $4e		; aufgeräumtzeiger -= stringlänge
   201  0959 f15f                       sbc ($5f),y	; immer c=1
   202  095b 854e                       sta $4e
   203  095d b003                       bcs $b5c1
   204  095f c64f                       dec $4f
   205  0961 38                         sec
   206                          
   207  0962 c8                         iny		; y=1
   208  0963 915f                       sta ($5f),y	; stringadresse l: neue adresse
   209  0965 c8                         iny		; y=2
   210  0966 a54f                       lda $4f
   211  0968 915f                       sta ($5f),y	; stringadresse h: neue adresse
   212  096a a000                       ldy #$00
   213  096c a522                       lda $22
   214  096e f15f                       sbc ($5f),y	; immer c=1
   215  0970 8522                       sta $22		; arbeitszeiger = alte stringadresse
   216  0972 b002                       bcs $b5d5
   217  0974 c623                       dec $23
   218                          
   219  0976 b15f                       lda ($5f),y	; stringlänge=0?
   220  0978 f009                       beq $b5e2		; ja, dann nicht kopieren
   221  097a a8                         tay		; länge-1
   222                          
   223  097b 88                         dey		; -> startindex fürs kopieren
   224  097c b122                       lda ($22),y	; arbeitszeiger mit altem string
   225  097e 914e                       sta ($4e),y	; aufgeräumtzeiger mit neuem stringort
   226  0980 98                         tya		; z-flag!
   227  0981 d0f8                       bne $b5da		; index 0 -> fertig kopiert
   228                          
   229  0983 a522                       lda $22
   230  0985 a623                       ldx $23
   231  0987 d083                       bne $b56b		; unbedingt, weiter in schleife
   232                          
   233                          
   234                          ; backlink setzen
   235                          ;
   236                          ; 	in:	22/23	descriptoradresse
   237                          ; 	out:	22/23	descriptoradresse
   238                          ;		a/x
   239                          ;	destroy: 4e/4f
   240                          ;	called:	$b531, $b637
   241                          
   242  0989 a000                       ldy #$00
   243  098b b122                       lda ($22),y	; stringlänge
   244  098d f023                       beq $b611		; fertig, wenn 0
   245  098f c8                         iny
   246  0990 18                         clc
   247  0991 7122                       adc ($22),y	; backlinkposition (am stringende)
   248  0993 854e                       sta $4e		; backlink-zeiger l
   249  0995 aa                         tax
   250  0996 c8                         iny
   251  0997 b122                       lda ($22),y
   252  0999 6900                       adc #$00
   253  099b 854f                       sta $4f		; backlink-zeiger h
   254  099d c532                       cmp $32		; < arraybereichende (außerhalb heap)?
   255  099f 9011                       bcc $b611		; ja, denn nächsten string
   256  09a1 d004                       bne $b606
   257  09a3 e431                       cpx $31
   258  09a5 900b                       bcc $b611		; < arraybereichende (außerhalb heap)?
   259                          
   260  09a7 a001                       ldy #$01
   261  09a9 a523                       lda $23
   262  09ab 914e                       sta ($4e),y	; descriptoradresse ...
   263  09ad 88                         dey
   264  09ae a522                       lda $22
   265  09b0 914e                       sta ($4e),y	; in den backlink übertragen
   266                          
   267  09b2 18                         clc		; nächster string/nächste variable
   268  09b3 a553                       lda $53		; schrittweite zum nächsten
   269  09b5 6522                       adc $22		; descriptor ...
   270  09b7 8522                       sta $22
   271  09b9 9002                       bcc $b61c
   272  09bb e623                       inc $23
   273  09bd a623                       ldx $23
   274  09bf 60                         rts
   275                          
   276                          ; nächste stringvariable und backlink setzen
   277                          ;
   278                          ; 	in: 22/23	variablenadresse
   279                          ; 	out:	22/23	variablenadresse
   280                          ;		a/x
   281                          ;	destroy: 4e/4f
   282                          ;	called: $b549
   283                          
   284  09c0 a000                       ldy #$00
   285  09c2 b122                       lda ($22),y	; variablenname 1. zeichen
   286  09c4 aa                         tax
   287  09c5 c8                         iny
   288  09c6 b122                       lda ($22),y	; variablenname 2. zeichen
   289  09c8 a8                         tay
   290  09c9 18                         clc
   291  09ca a522                       lda $22		; descriptoradresse (in variable)
   292  09cc 6902                       adc #$02
   293  09ce 8522                       sta $22
   294  09d0 9002                       bcc $b633
   295  09d2 e623                       inc $23
   296  09d4 8a                         txa		; variablen typ prüfen
   297  09d5 30db                       bmi $b611		; keine string, nächste variable
   298  09d7 98                         tya
   299  09d8 30af                       bmi $b5e8		; backlink setzen
   300  09da 10d6                       bpl $b611		; keine stringvar., nächste variable
   301                          
   302                          }
   303                          part1_real_end
   304                          
   305                          part4_real
   306                          !pseudopc $b6c1 {
   307                          
   308                          part4:
   309                          
   310  09dc 4cd6b6                     jmp $b6d6
   311                          
   312                          ; nächste array variable und backlink setzen
   313                          ;
   314                          ; 	in: 22/23	arrayadresse
   315                          ; 	out:	22/23	adresse folge-array
   316                          ;		58/59	adresse folge-array
   317                          ;		a/x	adresse folge-array
   318                          ;	destroy: 4e/4f
   319                          ;	called: $b55e
   320                          
   321  09df a000                       ldy #$00
   322  09e1 b122                       lda ($22),y	; variablenname 1. zeichen
   323  09e3 08                         php		; für später
   324  09e4 c8                         iny
   325  09e5 b122                       lda ($22),y	; variablenname 2. zeichen
   326  09e7 aa                         tax		; für später
   327  09e8 c8                         iny
   328  09e9 b122                       lda ($22),y	; offset nächstes array
   329                          					; bug: clc fehlt!
   330  09eb 6558                       adc $58
   331  09ed 4c75e4                     jmp $e475
   332                          }
   333                          part4_real_end
   334                          
   335                          
   336                          part3_real
   337                          !pseudopc $e474 {
   338                          
   339                          part3:
   340                          
   341  09f0 00                         brk		; einschaltmeldung kürzen
   342                          
   343  09f1 8558                       sta $58		; folge-array l
   344  09f3 c8                         iny
   345  09f4 b122                       lda ($22),y
   346  09f6 6559                       adc $59
   347  09f8 8559                       sta $59		; folge-array h
   348  09fa 28                         plp		; arraytyp:
   349  09fb 3020                       bmi $e4a1		; kein stringarray
   350  09fd 8a                         txa
   351  09fe 101d                       bpl $e4a1		; kein stringarray
   352  0a00 c8                         iny		; y=4
   353  0a01 b122                       lda ($22),y	; anzahl der dimensionen
   354  0a03 0a                         asl 		; *2
   355  0a04 6905                       adc #$05		; + 5 (var.name+offset+dimensionen)
   356  0a06 6522                       adc $22		; auf 1. element ...
   357  0a08 8522                       sta $22
   358  0a0a 9002                       bcc $e492
   359  0a0c e623                       inc $23
   360  0a0e a623                       ldx $23		; positionieren
   361                          
   362  0a10 e459                       cpx $59		; arrayende erreicht?
   363  0a12 d004                       bne $e49c		; nein, backlink setzen
   364  0a14 c558                       cmp $58
   365  0a16 f009                       beq $e4a5		; array durch
   366                          
   367  0a18 20e8b5                     jsr $b5e8		; backlink setzen
   368  0a1b d0f3                       bne $e494		; unbedingt
   369                          
   370  0a1d a558                       lda $58		; arrayzeiger
   371  0a1f a659                       ldx $59
   372  0a21 8522                       sta $22		; arbeitszeiger
   373  0a23 8623                       stx $23
   374  0a25 60                         rts
   375                          
   376                          ;--- $e4b7 - $e4d2 unused ($aa)
   377                          ;--- $e4d3 - $e4d9 unused ($aa) bei altem kernal,
   378                          ;----              patch for rs232-routines
   379                          
   380                          }
   381                          part3_real_end
   382                          
   383                          
   384                          part2_real
   385                          !pseudopc $e4ba {
   386                          
   387                          part2:
   388                          
   389                          ;**** string allocation (fortsetzung)
   390                          ;
   391                          ;	in: 	tos			; länge
   392                          ;		33/34			; stringadresse
   393                          ;	out:	a			; länge
   394                          ;		35/36			; stringadresse
   395                          ;	called:	$b523
   396                          
   397  0a26 8535                       sta $35		; 35/36 = a/x = 33/34
   398  0a28 8636                       stx $36
   399  0a2a aa                         tax		; a in x aufheben
   400  0a2b 68                         pla		; länge
   401  0a2c 48                         pha		; wieder auf stack
   402  0a2d a8                         tay		; index=länge (backlink-position)
   403  0a2e 9133                       sta ($33),y	; backlink l = string-/lückenlänge
   404  0a30 c8                         iny		; y=len+1
   405  0a31 d002                       bne $e4c9		; wenn länge=255, dann
   406  0a33 e634                       inc $34		; Überlauf, aber nur temporär!
   407                          
   408  0a35 a9ff                       lda #$ff		; backlink h = markierung "lücke"
   409  0a37 9133                       sta ($33),y
   410  0a39 a436                       ldy $36
   411  0a3b 8434                       sty $34		; Überlaufkorr. rückgänig
   412  0a3d 68                         pla		; länge vom stack
   413  0a3e 60                         rts
   414                          
   415                          }
   416                          part2_real_end
   417                          
