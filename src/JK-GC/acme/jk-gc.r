
; ******** Source: jk-gc.asm
     1                          *= $CDEF
     2                          !to "jk-gc.o", cbm	; set output file and format
     3                          ;
     4                          ;.OPT LIST,NOSYM
     5                          ;
     6                          ; *************************
     7                          ; *  GARBAGE  COLLECTION  *
     8                          ; *   VON JOHANN KLASEK   *
     9                          ; * 1985-12-27 VERS. 1.1  *
    10                          ; *************************
    11                          ;
    12                          ; AUFRUF: SYS ...
    13                          ; RÄUMT STRINGSPEICHER AUF
    14                          ; ES WERDEN NUR JENE SPEICHERSTELLEN
    15                          ; BENUTZT, DIE AUCH DIE NORMALE
    16                          ; GC VERWENDET; ALLE ANDEREN
    17                          ; WERDEN WIEDER RESTAURIERT.
    18                          
    19                          STRPTR = $33	; STRING POINTER = FRETOP
    20                          BUFPTR = $5F	; BUFFER POINTER
    21                          STRADR = $58	; STRING ADDRESS
    22                          STRDP  = $22	; STRING DESCRIPTOR ADDRESS
    23                          MEMBEG = $31	; MEMORY BEGINN = STREND
    24                          
    25                          MEMEND = $37	; MEMORY END
    26                          BERANF = $FB	; BEREICHSANFANG
    27                          BEREND = $FD	; BEREICHSENDE
    28                          FLAG   = $53	; ENDE-FLAG
    29                          LEN    = $4E	; STRING LENGTH
    30                          STAT   = $4F	; STRING STATE
    31                          PTR    = $50	; ARRAY POINTER
    32                          
    33                          TOSS   = $19	; TOP OF STRINGSTACK
    34                          EOSS   = $22	; END OF STRIKNGSTACK +1
    35                          TSSP   = $16	; TEMP. STRINGSTACK POINTER
    36                          
    37                          FRETOP = $33	; STRINGS START
    38                          STREND = $31	; ARRAYS END +1
    39                          ARYTAB = $2F	; ARRAY START
    40                          VARTAB = $2D	; VARIABLES START
    41                          
    42                          
    43                          CPTR   = $FB
    44                          
    45                          INIT
    46  cdef a937               	LDA #$37
    47  cdf1 8501               	STA $01		; ALLES ROM
    48  cdf3 a000               	LDY #0
    49  cdf5 84fb               	STY CPTR
    50  cdf7 a9a0               	LDA #$A0
    51  cdf9 85fc               	STA CPTR+1	; BASIC-ROM ANFANG
    52  cdfb a220               	LDX #>($2000)	; BASIC-ROM LÄNGE
    53  cdfd b1fb               CPYROM	LDA (CPTR),Y	; ROM LESEN
    54  cdff 91fb               	STA (CPTR),Y	; RAM SCHREIBEN
    55  ce01 c8                 	INY
    56  ce02 d0f9               	BNE CPYROM
    57  ce04 e6fc               	INC CPTR+1
    58  ce06 ca                 	DEX
    59  ce07 d0f4               	BNE CPYROM
    60  ce09 a501               	LDA $01
    61  ce0b 29fe               	AND #%11111110	; BASIC-ROM AUS MASKE
    62  ce0d 8501               	STA $01
    63  ce0f a91f               	LDA #<COLLECT	; JMP COLLECT
    64  ce11 8d27b5             	STA $B527	; PATCHEN ...
    65  ce14 a9ce               	LDA #>COLLECT
    66  ce16 8d28b5             	STA $B528
    67  ce19 a94c               	LDA #$4C	; JMP-OPCODE
    68  ce1b 8d26b5             	STA $B526
    69  ce1e 60                 	RTS
    70                          
    71                          COLLECT
    72  ce1f 78                 	SEI		; BETRIEBSSYS.-ROM
    73  ce20 a501               	LDA $01		; WEGBLENDEN
    74  ce22 48                 	PHA		; DAMIT RAM ZUGÄNGLICH
    75  ce23 a935               	LDA #$35	; WIRD.
    76  ce25 8501               	STA $01
    77                          
    78  ce27 a003               	LDY #3		; ZEROPAGE-REG.
    79  ce29 b9fb00             L10	LDA $FB,Y	; RETTEN
    80  ce2c 99fdcf             	STA SPTR+2,Y
    81  ce2f 88                 	DEY
    82  ce30 10f7               	BPL L10
    83  ce32 c8                 	INY
    84  ce33 8453               	STY FLAG	; ENDEFLAG=0 (NEIN)
    85  ce35 a550               	LDA PTR		; PTR-REGISTER RETTEN
    86  ce37 a651               	LDX PTR+1
    87  ce39 8dfbcf             	STA SPTR
    88  ce3c 8efccf             	STX SPTR+1
    89  ce3f a537               	LDA MEMEND	; STRING POINTER
    90  ce41 8533               	STA STRPTR	; UND BEREICHSANFANG
    91  ce43 a638               	LDX MEMEND+1	; AUF SPEICHERENDE
    92  ce45 8634               	STX STRPTR+1	; SETZEN
    93  ce47 85fb               	STA BERANF
    94  ce49 86fc               	STX BERANF+1
    95                          
    96  ce4b a6fb               BEGIN1	LDX BERANF	; *** HAUPTSCHLEIFE ***
    97  ce4d a5fc               	LDA BERANF+1	; BEREICH
    98  ce4f 86fd               	STX BEREND	; UM CA. 8K
    99  ce51 85fe               	STA BEREND+1	; NACH UNTEN VERLEGEN
   100  ce53 38                 	SEC
   101  ce54 e91f               	SBC #$1F	; 8K - 1 PAGE
   102  ce56 9008               	BCC L2
   103  ce58 85fc               	STA BERANF+1
   104  ce5a e431               	CPX STREND	; STRINGBEREICHENDE
   105  ce5c e532               	SBC STREND+1	; ERREICHT?
   106  ce5e b012               	BCS L1
   107  ce60 e653               L2	INC FLAG	; JA, ENDEFLAG SETZEN
   108  ce62 a531               	LDA STREND	; BEREICHANFANG =
   109  ce64 a632               	LDX STREND+1	; SPEICHERANFANG
   110  ce66 85fb               	STA BERANF	; BEREICHANFANG = BEREICHENDE
   111  ce68 86fc               	STX BERANF+1	; (SONDERFALL)
   112  ce6a c5fd               	CMP BEREND	; -> BEREICH IST 0 BYTE LANG
   113  ce6c d004               	BNE L1		; -> FERTIG
   114  ce6e e4fe               	CPX BEREND+1
   115  ce70 f074               	BEQ ENDE
   116  ce72 a900               L1	LDA #0		; BUFFERPOINTER AUF
   117  ce74 855f               	STA BUFPTR	; $10000 (65536) = 0
   118  ce76 8560               	STA BUFPTR+1	; SETZEN.
   119  ce78 38                 	SEC
   120                          
   121  ce79 2000cf             BEGIN2	JSR GETSA	; NÄCHSTE STRINGADRESSE HOLEN
   122  ce7c a54e               	LDA LEN		; WENN 0, DANN
   123  ce7e f03d               	BEQ WEITER	; KEINEN STRING MEHR
   124  ce80 98                 	TYA		; GEFUNDEN!
   125  ce81 e4fb               	CPX BERANF	;
   126  ce83 e5fc               	SBC BERANF+1
   127  ce85 9032               	BCC L3		; LIEGT STRINGADRESSE
   128  ce87 98                 	TYA		; IM BEREICH?
   129  ce88 e4fd               	CPX BEREND
   130  ce8a e5fe               	SBC BEREND+1	; AUSSERHALB, DANN
   131  ce8c b02b               	BCS L3		; NÄCHSTER STRING!
   132  ce8e 38                 	SEC
   133  ce8f a55f               	LDA BUFPTR	; STRINGLÄNGE VERSCHIEBEN
   134  ce91 e54e               	SBC LEN
   135  ce93 855f               	STA BUFPTR
   136  ce95 b002               	BCS L4
   137  ce97 c660               	DEC BUFPTR+1
   138  ce99 8459               L4	STY STRADR+1	; STRINGADRESSE ABSPEICHERN
   139  ce9b 8658               	STX STRADR
   140  ce9d a64e               	LDX LEN		; STRING IN DEN BUFFERBEREICH
   141  ce9f a000               	LDY #0
   142  cea1 b158               LOOP1	LDA (STRADR),Y	; ÜBERTRAGEN
   143  cea3 915f               	STA (BUFPTR),Y
   144  cea5 c8                 	INY
   145  cea6 ca                 	DEX
   146  cea7 d0f8               	BNE LOOP1
   147  cea9 18                 	CLC
   148  ceaa a55f               	LDA BUFPTR	; NEUE STRINGADRESSE BERECHNEN!
   149  ceac 6533               	ADC STRPTR
   150  ceae 8558               	STA STRADR	; STRADR=BUFPTR-(65536-STRPTR)
   151  ceb0 a560               	LDA BUFPTR+1
   152  ceb2 6534               	ADC STRPTR+1	; 16-BIT BERECHNUNG
   153  ceb4 8559               	STA STRADR+1	; -> 65536 = 0 -> SA=BP+SP
   154  ceb6 20e9cf             	JSR CORR	; STRINGADRESSE IM DESCRIPTOR ÄNDERN
   155  ceb9 18                 L3	CLC
   156  ceba 4c79ce             	JMP BEGIN2	; NÄCHSTER STRING
   157  cebd a55f               WEITER	LDA BUFPTR	; BUFFER LEER?
   158  cebf 0560               	ORA BUFPTR+1
   159  cec1 f01c               	BEQ L6		; JA, DANN BRAUCHT NICHTS
   160  cec3 18                 	CLC		; VERSCHOBEN WERDEN!
   161  cec4 a533               	LDA STRPTR
   162  cec6 aa                 	TAX
   163  cec7 655f               	ADC BUFPTR	; VERSCHIEBE PARAMETER
   164  cec9 8533               	STA STRPTR	; ERMITTELN
   165  cecb a534               	LDA STRPTR+1
   166  cecd a8                 	TAY
   167  cece 6560               	ADC BUFPTR+1	; BUFPTGR= QUELLBLOCKANFANG
   168  ced0 8534               	STA STRPTR+1
   169  ced2 a900               	LDA #$00	; QUELLBLOCKENDE
   170  ced4 855a               	STA $5A
   171  ced6 855b               	STA $5B
   172  ced8 8658               	STX $58		; ZIELBLOCKEND+1
   173  ceda 8459               	STY $59
   174  cedc 20bfa3             	JSR $A3BF	; BASIC-ROUTINE BLOCKVERSCHIEBEN
   175  cedf a553               L6	LDA FLAG	; ENDFLAG GESETZT?
   176  cee1 d003               	BNE ENDE
   177  cee3 4c4bce             	JMP BEGIN1	; NÄCHSTEN BEREICH UNTERSUCHEN
   178                          
   179  cee6 68                 ENDE	PLA		; URSPRÜNGLICHER ZUSTAND
   180  cee7 8501               	STA $01		; KERNAL-ROM WIEDER AKTIVIEREN
   181  cee9 58                 	CLI
   182  ceea adfbcf             	LDA SPTR	; ZEROPAGE REGISTER WIEDER
   183  ceed aefccf             	LDX SPTR+1	; HERSTELLEN.
   184  cef0 8550               	STA PTR
   185  cef2 8651               	STX PTR+1
   186  cef4 a003               	LDY #3
   187  cef6 b9fdcf             L11	LDA SPTR+2,Y
   188  cef9 99fb00             	STA $FB,Y
   189  cefc 88                 	DEY
   190  cefd 10f7               	BPL L11
   191  ceff 60                 	RTS
   192                          
   193                          ;
   194                          ; GETSA: ( C, STRDP -> STRDP, LEN, X, Y )
   195                          ;
   196                          
   197  cf00 906c               GETSA	BCC G1		; C=0 -> NÄCHSTEN STRING
   198  cf02 a900               	LDA #0		; ZUM NORMALEN EINSPRUNG
   199  cf04 8523               	STA STRDP+1	; TEMP. STRINGSTACK DURCHSUCHEN ...
   200  cf06 a216               	LDX #TOSS-3	; DESCRIPTOR-POINTER
   201  cf08 8622               	STX STRDP	; SETZEN ...
   202  cf0a a903               	LDA #3		; STATUS: TEMP. STRING-STACK
   203  cf0c 854f               	STA STAT
   204  cf0e a522               DSTCK	LDA STRDP	; NÄCHSTEN DESCRIPTOR
   205  cf10 18                 	CLC
   206  cf11 6903               	ADC #3		; DESCRIPTOR-LÄNGE
   207  cf13 8522               	STA STRDP
   208  cf15 a000               	LDY #0
   209  cf17 b122               	LDA (STRDP),Y	; LÄNGE
   210  cf19 854e               	STA LEN
   211  cf1b c8                 	INY
   212  cf1c b122               	LDA (STRDP),Y	; ADR. LOW
   213  cf1e aa                 	TAX
   214  cf1f c8                 	INY
   215  cf20 b122               	LDA (STRDP),Y	; ADR. HIGH
   216  cf22 a8                 	TAY
   217  cf23 a516               	LDA TSSP	; STRING-STACK ENDE?
   218  cf25 c522               	CMP STRDP
   219  cf27 f005               	BEQ VARS	; ENDE, DANN WEITER MIT VARIABLEN
   220  cf29 a54e               	LDA LEN
   221  cf2b f0e1               	BEQ DSTCK	; LEERER STRING, DANN NÄCHSTEN
   222  cf2d 60                 	RTS
   223                          
   224  cf2e a52d               VARS	LDA VARTAB	; VARIABLENANFANG
   225  cf30 a62e               	LDX VARTAB+1
   226  cf32 8522               	STA STRDP
   227  cf34 8623               	STX STRDP+1
   228  cf36 a522               	LDA STRDP
   229  cf38 a002               	LDY #2		; STATUS: EINFACHE VARIABLEN
   230  cf3a 844f               	STY STAT
   231  cf3c d00b               	BNE V1
   232  cf3e 18                 VAR	CLC		; NÄCHSTE VARIABLE
   233  cf3f a522               	LDA STRDP
   234  cf41 6907               	ADC #7		; VARIABLENLÄNGE
   235  cf43 8522               	STA STRDP
   236  cf45 9002               	BCC V1
   237  cf47 e623               	INC STRDP+1
   238  cf49 a623               V1	LDX STRDP+1	; VAR-ENDE (=ARRAY-ANFANG)?
   239  cf4b e430               	CPX ARYTAB+1
   240  cf4d d004               	BNE G2
   241  cf4f c52f               	CMP ARYTAB
   242  cf51 f026               	BEQ N2		; VARS ENDE, WEITER MIT ARRAYS
   243  cf53 a000               G2	LDY #0		; VARIABLENNAME
   244  cf55 b122               	LDA (STRDP),Y	; 1. ZEICHEN
   245  cf57 30e5               	BMI VAR		; KEIN STRING, NÄCHSTE V.
   246  cf59 c8                 	INY
   247  cf5a b122               	LDA (STRDP),Y
   248  cf5c 10e0               	BPL VAR		; KEIN STRING, NÄCHSTE V.
   249  cf5e c8                 	INY
   250  cf5f b122               	LDA (STRDP),Y	; STRINGLÄNGE
   251  cf61 f0db               	BEQ VAR		; = 0, NÄCHSTE V.
   252  cf63 854e               	STA LEN
   253  cf65 c8                 	INY
   254  cf66 b122               	LDA (STRDP),Y	; ADRESSE LOW
   255  cf68 aa                 	TAX
   256  cf69 c8                 	INY
   257  cf6a b122               	LDA (STRDP),Y	; ADRESSE HIGH
   258  cf6c a8                 	TAY
   259  cf6d 60                 	RTS
   260                          
   261  cf6e a54f               G1	LDA STAT	; GETSA FORTSETZUNGSEINSTIEG
   262  cf70 c902               	CMP #2		; STATUS STRING?
   263  cf72 904a               	BCC ARRAY	; =1 -> ARRAY
   264  cf74 f0c8               	BEQ VAR		; =2 -> VARIABLE
   265  cf76 4c0ecf             	JMP DSTCK	; =3 -> STRING DESC. STACK
   266                          
   267  cf79 8550               N2	STA PTR		; ARRAY-POINTER
   268  cf7b 8651               	STX PTR+1
   269  cf7d a001               	LDY #1
   270  cf7f 844f               	STY STAT	; ARRAYS-STATUS
   271  cf81 a550               NEXTARR	LDA PTR
   272  cf83 a651               	LDX PTR+1
   273  cf85 e432               	CPX STREND+1
   274  cf87 d004                       BNE A1
   275  cf89 c531               	CMP STREND
   276  cf8b f057               	BEQ N3		; ARRAYS FERTIG
   277  cf8d 8522               A1	STA STRDP	; C=0
   278  cf8f 8623               	STX STRDP+1
   279  cf91 a000               	LDY #0
   280  cf93 b122               	LDA (STRDP),Y	; ARRAY-NAME
   281  cf95 aa                 	TAX
   282  cf96 c8                 	INY
   283  cf97 b122               	LDA (STRDP),Y
   284  cf99 08                 	PHP		; VAR-TYP MERKEN
   285  cf9a c8                 	INY
   286  cf9b b122               	LDA (STRDP),Y	; ZUM NÄCHSTEN ARRAY
   287  cf9d 6550               	ADC PTR		; C IST BEREITS 0
   288  cf9f 8550               	STA PTR
   289  cfa1 c8                 	INY
   290  cfa2 b122               	LDA (STRDP),Y
   291  cfa4 6551               	ADC PTR+1
   292  cfa6 8551               	STA PTR+1
   293  cfa8 28                 	PLP		; VAR-TYP HOLEN
   294  cfa9 10d6               	BPL NEXTARR	; KEIN STRINGARRAY
   295  cfab 8a                 	TXA
   296  cfac 30d3               	BMI NEXTARR	; KEIN STRINGARRAY
   297  cfae c8                 	INY
   298  cfaf b122               	LDA (STRDP),Y	; ANZAHL DER DIMENSIONEN
   299  cfb1 a000               	LDY #0
   300  cfb3 0a                 	ASL		; *2
   301  cfb4 6902               	ADC #2		; +2 (VARNAME)
   302  cfb6 6522               	ADC STRDP
   303  cfb8 8522               	STA STRDP
   304  cfba 9002               	BCC ARRAY
   305  cfbc e623               	INC STRDP+1
   306  cfbe 18                 ARRAY	CLC
   307  cfbf a522               	LDA STRDP
   308  cfc1 6903               	ADC #3		; STRING-DESCRIPTOR-LÄNGE
   309  cfc3 8522               	STA STRDP
   310  cfc5 9002               	BCC A5
   311  cfc7 e623               	INC STRDP+1
   312  cfc9 a623               A5	LDX STRDP+1
   313  cfcb e451               	CPX PTR+1	; ARRAY DURCH?
   314  cfcd d004               	BNE A6
   315  cfcf c550               	CMP PTR
   316  cfd1 f0ae               	BEQ NEXTARR
   317  cfd3 a000               A6	LDY #0
   318  cfd5 b122               	LDA (STRDP),Y	; STR.-LÄNGE
   319  cfd7 f0e5               	BEQ ARRAY	; WEITER IM ARRAY
   320  cfd9 854e               	STA LEN
   321  cfdb c8                 	INY
   322  cfdc b122               	LDA (STRDP),Y	; ADRESSE LOW
   323  cfde aa                 	TAX
   324  cfdf c8                 	INY
   325  cfe0 b122               	LDA (STRDP),Y	; ADRESSE HIGH
   326  cfe2 a8                 	TAY
   327  cfe3 60                 	RTS		; IN X/Y RETOUR
   328                          
   329  cfe4 a900               N3	LDA #0
   330  cfe6 854e               	STA LEN
   331  cfe8 60                 	RTS
   332                          
   333                          ;
   334                          ; CORR ( STRADR, STAT -> )
   335                          ;
   336  cfe9 a54f               CORR	LDA STAT	; STR.-ADR. KORRIGIEREN
   337  cfeb 0a                 	ASL		; * 2 (C=0)
   338  cfec 69ff               	ADC #$FF	; MINUS 1
   339  cfee 2903               	AND #3		; NUR 2 BITS
   340  cff0 a8                 	TAY
   341  cff1 a558               	LDA STRADR	; LAGE DES DESCRIPTORS
   342  cff3 9122               	STA (STRDP),Y	; ... BEI STR. STACK
   343  cff5 c8                 	INY		; ... UND ARRAY VERSCHIEDEN!
   344  cff6 a559               	LDA STRADR+1
   345  cff8 9122               	STA (STRDP),Y
   346  cffa 60                 	RTS
   347                          
   348  cffb 000000000000       SPTR	!byte 0,0,0,0,0,0 ; 6 BYTE ZERO PAGE SAVE
   349                          
