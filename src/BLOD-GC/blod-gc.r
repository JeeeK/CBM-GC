
; ******** Source: blod-gc.asm
     1                          ;
     2                          ; **********************************
     3                          ; *  BACKLINK GARBAGE  COLLECTION  *
     4                          ; *        from Johann Klasek      *
     5                          ; *        j AT klasek DOT at      *
     6                          ; *       2021-03-30 VERS. 1.0     *
     7                          ; **********************************
     8                          ;
     9                          ; Collects unused (garbage) strings on the string heap,
    10                          ; replacing the BASIC 2.0 garbage collector on a C64.
    11                          ; Only those locations which is used by the legacy garbage
    12                          ; collector are in use here.
    13                          
    14                          ; Start of code ...
    15                          
    16                          !ifdef start {
    17                          	*=start
    18                          } else {
    19                          	*= $C500
    20                          }
    21                          
    22                          ; Options:
    23                          
    24                          ; Do not display an activiation mark on screen
    25                          ;no_indicator = 1
    26                          
    27                          
    28                          
    29                          ; BASIC system variables
    30                          
    31                          TOSS     = $19		; Top of String Descriptor Stack
    32                          EOSS     = $22		; End of String Descriptor Stack +1
    33                          TSSP     = $16		; Current String Descriptor Stack pointer
    34                          
    35                          VARTAB   = $2D		; End of BASIC program = begin of variable area
    36                          ARYTAB   = $2F		; End of variables = begin of arrays
    37                          STREND   = $31		; End of arrays = lowest possible string heap address
    38                          FRETOP   = $33		; Current string heap address
    39                          MEMSIZ   = $37		; Highest RAM address for BASIC, start of
    40                          			; string heap growing downwards.
    41                          MEMBEG   = STREND	; String heap memory begin = STREND
    42                          MEMEND   = MEMSIZ	; String heap memory end
    43                          
    44                          
    45                          ; variables
    46                          
    47                          HEAP     = FRETOP	; String pointer = FRETOP
    48                          STRDP    = $22		; String descriptor address (used in stage 1+3: GETSA in/out)
    49                          CPTR     = $22		; Pointer for installer routine (used in installer)
    50                          NEWHEAP  = $22		; New heap pointer (used in stage 2)
    51                          
    52                          STAT     = $57		; String status, for values in use,
    53                          			; see STAT_* below (GETSA in/out)
    54                          DESC     = $58		; String descriptor address (temp.)
    55                          STR      = $5A		; Points to a string
    56                          LEN      = $5D		; String length (GETSA out)
    57                          PTR      = $5F		; Array pointer (GETSA in/out)
    58                          
    59                          
    60                          
    61                          ; Constants
    62                          
    63                          ; for variable STAT (string status):
    64                          STAT_SDS = 0		; Is on String Descriptor Stack
    65                          STAT_VAR = 4		; Is a simple variable
    66                          STAT_ARY = 1		; Is in a array
    67                          
    68                          ; Memory configuration for PROCPORT:
    69                          MEMROM   = %00110111	; BASIC+KERNAL ROM, $37
    70                          MEMBAS   = %00110110	; BASIC RAM+KERNAL ROM, $34
    71                          MEMRAM   = %00110101	; BASIC+KERNAL RAM, $35
    72                          
    73                          ; for activity indicator
    74                          MARKCHAR = "*"          ; Indicator character
    75                          MARKCOL  = 9            ; Indicator color (red)
    76                          MARKOFF  = 40*25-1      ; indicator position (lower right corner)
    77                          MARKVPOS = VIDBASE+MARKOFF
    78                          MARKCPOS = COLBASE+MARKOFF
    79                          
    80                          
    81                          ; Memory locations
    82                          
    83                          GARBCOL  = $B526	; Entry point of the legacy GC
    84                          PATCH1   = $AA6C	; Overwrite string to variable
    85                          PATCH2   = $B66A	; String concatenation: 2nd argument handling
    86                          PATCH3   = $B726	; LEFT$() copy string
    87                          
    88                          BASIC    = $A000        ; BASIC ROM
    89                          KERNAL   = $E000        ; KERNAL ROM
    90                          ROMSIZE  = $2000        ; ROM length, 8 Kbyte
    91                          
    92                          VIDPAGE	 = $288		; Page of video RAM
    93                          VIDBASE  = $0400	; Video RAM
    94                          COLBASE  = $D800	; Color RAM
    95                          
    96                          PROCPORT = $01		; Processor port
    97                          
    98                          
    99                          
   100                          ; Installer
   101                          
   102                          INSTALL
   103                          
   104  c500 2c                 	!byte $2C	; Opcode BIT absolute, Argument 
   105                          			; contains the signature, acts as NOP.
   106  c501 4743               	!text "GC"	; Signature for the loader,
   107                          			; the same an on a fixed
   108                          			; location for all variants!
   109                          
   110                          	; BASIC-ROM/RAM patch hook
   111                          
   112                          	; copy BASIC into RAM to patch the GC routine
   113  c503 a937               	LDA #MEMROM
   114  c505 8501               	STA PROCPORT	; All ROM (where to copy from)
   115  c507 a000               	LDY #<BASIC	; ROM start
   116  c509 8422               	STY CPTR
   117  c50b a9a0               	LDA #>BASIC
   118  c50d 8523               	STA CPTR+1	; BASIC ROM start
   119  c50f a220               	LDX #>($2000)	; BASIC ROM length in pages
   120  c511 b122               CPYROM	LDA (CPTR),Y	; Read from ROM
   121  c513 9122               	STA (CPTR),Y	; Write to RAM
   122  c515 c8                 	INY
   123  c516 d0f9               	BNE CPYROM
   124  c518 e623               	INC CPTR+1	; Next page
   125  c51a ca                 	DEX		; Page counter
   126  c51b d0f4               	BNE CPYROM
   127                          
   128  c51d a501               	LDA PROCPORT	; Switch to RAM
   129  c51f 29fe               	AND #%11111110	; "BASIC off" mask
   130  c521 8501               	STA PROCPORT
   131                          
   132  c523 a951               	LDA #<HANDLE1	; let JSR in place!
   133  c525 8d6daa             	STA PATCH1+1
   134  c528 a9c5               	LDA #>HANDLE1
   135  c52a 8d6eaa             	STA PATCH1+2
   136                          
   137  c52d a9a2               	LDA #<HANDLE2	; let JSR in place!
   138  c52f 8d6bb6             	STA PATCH2+1
   139  c532 a9c5               	LDA #>HANDLE2
   140  c534 8d6cb6             	STA PATCH2+2
   141                          
   142  c537 a9bb               	LDA #<HANDLE3	; let JSR in place!
   143  c539 8d27b7             	STA PATCH3+1
   144  c53c a9c5               	LDA #>HANDLE3
   145  c53e 8d28b7             	STA PATCH3+2
   146                          
   147  c541 a9e1               	LDA #<COLLECT	; Write "JMP COLLECT"
   148  c543 8d27b5             	STA GARBCOL+1	; patch code.
   149  c546 a9c5               	LDA #>COLLECT
   150  c548 8d28b5             	STA GARBCOL+2
   151                          
   152  c54b a94c               	LDA #$4C	; The "JMP" opcode
   153  c54d 8d26b5             	STA GARBCOL
   154  c550 60                 	RTS
   155                          
   156                          
   157                          ; *** Handle Patch 1: LET variable overwrite
   158                          ;
   159                          ; Hooks at AA6C
   160                          ; Replacing:
   161                          ;	JSR $B6DB	; Remove only from SDS, but keep string on heap!
   162                          ; Continues at AA6F:
   163                          ;	LDY #$00
   164                          ;	LDA ($50),Y
   165                          ;	STA ($49),Y
   166                          
   167                          HANDLE1
   168  c551 c418               	CPY $18		; Descriptor on top of SDS?
   169  c553 d00a               	BNE +
   170  c555 c517               	CMP $17
   171  c557 d006               	BNE +
   172  c559 8516               	STA $16		; Yes, remove it from SDS
   173  c55b e903               	SBC #3
   174  c55d 8517               	STA $17
   175                          	
   176                          	; If destination variable points to string on the heap, free it.
   177                          
   178  c55f a000               +	LDY #0
   179                          	; $49 points to variable descriptor (in LET's destination variable)
   180  c561 b149               	LDA ($49),Y	; Get string length
   181  c563 f03c               	BEQ LEAVE	; Variable contains no string
   182  c565 aa                 	TAX		; > 0, save it for later
   183  c566 c8                 	INY
   184  c567 b149               	LDA ($49),Y	; String address low
   185  c569 855a               	STA STR
   186  c56b c8                 	INY
   187  c56c b149               	LDA ($49),Y	; String address high
   188  c56e 855b               	STA STR+1
   189                          
   190                          	; Free STR if on heap and return
   191                          
   192                          FREE
   193  c570 a55b               	LDA STR+1	; String address high
   194  c572 c534               	CMP FRETOP+1	; Heap top high
   195  c574 902b               	BCC LEAVE	; String below heap (on on heap)
   196  c576 d008               	BNE ++		; String on heap
   197  c578 a55a               	LDA STR		; String address low
   198  c57a c533               	CMP FRETOP	; Heap top low
   199  c57c 9023               	BCC LEAVE	; Leave when not on heap!
   200                          
   201  c57e a55b               	LDA STR+1	; String address greater or equal FRETOP
   202                          
   203  c580 c538               ++	CMP MEMEND+1	; String above string memory?
   204  c582 9008               	BCC +++		; no
   205  c584 d01b               	BNE LEAVE	; yes
   206  c586 a55a               	LDA STR		; High byte equal, compare low byte
   207  c588 c537               	CMP MEMEND
   208  c58a b015               	BCS LEAVE	; Above heap
   209                          	
   210                          	; String on heap: mark it as free
   211                          
   212  c58c 8a                 +++	TXA		; Restore length
   213  c58d c901               	CMP #1		; String of length 1?
   214  c58f d006               	BNE ++
   215                          
   216  c591 a000               	LDY #0
   217  c593 915a               	STA (STR),Y	; Single byte on heap contains 1
   218  c595 f00a               	BEQ LEAVE	; leave, always (Z=1)
   219                          
   220  c597 a8                 ++	TAY		; Length to Y (> 1!)
   221  c598 88                 	DEY
   222  c599 88                 	DEY		; Y: Length - 2
   223  c59a 915a               	STA (STR),Y	; Pre-last byte of string has length
   224  c59c c8                 	INY
   225  c59d a9ff               	LDA #$FF
   226  c59f 915a               	STA (STR),Y	; Last byte of string with gap-marker
   227  c5a1 60                 LEAVE	RTS
   228                          
   229                          
   230                          
   231                          ; String concatenation: free 2nd argument after copying!
   232                          
   233                          ;.,B65D 20 75 B4 JSR $B475       copy descriptor pointer and make string space A bytes long
   234                          ;.,B660 20 7A B6 JSR $B67A       copy string from descriptor to utility pointer
   235                          ;.,B663 A5 50    LDA $50         get descriptor pointer low byte
   236                          ;.,B665 A4 51    LDY $51         get descriptor pointer high byte
   237                          ;.,B667 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
   238                          ;                                returns with A = length, X = pointer low byte,
   239                          ;                                Y = pointer high byte
   240                          ;.,B66A 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   241                          ;.,B66D A5 6F    LDA $6F         get descriptor pointer low byte
   242                          ;.,B66F A4 70    LDY $70         get descriptor pointer high byte
   243                          ;.,B671 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
   244                          ;                                returns with A = length, X = pointer low byte,
   245                          ;                                Y = pointer high byte
   246                          ;.,B674 20 CA B4 JSR $B4CA       check space on descriptor stack then put string address
   247                          ;                                and length on descriptor stack and update stack pointers
   248                          ;.,B677 4C B8 AD JMP $ADB8       continue evaluation
   249                          
   250                          ; -> 
   251                          
   252                          ;.,B66A 20 8C B6 JSR HANDLE2     store string from pointer to utility pointer
   253                          ;
   254                          HANDLE2
   255  c5a2 208cb6             	JSR $B68C	; Copy string to utility pointer's location
   256  c5a5 a550               	LDA $50		; Descriptor address of 2nd argument
   257  c5a7 a451               	LDY $51		; It is never top on heap, so just mark it as free
   258  c5a9 c516               	CMP $16		; Previously popped element
   259  c5ab d0f4               	BNE LEAVE
   260  c5ad c418               	CPY $18		; High byte (normally 0)
   261  c5af d0f0               	BNE LEAVE
   262  c5b1 20d2c5             	JSR FREESDS	; mark already remove element from SDS as free
   263  c5b4 a56f               	LDA $6F
   264  c5b6 a470               	LDY $70
   265  c5b8 4cc2c5             	JMP POPSDS	; remove element from SDS and mark as free
   266                          	
   267                          
   268                          ; LEFT$(), RIGHT$(), MID$(): Free input string
   269                          
   270                          ;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   271                          ;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put string address
   272                          ;                                and length on descriptor stack and update stack pointers
   273                          ; -> 
   274                          ;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer
   275                          
   276                          
   277                          HANDLE3
   278                          	; A: length, copy from ($22) to ($35)
   279  c5bb 208cb6             	JSR $B68C	; Copy string part into allocated space
   280  c5be a550               	LDA $50
   281  c5c0 a451               	LDY $51
   282                          
   283                          	; the string itself is not top of heap, just mark as free and remove from SDS
   284                          
   285                          POPSDS
   286  c5c2 c418               	CPY $18		; Descriptor on top of SDS?
   287  c5c4 d0db               	BNE LEAVE	; RTS
   288  c5c6 c517               	CMP $17
   289  c5c8 d0d7               	BNE LEAVE	; RTS
   290                          	; free memory and pull from SDS
   291  c5ca 20d2c5             	JSR FREESDS
   292  c5cd a517               	LDA $17		; Top elememt on SDS
   293  c5cf 4ce3b6             	JMP $B6E3	; remove from SDS (A low byte to SDS element)
   294                          FREESDS
   295                          	; A/Y is pointer to string descriptor on the SDS!
   296  c5d2 aa                 	TAX		; Index in zero-page
   297  c5d3 b501               	LDA 1,X		; String address low
   298  c5d5 855a               	STA STR
   299  c5d7 b502               	LDA 2,X		; String address high
   300  c5d9 855b               	STA STR+1
   301  c5db b500               	LDA 0,X		; String length
   302  c5dd aa                 	TAX
   303  c5de d090               	BNE FREE	; Length X, address STR/STR+1
   304  c5e0 60                 	RTS		; No free if length = 0!
   305                          
   306                          
   307                          
   308                          ; *** Garbage Collector
   309                          
   310                          COLLECT
   311                          
   312                          !ifndef no_indicator {
   313  c5e1 a200               	LDX #0
   314  c5e3 8622               	STX CPTR	; Pointer low byte = 0
   315  c5e5 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   316                          	!if (>MARKOFF) >= 1 {
   317  c5e8 e8                 	INX
   318                          	!if (>MARKOFF) >= 2 {
   319  c5e9 e8                 	INX
   320                          	!if (>MARKOFF) >= 3 {
   321  c5ea e8                 	INX
   322                          	} } }
   323                          	; X contains now the page plus the offset's high byte
   324  c5eb 8623               	STX CPTR+1
   325  c5ed a0e7               	LDY #<(MARKOFF)
   326  c5ef b122               	LDA (CPTR),Y	; Activity indicator on screen:
   327  c5f1 8d15c8             	STA ORIGVID	; Save current character
   328  c5f4 a92a               	LDA #MARKCHAR
   329  c5f6 9122               	STA (CPTR),Y	; Set mark character
   330  c5f8 ade7db             	LDA MARKCPOS	; Same for the color information
   331  c5fb 8d16c8             	STA ORIGCOL	; Save current color
   332  c5fe a909               	LDA #MARKCOL
   333  c600 8de7db             	STA MARKCPOS	; Set mark color
   334                          }
   335                          
   336                          
   337                          
   338                          ; walk through all strings and reorganize them
   339                          
   340                          STAGE1
   341  c603 38                         SEC             ; Initialize search
   342                          NEXTSTR
   343  c604 2046c7             	JSR GETSA
   344  c607 f052                       BEQ STAGE2      ; No String found anymore
   345                                                  ; Address in X/Y
   346                          
   347  c609 c434               	CPY FRETOP+1	; String on heap?
   348  c60b 90f7               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   349  c60d d004               	BNE +
   350  c60f e433               	CPX FRETOP
   351  c611 90f1               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   352                          
   353  c613 865a               +	STX STR		; Start of string which is on heap
   354  c615 845b               	STY STR+1
   355  c617 a55d               	LDA LEN
   356  c619 c901               	CMP #1		; String length 1?
   357  c61b d014               	BNE ++
   358                          
   359                          	; LEN 1: 
   360                          	;	copy string value into descriptor
   361                          	;	overwrite string on heap with value 1
   362                          
   363  c61d a000               	LDY #0
   364  c61f b15a               	LDA (STR),Y	; String value
   365  c621 aa                 	TAX
   366  c622 a901               	LDA #1		; Marker for string with length 1
   367  c624 915a               	STA (STR),Y	; Store marker on heap
   368  c626 a557               	LDA STAT
   369  c628 4a                 	LSR		; Shift right gives offset, which
   370  c629 a8                 	TAY		; refers to STRDP leading to the descriptor
   371  c62a c8                 	INY		; Position string address low byte
   372  c62b 8a                 	TXA		; String value
   373  c62c 9122               	STA (STRDP),Y	; Store value in descriptor (low address byte)
   374  c62e 18                 	CLC		; Continuation mode for GETSA
   375  c62f 90d3               	BCC NEXTSTR	; Always
   376                          
   377                          	; LEN >1:
   378                          	;	copy backlink bytes to descriptor
   379                          	;	store descriptor pointer to backlink
   380                          
   381  c631 a8                 ++	TAY		; Length
   382  c632 88                 	DEY		; Index to last byte
   383  c633 b15a               	LDA (STR),Y
   384  c635 48                 	PHA		; Last byte of string
   385  c636 a623               	LDX STRDP+1
   386  c638 a557               	LDA STAT
   387  c63a 4a                 	LSR		; Shift right gives offset to the descriptor
   388  c63b 18                 	CLC
   389  c63c 6522               	ADC STRDP
   390  c63e 9001               	BCC +
   391  c640 e8                 	INX
   392  c641 48                 +	PHA		; STRDP + offset low
   393  c642 8a                 	TXA		; X STRDP + offset high
   394  c643 915a               	STA (STR),Y	; Back-link high
   395  c645 88                 	DEY
   396  c646 b15a               	LDA (STR),Y	; Pre-last byte string
   397  c648 aa                 	TAX
   398  c649 68                 	PLA		; STRDP + offset low
   399  c64a 915a               	STA (STR),Y	; Back-link low
   400  c64c a557               	LDA STAT
   401  c64e 4a                 	LSR		; Shift right gives offset, which
   402  c64f a8                 	TAY		; refers to STRDP leading to the descriptor
   403  c650 c8                 	INY		; Skip length byte
   404  c651 68                 	PLA		; Last byte of string
   405  c652 9122               	STA (STRDP),Y	; Store into descriptor address low byte
   406  c654 8a                 	TXA		; Pre-last byte of string
   407  c655 c8                 	INY		; =2
   408  c656 9122               	STA (STRDP),Y	; Store into descriptor address high byte
   409  c658 18                 	CLC		; Continuation mode for GETSA
   410  c659 90a9               	BCC NEXTSTR	; Always
   411                          	
   412                          
   413                          
   414                          
   415                          ; walk through heap, remove gaps and move strings
   416                          
   417                          STAGE2
   418  c65b a438               	LDY MEMEND+1	; Top of memory.
   419  c65d a637               	LDX MEMEND	; Set new heap top
   420  c65f 8622               	STX NEWHEAP	; to memory end.
   421  c661 8423               	STY NEWHEAP+1
   422                          			; Entry point from no-gap part
   423  c663 8460               LOOP2R	STY PTR+1	; PTR comes from X
   424  c665 a000               	LDY #0
   425                          LOOP2
   426  c667 8a                 	TXA		; PTR minus 1
   427  c668 d002               	BNE +
   428  c66a c660               	DEC PTR+1
   429  c66c ca                 +	DEX
   430  c66d 865f               -	STX PTR
   431                          
   432  c66f e433               	CPX HEAP	; PTR blow top of heap?
   433  c671 a560               	LDA PTR+1
   434  c673 e534               	SBC HEAP+1
   435  c675 b003               	BCS +		; PTR >= HEAP
   436  c677 4cfbc6             	JMP EXIT2
   437                          +
   438  c67a b15f               	LDA (PTR),Y	; Get back-link high
   439  c67c c901               	CMP #1		; 1-byte gap
   440  c67e f0e7               	BEQ LOOP2	; Skip it, covered later in stage 3.
   441                          
   442  c680 e8                 	INX		; Decrement PTR, but leaving A untouched
   443  c681 ca                 	DEX		; PTR low = 0?
   444  c682 d002               	BNE +
   445  c684 c660               	DEC PTR+1
   446  c686 ca                 +	DEX		; PTR low
   447  c687 865f               	STX PTR
   448                          
   449  c689 c9ff               	CMP #$FF	; Gap marker? (length >1)
   450  c68b d00f               	BNE NOGAP
   451                          			; Skip gap of a certain length ...
   452  c68d b15f               	LDA (PTR),Y	; Gap length
   453  c68f 49ff               	EOR #$FF	; A is > 1
   454                          			; Carry set from CMP above!
   455  c691 6901               	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
   456                          			; Never 0 because gap length > 1
   457  c693 655f               	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
   458  c695 aa                 	TAX		; PTR low byte
   459  c696 b0d5               	BCS -		; Position on last string byte
   460  c698 c660               	DEC PTR+1	; PTR high byte, always >0
   461  c69a d0d1               	BNE -		; Always, PTR has string address,
   462                          			; pointing to last string byte
   463                          
   464                          ; We have a backlink to the string:
   465  c69c 8559               NOGAP	STA DESC+1	; Backlink high and
   466  c69e b15f               	LDA (PTR),Y	; backlink low is the
   467  c6a0 8558               	STA DESC	; descriptor address.
   468                          
   469  c6a2 b158               	LDA (DESC),Y	; Length from descriptor
   470  c6a4 49ff               	EOR #$FF
   471  c6a6 48                 	PHA		; Needed for heap later
   472  c6a7 a660               	LDX PTR+1	; Transfer to STR ...
   473                          			; Carry clear from CMP #$FF
   474  c6a9 6903               	ADC #3		; -(LEN-2) + PTR -> PTR
   475  c6ab d002               	BNE +		; PTR already in position
   476                          			; Special case length = 2:
   477  c6ad e8                 	INX		; compensate for the high byte decrement
   478  c6ae 18                 	CLC		; Adding 0 with carry cleared, leaves PTR unchanged.
   479  c6af 655f               +	ADC PTR		; Accumulator before add. was in range 0 to FC
   480                          			; which never sets the carry!
   481  c6b1 b001               	BCS +
   482  c6b3 ca                 	DEX		; In case of adding 0 X is already compensated.
   483  c6b4 865b               +	STX STR+1	; STR points to string start.
   484  c6b6 855a               	STA STR
   485                          	
   486                          	; make space on heap vor LEN bytes
   487  c6b8 68                 	PLA		; LEN, but only complemented
   488  c6b9 38                 	SEC		; Finalize two's complement (+1 from carry)
   489  c6ba 6522               	ADC NEWHEAP	; HEAP - LEN -> HEAP
   490  c6bc 8522               	STA NEWHEAP
   491  c6be b002               	BCS +
   492  c6c0 c623               	DEC NEWHEAP+1
   493                          +	
   494                          	; copy LEN bytes from STR to HEAP
   495  c6c2 b158               	LDA (DESC),Y	; length from descriptor
   496  c6c4 a8                 	TAY		; as index
   497  c6c5 88                 	DEY		; index = length - 2
   498  c6c6 88                 	DEY
   499  c6c7 f00e               	BEQ +		; 0, nothing to copy
   500  c6c9 88                 	DEY		; -1, index of last byte
   501  c6ca f007               	BEQ ++		; No loop if index is 0.
   502  c6cc b15a               -	LDA (STR),Y	; Transfer byte 1 to len-1
   503  c6ce 9122               	STA (NEWHEAP),Y
   504  c6d0 88                 	DEY
   505  c6d1 d0f9               	BNE -
   506  c6d3 b15a               ++	LDA (STR),Y	; transfer byte 0
   507  c6d5 9122               	STA (NEWHEAP),Y
   508                          +	
   509                          	; correct descriptor
   510  c6d7 a002               	LDY #2		; Offset in descriptor
   511  c6d9 b158               	LDA (DESC),Y	; pre-last string byte 
   512  c6db 48                 	PHA		; Save
   513  c6dc a523               	LDA NEWHEAP+1
   514  c6de 9158               	STA (DESC),Y	; Restore string address low
   515  c6e0 88                 	DEY
   516  c6e1 b158               	LDA (DESC),Y	; last string byte
   517  c6e3 48                 	PHA		; Save
   518  c6e4 a522               	LDA NEWHEAP	; Restore string address high
   519  c6e6 9158               	STA (DESC),Y	; Backlink high
   520                          
   521  c6e8 88                 	DEY		; Y=0
   522                          	; Restore string bytes to backlink
   523  c6e9 b158               	LDA (DESC),Y	; Length byte
   524  c6eb a8                 	TAY
   525  c6ec 88                 	DEY		; Index of last string byte
   526  c6ed 68                 	PLA
   527  c6ee 9122               	STA (NEWHEAP),Y	; last byte
   528  c6f0 88                 	DEY
   529  c6f1 68                 	PLA		
   530  c6f2 9122               	STA (NEWHEAP),Y	; pre-last byte
   531                          
   532  c6f4 a65a               	LDX STR		; PTR low byte in X
   533  c6f6 a45b               	LDY STR+1	; always >0
   534  c6f8 4c63c6             	JMP LOOP2R	; Loop with set PTR and reset Y
   535                          	
   536                          EXIT2
   537  c6fb a522               	LDA NEWHEAP	; Set rebuilt, compacted heap
   538  c6fd 8533               	STA HEAP	; as new heap.
   539  c6ff a523               	LDA NEWHEAP+1
   540  c701 8534               	STA HEAP+1
   541                          
   542                          
   543                          
   544                          
   545                          ; Put strings with length 1 (stored in the descriptor) back on heap
   546                          
   547                          STAGE3
   548  c703 38                         SEC             ; Initialize search for GETSA
   549  c704 24                         !byte $24       ; BIT ZP, skip next instruction
   550                          NEXT1STR
   551  c705 18                 	CLC
   552  c706 2046c7             	JSR GETSA
   553  c709 f021                       BEQ EXIT        ; No String found anymore
   554                                                  ; Address in X/Y, descr. at STRDP
   555  c70b c65d               	DEC LEN
   556  c70d d0f6               	BNE NEXT1STR	; Loop if not length 1
   557                          	
   558  c70f 8a                 	TXA		; String addr low is the string byte!
   559  c710 a000               	LDY #0
   560  c712 a633               	LDX HEAP
   561  c714 d002               	BNE +		; Heap pointer - 1
   562  c716 c634               	DEC HEAP+1
   563  c718 ca                 +	DEX		; Low byte used later
   564  c719 8633               	STX HEAP
   565  c71b 9133               	STA (HEAP),Y	; stored string byte back to heap
   566                          
   567  c71d a557               	LDA STAT
   568  c71f 4a                 	LSR		; Shift right gives offset, which
   569  c720 a8                 	TAY		; refers to STRDP leading to the descriptor
   570  c721 c8                 	INY		; Low byte address in descriptor
   571  c722 8a                 	TXA		; Heap pointer low
   572  c723 9122               	STA (STRDP),Y	; stored back into descriptor
   573  c725 c8                 	INY
   574  c726 a534               	LDA HEAP+1	; Heap pointer high
   575  c728 9122               	STA (STRDP),Y	; stored back into descriptor
   576  c72a d0d9               	BNE NEXT1STR	; Branch always, because high byte >0
   577                          	
   578                          
   579                          ; *** Garbage collection finished
   580                          
   581                          EXIT
   582                          
   583                          !ifndef no_indicator {
   584  c72c a200               	LDX #0
   585  c72e 8622                       STX CPTR        ; Pointer low byte = 0
   586  c730 ae8802                     LDX VIDPAGE     ; Startpage of video RAM
   587                                  !if (>MARKOFF) >= 1 {
   588  c733 e8                         INX
   589                                  !if (>MARKOFF) >= 2 {
   590  c734 e8                         INX
   591                                  !if (>MARKOFF) >= 3 {
   592  c735 e8                         INX
   593                                  } } }
   594                                  ; X contains now the page plus the offset's high byte
   595  c736 8623                       STX CPTR+1
   596  c738 a0e7                       LDY #<(MARKOFF)
   597  c73a ad15c8                     LDA ORIGVID     ; Clear activation indicator:
   598  c73d 9122                       STA (CPTR),Y    ; restore character on screen
   599  c73f ad16c8                     LDA ORIGCOL     ; and its color.
   600  c742 8de7db                     STA MARKCPOS
   601                          }
   602  c745 60                 	RTS
   603                          
   604                          
   605                          ;
   606                          ; *** Get String - fetch next string with length > 0
   607                          ;
   608                          ; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
   609                          ; 
   610                          ; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
   611                          ;
   612                          ; If C=1 start from the beginning at SDS, otherwise
   613                          ; continue with position STRDP and string status STAT.
   614                          ; If the Z-Flag is set no string is available,
   615                          ; otherwise X/Y contains the address and LEN
   616                          ; the length of the string.
   617                          
   618  c746 905a               GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
   619                                                  ; otherwise start with at SDS ...
   620                          
   621                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   622                          ;
   623                          ;    +-------------+
   624                          ;    |             V
   625                          ;    |    belegt->|<-frei
   626                          ;   +-+     +-----+-----+-----+
   627                          ;   | |     |S|L|H|S|L|H|S|L|H|
   628                          ;   +-+     +-----+-----+-----+
   629                          ;    ^       ^     ^     ^     ^
   630                          ;    $16     $19   $1C   $1F   $22
   631                          ;    TSSP    TOSS
   632                          
   633                          DESCSTACK
   634  c748 a000               	LDY #0
   635  c74a 8423               	STY STRDP+1	; Zero descriptor pointer high
   636  c74c a900               	LDA #STAT_SDS	; Set status to SDS
   637  c74e 8557               	STA STAT
   638  c750 a219               	LDX #TOSS	; Start of SDS
   639  c752 d005               	BNE ISDSTEND	; branch always
   640  c754 a622               DSTACK	LDX STRDP
   641  c756 e8                 NEXTDST	INX		; next descriptor
   642  c757 e8                 	INX
   643  c758 e8                 	INX
   644                          ISDSTEND
   645  c759 e416               	CPX TSSP	; SDS finished?
   646  c75b f010               	BEQ VARS
   647  c75d b500               	LDA 0,X		; Check string length
   648  c75f f0f5               	BEQ NEXTDST
   649  c761 855d               	STA LEN		; Return variables:
   650  c763 8622               	STX STRDP	; length, descriptor address
   651  c765 b502               	LDA 2,X		; String address high
   652  c767 a8                 	TAY
   653  c768 b501               	LDA 1,X		; String address low
   654  c76a aa                 	TAX
   655  c76b 98                 	TYA		; Always not zero, Z=0
   656  c76c 60                 	RTS		; Returns address in X/Y
   657                          
   658                          ; *** Look up simple variables: VARTAB to ARYTAB
   659                          
   660  c76d a52d               VARS	LDA VARTAB	; Begin of variables
   661  c76f a62e               	LDX VARTAB+1
   662  c771 8522               	STA STRDP
   663  c773 8623               	STX STRDP+1
   664  c775 a004               	LDY #STAT_VAR	; Set status to variables
   665  c777 8457               	STY STAT
   666  c779 d00b               	BNE ISVAREND	; Branch always
   667                          VAR
   668  c77b 18                 NEXTVAR	CLC		; Next variable
   669  c77c a522               	LDA STRDP
   670  c77e 6907               	ADC #7		; Advance to next variable
   671  c780 8522               	STA STRDP
   672  c782 9002               	BCC ISVAREND
   673  c784 e623               	INC STRDP+1	; Overflow high byte
   674                          ISVAREND
   675  c786 c52f               	CMP ARYTAB
   676  c788 d006               	BNE CHECKVAR
   677  c78a a623               	LDX STRDP+1	; Variable end (=array start)?
   678  c78c e430               	CPX ARYTAB+1
   679  c78e f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   680                          CHECKVAR
   681  c790 a000               	LDY #0		; Variable name
   682  c792 b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   683  c794 30e5               	BMI NEXTVAR	; No string, to next variable
   684  c796 c8                 	INY
   685  c797 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   686  c799 10e0               	BPL NEXTVAR	; No string, to next variable
   687  c79b c8                 	INY
   688  c79c b122               	LDA (STRDP),Y	; String length
   689  c79e f0db               	BEQ NEXTVAR	; = 0, to next variable
   690  c7a0 d063               	BNE RETGETSA
   691                          
   692                          CHECKTYPE
   693  c7a2 a557               	LDA STAT	; GETSA intro with C=0
   694  c7a4 c901               	CMP #STAT_ARY	; String status?
   695  c7a6 f042               	BEQ ARRAY	; =1 -> arrays
   696  c7a8 b0d1               	BCS VAR		; =4 -> variables
   697  c7aa 4c54c7             	JMP DSTACK	; =0 -> SDS
   698                          
   699                          ; *** Look up arrays: ARYTAB to STREND
   700                          
   701  c7ad 855f               ARRAYS	STA PTR		; A/X set from simple variable processing,
   702  c7af 8660               	STX PTR+1	; pointing the start of arrays.
   703  c7b1 a001               	LDY #STAT_ARY
   704  c7b3 8457               	STY STAT	; Set status to arrays
   705                          ISARREND
   706  c7b5 a55f               	LDA PTR
   707  c7b7 a660               	LDX PTR+1
   708  c7b9 e432               CHKAEND	CPX STREND+1	; End of array area?
   709  c7bb d004                       BNE NEXTARR
   710  c7bd c531               	CMP STREND	; High byte matches, low byte is
   711                          			; less or equal.
   712  c7bf f04f               	BEQ NOSTRING	; Arrays finished -> no string
   713                          NEXTARR
   714                          			; Carry always cleared because of CPX/CMP
   715  c7c1 8522               	STA STRDP	; Start of an array
   716  c7c3 8623               	STX STRDP+1
   717  c7c5 a000               	LDY #0
   718  c7c7 b122               	LDA (STRDP),Y	; Array name
   719  c7c9 aa                 	TAX		; Array type, keep it for later
   720  c7ca c8                 	INY
   721  c7cb b122               	LDA (STRDP),Y
   722  c7cd 08                 	PHP		; Array type 2nd part, keep also
   723  c7ce c8                 	INY
   724  c7cf b122               	LDA (STRDP),Y	; Offset to next array
   725  c7d1 655f               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   726  c7d3 855f               	STA PTR		; Save start of following array
   727  c7d5 c8                 	INY
   728  c7d6 b122               	LDA (STRDP),Y
   729  c7d8 6560               	ADC PTR+1
   730  c7da 8560               	STA PTR+1
   731  c7dc 28                 	PLP		; Fetch array type
   732  c7dd 10d6               	BPL ISARREND	; Not a string array
   733  c7df 8a                 	TXA		; Fetch array type 2nd part
   734  c7e0 30d3               	BMI ISARREND	; Not string array
   735  c7e2 c8                 	INY
   736  c7e3 b122               	LDA (STRDP),Y	; Number of dimensions
   737  c7e5 0a                 	ASL		; *2
   738  c7e6 6905               	ADC #5		; Offset = dimensions*2+5
   739                          			; C=0 as long as dim.. <= 125
   740  c7e8 d003               	BNE ADVDESC	; Branch always
   741                          ARRAY			; Entry on continuation
   742                          NEXTASTR
   743  c7ea 18                 	CLC
   744  c7eb a903               	LDA #3		; String descriptor length
   745  c7ed 6522               ADVDESC	ADC STRDP	; Advance to next string
   746  c7ef 8522               	STA STRDP
   747  c7f1 9002               	BCC +
   748  c7f3 e623               	INC STRDP+1	; Overflow high byte
   749  c7f5 c55f               +	CMP PTR		; All array elements processed?
   750  c7f7 d006               	BNE IS0ASTR
   751  c7f9 a623               	LDX STRDP+1
   752  c7fb e460               	CPX PTR+1
   753  c7fd f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   754                          IS0ASTR
   755  c7ff a000               	LDY #0
   756  c801 b122               	LDA (STRDP),Y	; String length
   757  c803 f0e5               	BEQ NEXTASTR	; Next array element
   758                          RETGETSA
   759  c805 855d               	STA LEN		; Return value: length
   760  c807 c8                 	INY
   761  c808 b122               	LDA (STRDP),Y	; String address low
   762  c80a aa                 	TAX
   763  c80b c8                 	INY
   764  c80c b122               	LDA (STRDP),Y	; String address high
   765  c80e a8                 	TAY		; Always not zero, Z=0
   766  c80f 60                 	RTS		; Return address in X/Y
   767                          NOSTRING
   768  c810 a900               	LDA #0		; Length 0 
   769  c812 855d               	STA LEN		; No string found
   770  c814 60                 	RTS		; Z=1
   771                          
   772                          
   773                          
   774                          
   775                          !ifndef no_indicator {
   776  c815 00                 ORIGVID !byte 0		; Original character of marker position
   777  c816 00                 ORIGCOL !byte 0		; Original color of marker position
   778                          }
   779                          
   780                          
