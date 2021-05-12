
; ******** Source: blod-gc.asm
     1                          ;
     2                          ; **********************************
     3                          ; *  BACKLINK GARBAGE  COLLECTION  *
     4                          ; *        from Johann Klasek      *
     5                          ; *        j AT klasek DOT at      *
     6                          ; *       2021-03-30 VERS. 1.1     *
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
   327  c5f1 8d1ec8             	STA ORIGVID	; Save current character
   328  c5f4 a92a               	LDA #MARKCHAR
   329  c5f6 9122               	STA (CPTR),Y	; Set mark character
   330  c5f8 ade7db             	LDA MARKCPOS	; Same for the color information
   331  c5fb 8d1fc8             	STA ORIGCOL	; Save current color
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
   343  c604 204cc7             	JSR GETSA
   344  c607 f057               	BEQ STAGE2      ; No String found anymore
   345                          			; Address in X/Y, descr. at STRDP + STAT-offset
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
   357  c61b d019               	BNE ++
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
   374  c62e a900               	LDA #0		; Mark for high byte, means on heap string
   375  c630 c8                 	INY		; String address high byte
   376  c631 9122               	STA (STRDP),Y	; Set to zero
   377  c633 18                 	CLC		; Continuation mode for GETSA
   378  c634 90ce               	BCC NEXTSTR	; Always
   379                          
   380                          	; LEN >1:
   381                          	;	copy backlink bytes to descriptor
   382                          	;	store descriptor pointer to backlink
   383                          
   384  c636 a8                 ++	TAY		; Length
   385  c637 88                 	DEY		; Index to last byte
   386  c638 b15a               	LDA (STR),Y
   387  c63a 48                 	PHA		; Last byte of string
   388  c63b a623               	LDX STRDP+1
   389  c63d a557               	LDA STAT
   390  c63f 4a                 	LSR		; Shift right gives offset to the descriptor
   391  c640 18                 	CLC
   392  c641 6522               	ADC STRDP
   393  c643 9001               	BCC +
   394  c645 e8                 	INX
   395  c646 48                 +	PHA		; STRDP + offset low
   396  c647 8a                 	TXA		; X STRDP + offset high
   397  c648 915a               	STA (STR),Y	; Back-link high
   398  c64a 88                 	DEY
   399  c64b b15a               	LDA (STR),Y	; Pre-last byte string
   400  c64d aa                 	TAX
   401  c64e 68                 	PLA		; STRDP + offset low
   402  c64f 915a               	STA (STR),Y	; Back-link low
   403  c651 a557               	LDA STAT
   404  c653 4a                 	LSR		; Shift right gives offset, which
   405  c654 a8                 	TAY		; refers to STRDP leading to the descriptor
   406  c655 c8                 	INY		; Skip length byte
   407  c656 68                 	PLA		; Last byte of string
   408  c657 9122               	STA (STRDP),Y	; Store into descriptor address low byte
   409  c659 8a                 	TXA		; Pre-last byte of string
   410  c65a c8                 	INY		; =2
   411  c65b 9122               	STA (STRDP),Y	; Store into descriptor address high byte
   412  c65d 18                 	CLC		; Continuation mode for GETSA
   413  c65e 90a4               	BCC NEXTSTR	; Always
   414                          	
   415                          
   416                          
   417                          
   418                          ; walk through heap, remove gaps and move strings
   419                          
   420                          STAGE2
   421  c660 a438               	LDY MEMEND+1	; Top of memory.
   422  c662 a637               	LDX MEMEND	; Set new heap top
   423  c664 8622               	STX NEWHEAP	; to memory end.
   424  c666 8423               	STY NEWHEAP+1
   425                          			; Entry point from no-gap part
   426  c668 8460               LOOP2R	STY PTR+1	; PTR comes from X
   427  c66a a000               	LDY #0
   428                          LOOP2
   429  c66c 8a                 	TXA		; PTR minus 1
   430  c66d d002               	BNE +
   431  c66f c660               	DEC PTR+1
   432  c671 ca                 +	DEX
   433  c672 865f               -	STX PTR
   434                          
   435  c674 e433               	CPX HEAP	; PTR blow top of heap?
   436  c676 a560               	LDA PTR+1
   437  c678 e534               	SBC HEAP+1
   438  c67a b003               	BCS +		; PTR >= HEAP
   439  c67c 4c00c7             	JMP EXIT2
   440                          +
   441  c67f b15f               	LDA (PTR),Y	; Get back-link high
   442  c681 c901               	CMP #1		; 1-byte gap
   443  c683 f0e7               	BEQ LOOP2	; Skip it, covered later in stage 3.
   444                          
   445  c685 e8                 	INX		; Decrement PTR, but leaving A untouched
   446  c686 ca                 	DEX		; PTR low = 0?
   447  c687 d002               	BNE +
   448  c689 c660               	DEC PTR+1
   449  c68b ca                 +	DEX		; PTR low
   450  c68c 865f               	STX PTR
   451                          
   452  c68e c9ff               	CMP #$FF	; Gap marker? (length >1)
   453  c690 d00f               	BNE NOGAP
   454                          			; Skip gap of a certain length ...
   455  c692 b15f               	LDA (PTR),Y	; Gap length
   456  c694 49ff               	EOR #$FF	; A is > 1
   457                          			; Carry set from CMP above!
   458  c696 6901               	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
   459                          			; Never 0 because gap length > 1
   460  c698 655f               	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
   461  c69a aa                 	TAX		; PTR low byte
   462  c69b b0d5               	BCS -		; Position on last string byte
   463  c69d c660               	DEC PTR+1	; PTR high byte, always >0
   464  c69f d0d1               	BNE -		; Always, PTR has string address,
   465                          			; pointing to last string byte
   466                          
   467                          ; We have a backlink to the string:
   468  c6a1 8559               NOGAP	STA DESC+1	; Backlink high and
   469  c6a3 b15f               	LDA (PTR),Y	; backlink low is the
   470  c6a5 8558               	STA DESC	; descriptor address.
   471                          
   472  c6a7 b158               	LDA (DESC),Y	; Length from descriptor
   473  c6a9 49ff               	EOR #$FF
   474  c6ab 48                 	PHA		; Needed for heap later
   475  c6ac a660               	LDX PTR+1	; Transfer to STR ...
   476                          			; Carry clear from CMP #$FF
   477  c6ae 6903               	ADC #3		; -(LEN-2) + PTR -> PTR
   478  c6b0 d002               	BNE +		; PTR already in position
   479                          			; Special case length = 2:
   480  c6b2 e8                 	INX		; compensate for the high byte decrement
   481  c6b3 18                 	CLC		; Adding 0 with carry cleared, leaves PTR unchanged.
   482  c6b4 655f               +	ADC PTR		; Accumulator before add. was in range 0 to FC
   483                          			; which never sets the carry!
   484  c6b6 b001               	BCS +
   485  c6b8 ca                 	DEX		; In case of adding 0 X is already compensated.
   486  c6b9 865b               +	STX STR+1	; STR points to string start.
   487  c6bb 855a               	STA STR
   488                          	
   489                          	; make space on heap vor LEN bytes
   490  c6bd 68                 	PLA		; LEN, but only complemented
   491  c6be 38                 	SEC		; Finalize two's complement (+1 from carry)
   492  c6bf 6522               	ADC NEWHEAP	; HEAP - LEN -> HEAP
   493  c6c1 8522               	STA NEWHEAP
   494  c6c3 b002               	BCS +
   495  c6c5 c623               	DEC NEWHEAP+1
   496                          +	
   497                          	; copy LEN bytes from STR to HEAP
   498  c6c7 b158               	LDA (DESC),Y	; length from descriptor
   499  c6c9 a8                 	TAY		; as index
   500  c6ca 88                 	DEY		; index = length - 2
   501  c6cb 88                 	DEY
   502  c6cc f00e               	BEQ +		; 0, nothing to copy
   503  c6ce 88                 	DEY		; -1, index of last byte
   504  c6cf f007               	BEQ ++		; No loop if index is 0.
   505  c6d1 b15a               -	LDA (STR),Y	; Transfer byte 1 to len-1
   506  c6d3 9122               	STA (NEWHEAP),Y
   507  c6d5 88                 	DEY
   508  c6d6 d0f9               	BNE -
   509  c6d8 b15a               ++	LDA (STR),Y	; transfer byte 0
   510  c6da 9122               	STA (NEWHEAP),Y
   511                          +	
   512                          	; correct descriptor
   513  c6dc a002               	LDY #2		; Offset in descriptor
   514  c6de b158               	LDA (DESC),Y	; pre-last string byte 
   515  c6e0 48                 	PHA		; Save
   516  c6e1 a523               	LDA NEWHEAP+1
   517  c6e3 9158               	STA (DESC),Y	; Restore string address low
   518  c6e5 88                 	DEY
   519  c6e6 b158               	LDA (DESC),Y	; last string byte
   520  c6e8 48                 	PHA		; Save
   521  c6e9 a522               	LDA NEWHEAP	; Restore string address high
   522  c6eb 9158               	STA (DESC),Y	; Backlink high
   523                          
   524  c6ed 88                 	DEY		; Y=0
   525                          	; Restore string bytes to backlink
   526  c6ee b158               	LDA (DESC),Y	; Length byte
   527  c6f0 a8                 	TAY
   528  c6f1 88                 	DEY		; Index of last string byte
   529  c6f2 68                 	PLA
   530  c6f3 9122               	STA (NEWHEAP),Y	; last byte
   531  c6f5 88                 	DEY
   532  c6f6 68                 	PLA		
   533  c6f7 9122               	STA (NEWHEAP),Y	; pre-last byte
   534                          
   535  c6f9 a65a               	LDX STR		; PTR low byte in X
   536  c6fb a45b               	LDY STR+1	; always >0
   537  c6fd 4c68c6             	JMP LOOP2R	; Loop with set PTR and reset Y
   538                          	
   539                          EXIT2
   540  c700 a522               	LDA NEWHEAP	; Set rebuilt, compacted heap
   541  c702 8533               	STA HEAP	; as new heap.
   542  c704 a523               	LDA NEWHEAP+1
   543  c706 8534               	STA HEAP+1
   544                          
   545                          
   546                          
   547                          
   548                          ; Put strings with length 1 (stored in the descriptor) back on heap
   549                          
   550                          STAGE3
   551  c708 38                         SEC             ; Initialize search for GETSA
   552  c709 24                         !byte $24       ; BIT ZP, skip next instruction
   553                          NEXT1STR
   554  c70a 18                 	CLC
   555  c70b 204cc7             	JSR GETSA
   556  c70e f022               	BEQ EXIT        ; No String found anymore
   557                          			; Address in X/Y, descr. at STRDP + STAT-offset
   558  c710 c65d               	DEC LEN
   559  c712 d0f6               	BNE NEXT1STR	; Loop if not length 1
   560  c714 98                 	TYA		; Check string address high byte
   561  c715 d0f3               	BNE NEXT1STR	; If not zero, string is not on heap!
   562                          			; Y is always 0.	
   563  c717 8a                 	TXA		; String addr low is the string byte!
   564  c718 a633               	LDX HEAP
   565  c71a d002               	BNE +		; Heap pointer - 1
   566  c71c c634               	DEC HEAP+1
   567  c71e ca                 +	DEX		; Low byte used later
   568  c71f 8633               	STX HEAP
   569  c721 9133               	STA (HEAP),Y	; stored string byte back to heap
   570                          
   571  c723 a557               	LDA STAT
   572  c725 4a                 	LSR		; Shift right gives offset, which
   573  c726 a8                 	TAY		; refers to STRDP leading to the descriptor
   574  c727 c8                 	INY		; Low byte address in descriptor
   575  c728 8a                 	TXA		; Heap pointer low
   576  c729 9122               	STA (STRDP),Y	; stored back into descriptor
   577  c72b c8                 	INY
   578  c72c a534               	LDA HEAP+1	; Heap pointer high
   579  c72e 9122               	STA (STRDP),Y	; stored back into descriptor
   580  c730 d0d8               	BNE NEXT1STR	; Branch always, because high byte >0
   581                          	
   582                          
   583                          ; *** Garbage collection finished
   584                          
   585                          EXIT
   586                          
   587                          !ifndef no_indicator {
   588  c732 a200               	LDX #0
   589  c734 8622                       STX CPTR        ; Pointer low byte = 0
   590  c736 ae8802                     LDX VIDPAGE     ; Startpage of video RAM
   591                                  !if (>MARKOFF) >= 1 {
   592  c739 e8                         INX
   593                                  !if (>MARKOFF) >= 2 {
   594  c73a e8                         INX
   595                                  !if (>MARKOFF) >= 3 {
   596  c73b e8                         INX
   597                                  } } }
   598                                  ; X contains now the page plus the offset's high byte
   599  c73c 8623                       STX CPTR+1
   600  c73e a0e7                       LDY #<(MARKOFF)
   601  c740 ad1ec8                     LDA ORIGVID     ; Clear activation indicator:
   602  c743 9122                       STA (CPTR),Y    ; restore character on screen
   603  c745 ad1fc8                     LDA ORIGCOL     ; and its color.
   604  c748 8de7db                     STA MARKCPOS
   605                          }
   606  c74b 60                 	RTS
   607                          
   608                          
   609                          ;
   610                          ; *** Get String - fetch next string with length > 0
   611                          ;
   612                          ; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
   613                          ; 
   614                          ; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
   615                          ;
   616                          ; If C=1 start from the beginning at SDS, otherwise
   617                          ; continue with position STRDP and string status STAT.
   618                          ; If the Z-Flag is set no string is available,
   619                          ; otherwise X/Y contains the address and LEN
   620                          ; the length of the string.
   621                          
   622  c74c 905b               GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
   623                                                  ; otherwise start with at SDS ...
   624                          
   625                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   626                          ;
   627                          ;    +-------------+
   628                          ;    |             V
   629                          ;    |    belegt->|<-frei
   630                          ;   +-+     +-----+-----+-----+
   631                          ;   | |     |S|L|H|S|L|H|S|L|H|
   632                          ;   +-+     +-----+-----+-----+
   633                          ;    ^       ^     ^     ^     ^
   634                          ;    $16     $19   $1C   $1F   $22
   635                          ;    TSSP    TOSS
   636                          
   637                          DESCSTACK
   638  c74e a000               	LDY #0
   639  c750 8423               	STY STRDP+1	; Zero descriptor pointer high
   640  c752 a900               	LDA #STAT_SDS	; Set status to SDS
   641  c754 8557               	STA STAT
   642  c756 a219               	LDX #TOSS	; Start of SDS
   643  c758 d005               	BNE ISDSTEND	; branch always
   644  c75a a622               DSTACK	LDX STRDP
   645  c75c e8                 NEXTDST	INX		; next descriptor
   646  c75d e8                 	INX
   647  c75e e8                 	INX
   648                          ISDSTEND
   649  c75f e416               	CPX TSSP	; SDS finished?
   650  c761 f011               	BEQ VARS
   651  c763 b500               	LDA 0,X		; Check string length
   652  c765 f0f5               	BEQ NEXTDST
   653  c767 855d               	STA LEN		; Return variables:
   654  c769 8622               	STX STRDP	; length, descriptor address
   655  c76b b502               	LDA 2,X		; String address high
   656  c76d a8                 	TAY
   657  c76e b501               	LDA 1,X		; String address low
   658  c770 aa                 	TAX
   659  c771 a55d               	LDA LEN		; Always not zero, Z=0
   660  c773 60                 	RTS		; Returns address in X/Y
   661                          
   662                          ; *** Look up simple variables: VARTAB to ARYTAB
   663                          
   664  c774 a52d               VARS	LDA VARTAB	; Begin of variables
   665  c776 a62e               	LDX VARTAB+1
   666  c778 8522               	STA STRDP
   667  c77a 8623               	STX STRDP+1
   668  c77c a004               	LDY #STAT_VAR	; Set status to variables
   669  c77e 8457               	STY STAT
   670  c780 d00b               	BNE ISVAREND	; Branch always
   671                          VAR
   672  c782 18                 NEXTVAR	CLC		; Next variable
   673  c783 a522               	LDA STRDP
   674  c785 6907               	ADC #7		; Advance to next variable
   675  c787 8522               	STA STRDP
   676  c789 9002               	BCC ISVAREND
   677  c78b e623               	INC STRDP+1	; Overflow high byte
   678                          ISVAREND
   679  c78d c52f               	CMP ARYTAB
   680  c78f d006               	BNE CHECKVAR
   681  c791 a623               	LDX STRDP+1	; Variable end (=array start)?
   682  c793 e430               	CPX ARYTAB+1
   683  c795 f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   684                          CHECKVAR
   685  c797 a000               	LDY #0		; Variable name
   686  c799 b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   687  c79b 30e5               	BMI NEXTVAR	; No string, to next variable
   688  c79d c8                 	INY
   689  c79e b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   690  c7a0 10e0               	BPL NEXTVAR	; No string, to next variable
   691  c7a2 c8                 	INY
   692  c7a3 b122               	LDA (STRDP),Y	; String length
   693  c7a5 f0db               	BEQ NEXTVAR	; = 0, to next variable
   694  c7a7 d063               	BNE RETGETSA
   695                          
   696                          CHECKTYPE
   697  c7a9 a557               	LDA STAT	; GETSA intro with C=0
   698  c7ab c901               	CMP #STAT_ARY	; String status?
   699  c7ad f042               	BEQ ARRAY	; =1 -> arrays
   700  c7af b0d1               	BCS VAR		; =4 -> variables
   701  c7b1 4c5ac7             	JMP DSTACK	; =0 -> SDS
   702                          
   703                          ; *** Look up arrays: ARYTAB to STREND
   704                          
   705  c7b4 855f               ARRAYS	STA PTR		; A/X set from simple variable processing,
   706  c7b6 8660               	STX PTR+1	; pointing the start of arrays.
   707  c7b8 a001               	LDY #STAT_ARY
   708  c7ba 8457               	STY STAT	; Set status to arrays
   709                          ISARREND
   710  c7bc a55f               	LDA PTR
   711  c7be a660               	LDX PTR+1
   712  c7c0 e432               CHKAEND	CPX STREND+1	; End of array area?
   713  c7c2 d004                       BNE NEXTARR
   714  c7c4 c531               	CMP STREND	; High byte matches, low byte is
   715                          			; less or equal.
   716  c7c6 f051               	BEQ NOSTRING	; Arrays finished -> no string
   717                          NEXTARR
   718                          			; Carry always cleared because of CPX/CMP
   719  c7c8 8522               	STA STRDP	; Start of an array
   720  c7ca 8623               	STX STRDP+1
   721  c7cc a000               	LDY #0
   722  c7ce b122               	LDA (STRDP),Y	; Array name
   723  c7d0 aa                 	TAX		; Array type, keep it for later
   724  c7d1 c8                 	INY
   725  c7d2 b122               	LDA (STRDP),Y
   726  c7d4 08                 	PHP		; Array type 2nd part, keep also
   727  c7d5 c8                 	INY
   728  c7d6 b122               	LDA (STRDP),Y	; Offset to next array
   729  c7d8 655f               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   730  c7da 855f               	STA PTR		; Save start of following array
   731  c7dc c8                 	INY
   732  c7dd b122               	LDA (STRDP),Y
   733  c7df 6560               	ADC PTR+1
   734  c7e1 8560               	STA PTR+1
   735  c7e3 28                 	PLP		; Fetch array type
   736  c7e4 10d6               	BPL ISARREND	; Not a string array
   737  c7e6 8a                 	TXA		; Fetch array type 2nd part
   738  c7e7 30d3               	BMI ISARREND	; Not string array
   739  c7e9 c8                 	INY
   740  c7ea b122               	LDA (STRDP),Y	; Number of dimensions
   741  c7ec 0a                 	ASL		; *2
   742  c7ed 6905               	ADC #5		; Offset = dimensions*2+5
   743                          			; C=0 as long as dim.. <= 125
   744  c7ef d003               	BNE ADVDESC	; Branch always
   745                          ARRAY			; Entry on continuation
   746                          NEXTASTR
   747  c7f1 18                 	CLC
   748  c7f2 a903               	LDA #3		; String descriptor length
   749  c7f4 6522               ADVDESC	ADC STRDP	; Advance to next string
   750  c7f6 8522               	STA STRDP
   751  c7f8 9002               	BCC +
   752  c7fa e623               	INC STRDP+1	; Overflow high byte
   753  c7fc c55f               +	CMP PTR		; All array elements processed?
   754  c7fe d006               	BNE IS0ASTR
   755  c800 a623               	LDX STRDP+1
   756  c802 e460               	CPX PTR+1
   757  c804 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   758                          IS0ASTR
   759  c806 a000               	LDY #0
   760  c808 b122               	LDA (STRDP),Y	; String length
   761  c80a f0e5               	BEQ NEXTASTR	; Next array element
   762                          RETGETSA
   763  c80c 855d               	STA LEN		; Return value: length
   764  c80e c8                 	INY
   765  c80f b122               	LDA (STRDP),Y	; String address low
   766  c811 aa                 	TAX
   767  c812 c8                 	INY
   768  c813 b122               	LDA (STRDP),Y	; String address high
   769  c815 a8                 	TAY
   770  c816 a55d               	LDA LEN		; Always not zero, Z=0
   771  c818 60                 	RTS		; Return address in X/Y
   772                          NOSTRING
   773  c819 a900               	LDA #0		; Length 0 
   774  c81b 855d               	STA LEN		; No string found
   775  c81d 60                 	RTS		; Z=1
   776                          
   777                          
   778                          
   779                          
   780                          !ifndef no_indicator {
   781  c81e 00                 ORIGVID !byte 0		; Original character of marker position
   782  c81f 00                 ORIGCOL !byte 0		; Original color of marker position
   783                          }
   784                          
   785                          
