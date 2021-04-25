
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
    19                          	*= $C000
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
    92                          			; Buffer:
    93                          !ifndef basic_rom_buffer {
    94                          BUF	 = KERNAL	; Buffer under KERNAL ROM
    95                          } else {
    96                          BUF	 = BASIC	; Buffer under BASIC ROM
    97                          }
    98                          BUFSIZE  = ROMSIZE	; Buffer size
    99                          
   100                          VIDPAGE	 = $288		; Page of video RAM
   101                          VIDBASE  = $0400	; Video RAM
   102                          COLBASE  = $D800	; Color RAM
   103                          
   104                          PROCPORT = $01		; Processor port
   105                          
   106                          
   107                          
   108                          ; Installer
   109                          
   110                          INSTALL
   111                          
   112  c500 2c                 	!byte $2C	; Opcode BIT absolute, Argument 
   113                          			; contains the signature, acts as NOP.
   114  c501 4743               	!text "GC"	; Signature for the loader,
   115                          			; the same an on a fixed
   116                          			; location for all variants!
   117                          
   118                          	; BASIC-ROM/RAM patch hook
   119                          
   120                          	; copy BASIC into RAM to patch the GC routine
   121  c503 a937               	LDA #MEMROM
   122  c505 8501               	STA PROCPORT	; All ROM (where to copy from)
   123  c507 a000               	LDY #<BASIC	; ROM start
   124  c509 8422               	STY CPTR
   125  c50b a9a0               	LDA #>BASIC
   126  c50d 8523               	STA CPTR+1	; BASIC ROM start
   127  c50f a220               	LDX #>($2000)	; BASIC ROM length in pages
   128  c511 b122               CPYROM	LDA (CPTR),Y	; Read from ROM
   129  c513 9122               	STA (CPTR),Y	; Write to RAM
   130  c515 c8                 	INY
   131  c516 d0f9               	BNE CPYROM
   132  c518 e623               	INC CPTR+1	; Next page
   133  c51a ca                 	DEX		; Page counter
   134  c51b d0f4               	BNE CPYROM
   135                          
   136  c51d a501               	LDA PROCPORT	; Switch to RAM
   137  c51f 29fe               	AND #%11111110	; "BASIC off" mask
   138  c521 8501               	STA PROCPORT
   139                          
   140  c523 a951               	LDA #<HANDLE1	; let JSR in place!
   141  c525 8d6daa             	STA PATCH1+1
   142  c528 a9c5               	LDA #>HANDLE1
   143  c52a 8d6eaa             	STA PATCH1+2
   144                          
   145  c52d a9a2               	LDA #<HANDLE2	; let JSR in place!
   146  c52f 8d6bb6             	STA PATCH2+1
   147  c532 a9c5               	LDA #>HANDLE2
   148  c534 8d6cb6             	STA PATCH2+2
   149                          
   150  c537 a9bb               	LDA #<HANDLE3	; let JSR in place!
   151  c539 8d27b7             	STA PATCH3+1
   152  c53c a9c5               	LDA #>HANDLE3
   153  c53e 8d28b7             	STA PATCH3+2
   154                          
   155  c541 a9e1               	LDA #<COLLECT	; Write "JMP COLLECT"
   156  c543 8d27b5             	STA GARBCOL+1	; patch code.
   157  c546 a9c5               	LDA #>COLLECT
   158  c548 8d28b5             	STA GARBCOL+2
   159                          
   160  c54b a94c               	LDA #$4C	; The "JMP" opcode
   161  c54d 8d26b5             	STA GARBCOL
   162  c550 60                 	RTS
   163                          
   164                          
   165                          ; *** Handle Patch 1: LET variable overwrite
   166                          ;
   167                          ; Hooks at AA6C
   168                          ; Replacing:
   169                          ;	JSR $B6DB	; Remove only from SDS, but keep string on heap!
   170                          ; Continues at AA6F:
   171                          ;	LDY #$00
   172                          ;	LDA ($50),Y
   173                          ;	STA ($49),Y
   174                          
   175                          HANDLE1
   176  c551 c418               	CPY $18		; Descriptor on top of SDS?
   177  c553 d00a               	BNE +
   178  c555 c517               	CMP $17
   179  c557 d006               	BNE +
   180  c559 8516               	STA $16		; Yes, remove it from SDS
   181  c55b e903               	SBC #3
   182  c55d 8517               	STA $17
   183                          	
   184                          	; If destination variable points to string on the heap, free it.
   185                          
   186  c55f a000               +	LDY #0
   187                          	; $49 points to variable descriptor (in LET's destination variable)
   188  c561 b149               	LDA ($49),Y	; Get string length
   189  c563 f03c               	BEQ LEAVE	; Variable contains no string
   190  c565 aa                 	TAX		; > 0, save it for later
   191  c566 c8                 	INY
   192  c567 b149               	LDA ($49),Y	; String address low
   193  c569 855a               	STA STR
   194  c56b c8                 	INY
   195  c56c b149               	LDA ($49),Y	; String address high
   196  c56e 855b               	STA STR+1
   197                          
   198                          	; Free STR if on heap and return
   199                          
   200                          FREE
   201  c570 a000               	LDY #0
   202  c572 a55b               	LDA STR+1	; String address high
   203  c574 c534               	CMP FRETOP+1	; Heap top high
   204  c576 9029               	BCC LEAVE	; String below heap (on on heap)
   205  c578 d008               	BNE ++		; String on heap
   206  c57a a55a               	LDA STR		; String address low
   207  c57c c533               	CMP FRETOP	; Heap top low
   208  c57e 9021               	BCC LEAVE	; Leave when not on heap!
   209                          
   210  c580 a55b               	LDA STR+1	; String address greater or equal FRETOP
   211                          
   212  c582 c538               ++	CMP MEMEND+1	; String above string memory?
   213  c584 9008               	BCC +++		; no
   214  c586 d019               	BNE LEAVE	; yes
   215  c588 a55a               	LDA STR		; High byte equal, compare low byte
   216  c58a c537               	CMP MEMEND
   217  c58c b013               	BCS LEAVE	; Above heap
   218                          	
   219                          	; String on heap: mark it as free
   220                          
   221  c58e 8a                 +++	TXA		; Restore length
   222  c58f c901               	CMP #1		; String of length 1?
   223  c591 d004               	BNE ++
   224                          
   225  c593 915a               	STA (STR),Y	; Single byte on heap contains 1
   226  c595 f00a               	BEQ LEAVE	; leave, always (Z=1)
   227                          
   228  c597 a8                 ++	TAY		; Length to Y (> 1!)
   229  c598 88                 	DEY
   230  c599 88                 	DEY		; Y: Length - 2
   231  c59a 915a               	STA (STR),Y	; Pre-last byte of string has length
   232  c59c c8                 	INY
   233  c59d a9ff               	LDA #$FF
   234  c59f 915a               	STA (STR),Y	; Last byte of string with gap-marker
   235  c5a1 60                 LEAVE	RTS
   236                          
   237                          
   238                          
   239                          ; String concatenation: free 2nd argument after copying!
   240                          
   241                          ;.,B65D 20 75 B4 JSR $B475       copy descriptor pointer and make string space A bytes long
   242                          ;.,B660 20 7A B6 JSR $B67A       copy string from descriptor to utility pointer
   243                          ;.,B663 A5 50    LDA $50         get descriptor pointer low byte
   244                          ;.,B665 A4 51    LDY $51         get descriptor pointer high byte
   245                          ;.,B667 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
   246                          ;                                returns with A = length, X = pointer low byte,
   247                          ;                                Y = pointer high byte
   248                          ;.,B66A 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   249                          ;.,B66D A5 6F    LDA $6F         get descriptor pointer low byte
   250                          ;.,B66F A4 70    LDY $70         get descriptor pointer high byte
   251                          ;.,B671 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of string space
   252                          ;                                returns with A = length, X = pointer low byte,
   253                          ;                                Y = pointer high byte
   254                          ;.,B674 20 CA B4 JSR $B4CA       check space on descriptor stack then put string address
   255                          ;                                and length on descriptor stack and update stack pointers
   256                          ;.,B677 4C B8 AD JMP $ADB8       continue evaluation
   257                          
   258                          ; -> 
   259                          
   260                          ;.,B66A 20 8C B6 JSR HANDLE2     store string from pointer to utility pointer
   261                          ;
   262                          HANDLE2
   263  c5a2 208cb6             	JSR $B68C	; Copy string to utility pointer's location
   264  c5a5 a550               	LDA $50		; Descriptor address of 2nd argument
   265  c5a7 a451               	LDY $51		; It is never top on heap, so just mark it as free
   266  c5a9 c516               	CMP $16		; Previously popped element
   267  c5ab d0f4               	BNE LEAVE
   268  c5ad c418               	CPY $18		; High byte (normally 0)
   269  c5af d0f0               	BNE LEAVE
   270  c5b1 20d2c5             	JSR FREESDS	; mark already remove element from SDS as free
   271  c5b4 a56f               	LDA $6F
   272  c5b6 a470               	LDY $70
   273  c5b8 4cc2c5             	JMP POPSDS	; remove element from SDS and mark as free
   274                          	
   275                          
   276                          ; LEFT$(), RIGHT$(), MID$(): Free input string
   277                          
   278                          ;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   279                          ;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put string address
   280                          ;                                and length on descriptor stack and update stack pointers
   281                          ; -> 
   282                          ;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer
   283                          
   284                          
   285                          HANDLE3
   286                          	; A: length, copy from ($22) to ($35)
   287  c5bb 208cb6             	JSR $B68C	; Copy string part into allocated space
   288  c5be a550               	LDA $50
   289  c5c0 a451               	LDY $51
   290                          
   291                          	; the string itself is not top of heap, just mark as free and remove from SDS
   292                          
   293                          POPSDS
   294  c5c2 c418               	CPY $18		; Descriptor on top of SDS?
   295  c5c4 d0db               	BNE LEAVE	; RTS
   296  c5c6 c517               	CMP $17
   297  c5c8 d0d7               	BNE LEAVE	; RTS
   298                          	; free memory and pull from SDS
   299  c5ca 20d2c5             	JSR FREESDS
   300  c5cd a517               	LDA $17		; Top elememt on SDS
   301  c5cf 4ce3b6             	JMP $B6E3	; remove from SDS (A low byte to SDS element)
   302                          FREESDS
   303                          	; A/Y is pointer to string descriptor on the SDS!
   304  c5d2 aa                 	TAX		; Index in zero-page
   305  c5d3 b501               	LDA 1,X		; String address low
   306  c5d5 855a               	STA STR
   307  c5d7 b502               	LDA 2,X		; String address high
   308  c5d9 855b               	STA STR+1
   309  c5db b500               	LDA 0,X		; String length
   310  c5dd aa                 	TAX
   311  c5de 4c70c5             	JMP FREE	; Length X, address STR/STR+1
   312                          
   313                          
   314                          
   315                          ; *** Garbage Collector
   316                          
   317                          COLLECT
   318                          
   319                          !ifndef no_indicator {
   320  c5e1 a200               	LDX #0
   321  c5e3 8622               	STX CPTR	; Pointer low byte = 0
   322  c5e5 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   323                          	!if (>MARKOFF) >= 1 {
   324  c5e8 e8                 	INX
   325                          	!if (>MARKOFF) >= 2 {
   326  c5e9 e8                 	INX
   327                          	!if (>MARKOFF) >= 3 {
   328  c5ea e8                 	INX
   329                          	} } }
   330                          	; X contains now the page plus the offset's high byte
   331  c5eb 8623               	STX CPTR+1
   332  c5ed a0e7               	LDY #<(MARKOFF)
   333  c5ef b122               	LDA (CPTR),Y	; Activity indicator on screen:
   334  c5f1 8d13c8             	STA ORIGVID	; Save current character
   335  c5f4 a92a               	LDA #MARKCHAR
   336  c5f6 9122               	STA (CPTR),Y	; Set mark character
   337  c5f8 ade7db             	LDA MARKCPOS	; Same for the color information
   338  c5fb 8d14c8             	STA ORIGCOL	; Save current color
   339  c5fe a909               	LDA #MARKCOL
   340  c600 8de7db             	STA MARKCPOS	; Set mark color
   341                          }
   342                          
   343                          
   344                          
   345                          ; walk through all strings and reorganize them
   346                          
   347                          STAGE1
   348  c603 38                         SEC             ; Initialize search
   349                          NEXTSTR
   350  c604 2044c7             	JSR GETSA
   351  c607 f052                       BEQ STAGE2      ; No String found anymore
   352                                                  ; Address in X/Y
   353                          
   354  c609 c434               	CPY FRETOP+1	; String on heap?
   355  c60b 90f7               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   356  c60d d004               	BNE +
   357  c60f e433               	CPX FRETOP
   358  c611 90f1               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   359                          
   360  c613 865a               +	STX STR		; Start of string which is on heap
   361  c615 845b               	STY STR+1
   362  c617 a55d               	LDA LEN
   363  c619 c901               	CMP #1		; String length 1?
   364  c61b d014               	BNE ++
   365                          
   366                          	; LEN 1: 
   367                          	;	copy string value into descriptor
   368                          	;	overwrite string on heap with value 1
   369                          
   370  c61d a000               	LDY #0
   371  c61f b15a               	LDA (STR),Y	; String value
   372  c621 aa                 	TAX
   373  c622 a901               	LDA #1		; Marker for string with length 1
   374  c624 915a               	STA (STR),Y	; Store marker on heap
   375  c626 a557               	LDA STAT
   376  c628 4a                 	LSR		; Shift right gives offset, which
   377  c629 a8                 	TAY		; refers to STRDP leading to the descriptor
   378  c62a c8                 	INY		; Position string address low byte
   379  c62b 8a                 	TXA		; String value
   380  c62c 9122               	STA (STRDP),Y	; Store value in descriptor (low address byte)
   381  c62e 18                 	CLC		; Continuation mode for GETSA
   382  c62f 90d3               	BCC NEXTSTR	; Always
   383                          
   384                          	; LEN >1:
   385                          	;	copy backlink bytes to descriptor
   386                          	;	store descriptor pointer to backlink
   387                          
   388  c631 a8                 ++	TAY		; Length
   389  c632 88                 	DEY		; Index to last byte
   390  c633 b15a               	LDA (STR),Y
   391  c635 48                 	PHA		; Last byte of string
   392  c636 a623               	LDX STRDP+1
   393  c638 a557               	LDA STAT
   394  c63a 4a                 	LSR		; Shift right gives offset to the descriptor
   395  c63b 18                 	CLC
   396  c63c 6522               	ADC STRDP
   397  c63e 9001               	BCC +
   398  c640 e8                 	INX
   399  c641 48                 +	PHA		; STRDP + offset low
   400  c642 8a                 	TXA		; X STRDP + offset high
   401  c643 915a               	STA (STR),Y	; Back-link high
   402  c645 88                 	DEY
   403  c646 b15a               	LDA (STR),Y	; Pre-last byte string
   404  c648 aa                 	TAX
   405  c649 68                 	PLA		; STRDP + offset low
   406  c64a 915a               	STA (STR),Y	; Back-link low
   407  c64c a557               	LDA STAT
   408  c64e 4a                 	LSR		; Shift right gives offset, which
   409  c64f a8                 	TAY		; refers to STRDP leading to the descriptor
   410  c650 c8                 	INY		; Skip length byte
   411  c651 68                 	PLA		; Last byte of string
   412  c652 9122               	STA (STRDP),Y	; Store into descriptor address low byte
   413  c654 8a                 	TXA		; Pre-last byte of string
   414  c655 c8                 	INY		; =2
   415  c656 9122               	STA (STRDP),Y	; Store into descriptor address high byte
   416  c658 18                 	CLC		; Continuation mode for GETSA
   417  c659 90a9               	BCC NEXTSTR	; Always
   418                          	
   419                          
   420                          
   421                          
   422                          ; walk through heap, remove gaps and move strings
   423                          
   424                          STAGE2
   425  c65b a438               	LDY MEMEND+1	; Top of memory.
   426  c65d a637               	LDX MEMEND	; Set new heap top
   427  c65f 8622               	STX NEWHEAP	; to memory end.
   428  c661 8423               	STY NEWHEAP+1
   429                          			; Entry point from no-gap part
   430  c663 8460               LOOP2R	STY PTR+1	; PTR comes from X
   431  c665 a000               	LDY #0
   432                          LOOP2
   433  c667 8a                 	TXA		; PTR minus 1
   434  c668 d002               	BNE +
   435  c66a c660               	DEC PTR+1
   436  c66c ca                 +	DEX
   437  c66d 865f               -	STX PTR
   438                          
   439  c66f e433               	CPX HEAP	; PTR blow top of heap?
   440  c671 a560               	LDA PTR+1
   441  c673 e534               	SBC HEAP+1
   442  c675 b003               	BCS +		; PTR >= HEAP
   443  c677 4cf9c6             	JMP EXIT2
   444                          +
   445  c67a b15f               	LDA (PTR),Y	; Get back-link high
   446  c67c c901               	CMP #1		; 1-byte gap
   447  c67e f0e7               	BEQ LOOP2	; Skip it, covered later in stage 3.
   448                          
   449  c680 e8                 	INX		; Decrement PTR, but leaving A untouched
   450  c681 ca                 	DEX		; PTR low = 0?
   451  c682 d002               	BNE +
   452  c684 c660               	DEC PTR+1
   453  c686 ca                 +	DEX		; PTR low
   454  c687 865f               	STX PTR
   455                          
   456  c689 c9ff               	CMP #$FF	; Gap marker? (length >1)
   457  c68b d00f               	BNE NOGAP
   458                          			; Skip gap of a certain length ...
   459  c68d b15f               	LDA (PTR),Y	; Gap length
   460  c68f 49ff               	EOR #$FF	; A is > 1
   461                          			; Carry set from CMP above!
   462  c691 6901               	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
   463                          			; Never 0 because gap length > 1
   464  c693 655f               	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
   465  c695 aa                 	TAX		; PTR low byte
   466  c696 b0d5               	BCS -		; Position on last string byte
   467  c698 c660               	DEC PTR+1	; PTR high byte, always >0
   468  c69a d0d1               	BNE -		; Always, PTR has string address,
   469                          			; pointing to last string byte
   470                          
   471                          ; We have a backlink to the string:
   472  c69c 8559               NOGAP	STA DESC+1	; Backlink high and
   473  c69e b15f               	LDA (PTR),Y	; backlink low is the
   474  c6a0 8558               	STA DESC	; descriptor address.
   475                          
   476  c6a2 b158               	LDA (DESC),Y	; Length from descriptor
   477  c6a4 49ff               	EOR #$FF
   478  c6a6 48                 	PHA		; Needed for heap later
   479  c6a7 a660               	LDX PTR+1	; Transfer to STR ...
   480                          			; Carry clear from CMP #$FF
   481  c6a9 6903               	ADC #3		; -(LEN-2) + PTR -> PTR
   482  c6ab f005               	BEQ +		; PTR already in position
   483                          			; A before add. was in range 0 to FC
   484                          			; which never sets the carry!
   485  c6ad 655f               	ADC PTR
   486  c6af b001               	BCS +
   487  c6b1 ca                 	DEX
   488  c6b2 865b               +	STX STR+1	; STR points to string start.
   489  c6b4 855a               	STA STR
   490                          	
   491                          	; make space on heap vor LEN bytes
   492  c6b6 68                 	PLA		; LEN, but only complemented
   493  c6b7 38                 	SEC		; Finalize two's complement (+1 from carry)
   494  c6b8 6522               	ADC NEWHEAP	; HEAP - LEN -> HEAP
   495  c6ba 8522               	STA NEWHEAP
   496  c6bc b002               	BCS +
   497  c6be c623               	DEC NEWHEAP+1
   498                          +	
   499                          	; copy LEN bytes from STR to HEAP
   500  c6c0 b158               	LDA (DESC),Y	; length from descriptor
   501  c6c2 a8                 	TAY		; as index
   502  c6c3 88                 	DEY		; index = length - 2
   503  c6c4 88                 	DEY
   504  c6c5 f00e               	BEQ +		; 0, nothing to copy
   505  c6c7 88                 	DEY		; -1, index of last byte
   506  c6c8 f007               	BEQ ++		; No loop if index is 0.
   507  c6ca b15a               -	LDA (STR),Y	; Transfer byte 1 to len-1
   508  c6cc 9122               	STA (NEWHEAP),Y
   509  c6ce 88                 	DEY
   510  c6cf d0f9               	BNE -
   511  c6d1 b15a               ++	LDA (STR),Y	; transfer byte 0
   512  c6d3 9122               	STA (NEWHEAP),Y
   513                          +	
   514                          	; correct descriptor
   515  c6d5 a002               	LDY #2		; Offset in descriptor
   516  c6d7 b158               	LDA (DESC),Y	; pre-last string byte 
   517  c6d9 48                 	PHA		; Save
   518  c6da a523               	LDA NEWHEAP+1
   519  c6dc 9158               	STA (DESC),Y	; Restore string address low
   520  c6de 88                 	DEY
   521  c6df b158               	LDA (DESC),Y	; last string byte
   522  c6e1 48                 	PHA		; Save
   523  c6e2 a522               	LDA NEWHEAP	; Restore string address high
   524  c6e4 9158               	STA (DESC),Y	; Backlink high
   525                          
   526  c6e6 88                 	DEY		; Y=0
   527                          	; Restore string bytes to backlink
   528  c6e7 b158               	LDA (DESC),Y	; Length byte
   529  c6e9 a8                 	TAY
   530  c6ea 88                 	DEY		; Index of last string byte
   531  c6eb 68                 	PLA
   532  c6ec 9122               	STA (NEWHEAP),Y	; last byte
   533  c6ee 88                 	DEY
   534  c6ef 68                 	PLA		
   535  c6f0 9122               	STA (NEWHEAP),Y	; pre-last byte
   536                          
   537  c6f2 a65a               	LDX STR		; PTR low byte in X
   538  c6f4 a45b               	LDY STR+1	; always >0
   539  c6f6 4c63c6             	JMP LOOP2R	; Loop with set PTR and reset Y
   540                          	
   541                          EXIT2
   542  c6f9 a522               	LDA NEWHEAP	; Set rebuilt, compacted heap
   543  c6fb 8533               	STA HEAP	; as new heap.
   544  c6fd a523               	LDA NEWHEAP+1
   545  c6ff 8534               	STA HEAP+1
   546                          
   547                          
   548                          
   549                          
   550                          ; Put strings with length 1 (stored in the descriptor) back on heap
   551                          
   552                          STAGE3
   553  c701 38                         SEC             ; Initialize search for GETSA
   554  c702 24                         !byte $24       ; BIT ZP, skip next instruction
   555                          NEXT1STR
   556  c703 18                 	CLC
   557  c704 2044c7             	JSR GETSA
   558  c707 f021                       BEQ EXIT        ; No String found anymore
   559                                                  ; Address in X/Y, descr. at STRDP
   560  c709 c65d               	DEC LEN
   561  c70b d0f6               	BNE NEXT1STR	; Loop if not length 1
   562                          	
   563  c70d 8a                 	TXA		; String addr low is the string byte!
   564  c70e a000               	LDY #0
   565  c710 a633               	LDX HEAP
   566  c712 d002               	BNE +		; Heap pointer - 1
   567  c714 c634               	DEC HEAP+1
   568  c716 ca                 +	DEX		; Low byte used later
   569  c717 8633               	STX HEAP
   570  c719 9133               	STA (HEAP),Y	; stored string byte back to heap
   571                          
   572  c71b a557               	LDA STAT
   573  c71d 4a                 	LSR		; Shift right gives offset, which
   574  c71e a8                 	TAY		; refers to STRDP leading to the descriptor
   575  c71f c8                 	INY		; Low byte address in descriptor
   576  c720 8a                 	TXA		; Heap pointer low
   577  c721 9122               	STA (STRDP),Y	; stored back into descriptor
   578  c723 c8                 	INY
   579  c724 a534               	LDA HEAP+1	; Heap pointer high
   580  c726 9122               	STA (STRDP),Y	; stored back into descriptor
   581  c728 d0d9               	BNE NEXT1STR	; Branch always, because high byte >0
   582                          	
   583                          
   584                          ; *** Garbage collection finished
   585                          
   586                          EXIT
   587                          
   588                          !ifndef no_indicator {
   589  c72a a200               	LDX #0
   590  c72c 8622                       STX CPTR        ; Pointer low byte = 0
   591  c72e ae8802                     LDX VIDPAGE     ; Startpage of video RAM
   592                                  !if (>MARKOFF) >= 1 {
   593  c731 e8                         INX
   594                                  !if (>MARKOFF) >= 2 {
   595  c732 e8                         INX
   596                                  !if (>MARKOFF) >= 3 {
   597  c733 e8                         INX
   598                                  } } }
   599                                  ; X contains now the page plus the offset's high byte
   600  c734 8623                       STX CPTR+1
   601  c736 a0e7                       LDY #<(MARKOFF)
   602  c738 ad13c8                     LDA ORIGVID     ; Clear activation indicator:
   603  c73b 9122                       STA (CPTR),Y    ; restore character on screen
   604  c73d ad14c8                     LDA ORIGCOL     ; and its color.
   605  c740 8de7db                     STA MARKCPOS
   606                          }
   607  c743 60                 	RTS
   608                          
   609                          
   610                          ;
   611                          ; *** Get String - fetch next string with length > 0
   612                          ;
   613                          ; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
   614                          ; 
   615                          ; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
   616                          ;
   617                          ; If C=1 start from the beginning at SDS, otherwise
   618                          ; continue with position STRDP and string status STAT.
   619                          ; If the Z-Flag is set no string is available,
   620                          ; otherwise X/Y contains the address and LEN
   621                          ; the length of the string.
   622                          
   623  c744 905a               GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
   624                                                  ; otherwise start with at SDS ...
   625                          
   626                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   627                          ;
   628                          ;    +-------------+
   629                          ;    |             V
   630                          ;    |    belegt->|<-frei
   631                          ;   +-+     +-----+-----+-----+
   632                          ;   | |     |S|L|H|S|L|H|S|L|H|
   633                          ;   +-+     +-----+-----+-----+
   634                          ;    ^       ^     ^     ^     ^
   635                          ;    $16     $19   $1C   $1F   $22
   636                          ;    TSSP    TOSS
   637                          
   638                          DESCSTACK
   639  c746 a000               	LDY #0
   640  c748 8423               	STY STRDP+1	; Zero descriptor pointer high
   641  c74a a900               	LDA #STAT_SDS	; Set status to SDS
   642  c74c 8557               	STA STAT
   643  c74e a219               	LDX #TOSS	; Start of SDS
   644  c750 d005               	BNE ISDSTEND	; branch always
   645  c752 a622               DSTACK	LDX STRDP
   646  c754 e8                 NEXTDST	INX		; next descriptor
   647  c755 e8                 	INX
   648  c756 e8                 	INX
   649                          ISDSTEND
   650  c757 e416               	CPX TSSP	; SDS finished?
   651  c759 f010               	BEQ VARS
   652  c75b b500               	LDA 0,X		; Check string length
   653  c75d f0f5               	BEQ NEXTDST
   654  c75f 855d               	STA LEN		; Return variables:
   655  c761 8622               	STX STRDP	; length, descriptor address
   656  c763 b502               	LDA 2,X		; String address high
   657  c765 a8                 	TAY
   658  c766 b501               	LDA 1,X		; String address low
   659  c768 aa                 	TAX
   660  c769 98                 	TYA		; Always not zero, Z=0
   661  c76a 60                 	RTS		; Returns address in X/Y
   662                          
   663                          ; *** Look up simple variables: VARTAB to ARYTAB
   664                          
   665  c76b a52d               VARS	LDA VARTAB	; Begin of variables
   666  c76d a62e               	LDX VARTAB+1
   667  c76f 8522               	STA STRDP
   668  c771 8623               	STX STRDP+1
   669  c773 a004               	LDY #STAT_VAR	; Set status to variables
   670  c775 8457               	STY STAT
   671  c777 d00b               	BNE ISVAREND	; Branch always
   672                          VAR
   673  c779 18                 NEXTVAR	CLC		; Next variable
   674  c77a a522               	LDA STRDP
   675  c77c 6907               	ADC #7		; Advance to next variable
   676  c77e 8522               	STA STRDP
   677  c780 9002               	BCC ISVAREND
   678  c782 e623               	INC STRDP+1	; Overflow high byte
   679                          ISVAREND
   680  c784 c52f               	CMP ARYTAB
   681  c786 d006               	BNE CHECKVAR
   682  c788 a623               	LDX STRDP+1	; Variable end (=array start)?
   683  c78a e430               	CPX ARYTAB+1
   684  c78c f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   685                          CHECKVAR
   686  c78e a000               	LDY #0		; Variable name
   687  c790 b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   688  c792 30e5               	BMI NEXTVAR	; No string, to next variable
   689  c794 c8                 	INY
   690  c795 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   691  c797 10e0               	BPL NEXTVAR	; No string, to next variable
   692  c799 c8                 	INY
   693  c79a b122               	LDA (STRDP),Y	; String length
   694  c79c f0db               	BEQ NEXTVAR	; = 0, to next variable
   695  c79e d063               	BNE RETGETSA
   696                          
   697                          CHECKTYPE
   698  c7a0 a557               	LDA STAT	; GETSA intro with C=0
   699  c7a2 c901               	CMP #STAT_ARY	; String status?
   700  c7a4 f042               	BEQ ARRAY	; =1 -> arrays
   701  c7a6 b0d1               	BCS VAR		; =4 -> variables
   702  c7a8 4c52c7             	JMP DSTACK	; =0 -> SDS
   703                          
   704                          ; *** Look up arrays: ARYTAB to STREND
   705                          
   706  c7ab 855f               ARRAYS	STA PTR		; A/X set from simple variable processing,
   707  c7ad 8660               	STX PTR+1	; pointing the start of arrays.
   708  c7af a001               	LDY #STAT_ARY
   709  c7b1 8457               	STY STAT	; Set status to arrays
   710                          ISARREND
   711  c7b3 a55f               	LDA PTR
   712  c7b5 a660               	LDX PTR+1
   713  c7b7 e432               CHKAEND	CPX STREND+1	; End of array area?
   714  c7b9 d004                       BNE NEXTARR
   715  c7bb c531               	CMP STREND	; High byte matches, low byte is
   716                          			; less or equal.
   717  c7bd f04f               	BEQ NOSTRING	; Arrays finished -> no string
   718                          NEXTARR
   719                          			; Carry always cleared because of CPX/CMP
   720  c7bf 8522               	STA STRDP	; Start of an array
   721  c7c1 8623               	STX STRDP+1
   722  c7c3 a000               	LDY #0
   723  c7c5 b122               	LDA (STRDP),Y	; Array name
   724  c7c7 aa                 	TAX		; Array type, keep it for later
   725  c7c8 c8                 	INY
   726  c7c9 b122               	LDA (STRDP),Y
   727  c7cb 08                 	PHP		; Array type 2nd part, keep also
   728  c7cc c8                 	INY
   729  c7cd b122               	LDA (STRDP),Y	; Offset to next array
   730  c7cf 655f               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   731  c7d1 855f               	STA PTR		; Save start of following array
   732  c7d3 c8                 	INY
   733  c7d4 b122               	LDA (STRDP),Y
   734  c7d6 6560               	ADC PTR+1
   735  c7d8 8560               	STA PTR+1
   736  c7da 28                 	PLP		; Fetch array type
   737  c7db 10d6               	BPL ISARREND	; Not a string array
   738  c7dd 8a                 	TXA		; Fetch array type 2nd part
   739  c7de 30d3               	BMI ISARREND	; Not string array
   740  c7e0 c8                 	INY
   741  c7e1 b122               	LDA (STRDP),Y	; Number of dimensions
   742  c7e3 0a                 	ASL		; *2
   743  c7e4 6905               	ADC #5		; Offset = dimensions*2+5
   744                          			; C=0 as long as dim.. <= 125
   745  c7e6 d003               	BNE ADVDESC	; Branch always
   746                          ARRAY			; Entry on continuation
   747                          NEXTASTR
   748  c7e8 18                 	CLC
   749  c7e9 a903               	LDA #3		; String descriptor length
   750  c7eb 6522               ADVDESC	ADC STRDP	; Advance to next string
   751  c7ed 8522               	STA STRDP
   752  c7ef 9002               	BCC +
   753  c7f1 e623               	INC STRDP+1	; Overflow high byte
   754  c7f3 c55f               +	CMP PTR		; All array elements processed?
   755  c7f5 d006               	BNE IS0ASTR
   756  c7f7 a623               	LDX STRDP+1
   757  c7f9 e460               	CPX PTR+1
   758  c7fb f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   759                          IS0ASTR
   760  c7fd a000               	LDY #0
   761  c7ff b122               	LDA (STRDP),Y	; String length
   762  c801 f0e5               	BEQ NEXTASTR	; Next array element
   763                          RETGETSA
   764  c803 855d               	STA LEN		; Return value: length
   765  c805 c8                 	INY
   766  c806 b122               	LDA (STRDP),Y	; String address low
   767  c808 aa                 	TAX
   768  c809 c8                 	INY
   769  c80a b122               	LDA (STRDP),Y	; String address high
   770  c80c a8                 	TAY		; Always not zero, Z=0
   771  c80d 60                 	RTS		; Return address in X/Y
   772                          NOSTRING
   773  c80e a900               	LDA #0		; Length 0 
   774  c810 855d               	STA LEN		; No string found
   775  c812 60                 	RTS		; Z=1
   776                          
   777                          
   778                          
   779                          
   780                          !ifndef no_indicator {
   781  c813 00                 ORIGVID !byte 0		; Original character of marker position
   782  c814 00                 ORIGCOL !byte 0		; Original color of marker position
   783                          }
   784                          
   785                          
