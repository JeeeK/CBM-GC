
; ******** Source: blod-gc.asm
     1                          ;
     2                          ; **********************************
     3                          ; *  BACKLINK GARBAGE  COLLECTION  *
     4                          ; *        from Johann Klasek      *
     5                          ; *        j AT klasek DOT at      *
     6                          ; *            2021-03-30          *
     7                          ; *       2024-06-09 VERS. 1.2     *
     8                          ; **********************************
     9                          ;
    10                          ; Collects unused (garbage) strings on the string heap,
    11                          ; replacing the BASIC 2.0 garbage collector on a C64.
    12                          ; Only those locations which is used by the legacy garbage
    13                          ; collector are in use here.
    14                          
    15                          ; Start of code ...
    16                          
    17                          !ifdef start {
    18                          	*=start
    19                          } else {
    20                          	*= $C500
    21                          }
    22                          
    23                          ; Options:
    24                          
    25                          ; Do not display an activiation mark on screen
    26                          ;no_indicator = 1
    27                          
    28                          
    29                          
    30                          ; BASIC system variables
    31                          
    32                          TOSS     = $19		; Top of String Descriptor Stack
    33                          EOSS     = $22		; End of String Descriptor Stack +1
    34                          TSSP     = $16		; Current String Descriptor Stack pointer
    35                          
    36                          VARTAB   = $2D		; End of BASIC program = begin of variable area
    37                          ARYTAB   = $2F		; End of variables = begin of arrays
    38                          STREND   = $31		; End of arrays = lowest possible string heap address
    39                          FRETOP   = $33		; Current string heap address
    40                          MEMSIZ   = $37		; Highest RAM address for BASIC, start of
    41                          			; string heap growing downwards.
    42                          MEMBEG   = STREND	; String heap memory begin = STREND
    43                          MEMEND   = MEMSIZ	; String heap memory end
    44                          
    45                          
    46                          ; variables
    47                          
    48                          HEAP     = FRETOP	; String pointer = FRETOP
    49                          STRDP    = $22		; String descriptor address (used in stage 1+3: GETSA in/out)
    50                          CPTR     = $22		; Pointer for installer routine (used in installer)
    51                          NEWHEAP  = $22		; New heap pointer (used in stage 2)
    52                          
    53                          STAT     = $57		; String status, for values in use,
    54                          			; see STAT_* below (GETSA in/out)
    55                          DESC     = $58		; String descriptor address (temp.)
    56                          STR      = $5A		; Points to a string
    57                          LEN      = $5D		; String length (GETSA out)
    58                          PTR      = $5F		; Array pointer (GETSA in/out)
    59                          
    60                          
    61                          
    62                          ; Constants
    63                          
    64                          ; for variable STAT (string status):
    65                          STAT_SDS = 0		; Is on String Descriptor Stack
    66                          STAT_VAR = 4		; Is a simple variable
    67                          STAT_ARY = 1		; Is in a array
    68                          
    69                          ; Memory configuration for PROCPORT:
    70                          MEMROM   = %00110111	; BASIC+KERNAL ROM, $37
    71                          MEMBAS   = %00110110	; BASIC RAM+KERNAL ROM, $34
    72                          MEMRAM   = %00110101	; BASIC+KERNAL RAM, $35
    73                          
    74                          ; for activity indicator
    75                          MARKCHAR = "*"          ; Indicator character
    76                          MARKCOL  = 9            ; Indicator color (red)
    77                          MARKOFF  = 40*25-1      ; indicator position (lower right corner)
    78                          MARKVPOS = VIDBASE+MARKOFF
    79                          MARKCPOS = COLBASE+MARKOFF
    80                          
    81                          
    82                          ; Memory locations
    83                          
    84                          GARBCOL  = $B526	; Entry point of the legacy GC
    85                          PATCH1   = $AA6C	; Overwrite string to variable
    86                          PATCH2   = $B66A	; String concatenation: 2nd argument handling
    87                          PATCH3   = $B726	; LEFT$() copy string
    88                          
    89                          BASIC    = $A000        ; BASIC ROM
    90                          KERNAL   = $E000        ; KERNAL ROM
    91                          ROMSIZE  = $2000        ; ROM length, 8 Kbyte
    92                          
    93                          VIDPAGE	 = $288		; Page of video RAM
    94                          VIDBASE  = $0400	; Video RAM
    95                          COLBASE  = $D800	; Color RAM
    96                          
    97                          PROCPORT = $01		; Processor port
    98                          
    99                          
   100                          
   101                          ; Installer
   102                          
   103                          INSTALL
   104                          
   105  c500 2c                 	!byte $2C	; Opcode BIT absolute, Argument 
   106                          			; contains the signature, acts as NOP.
   107  c501 4743               	!text "GC"	; Signature for the loader,
   108                          			; the same an on a fixed
   109                          			; location for all variants!
   110                          
   111                          	; BASIC-ROM/RAM patch hook
   112                          
   113                          	; copy BASIC into RAM to patch the GC routine
   114  c503 a937               	LDA #MEMROM
   115  c505 8501               	STA PROCPORT	; All ROM (where to copy from)
   116  c507 a000               	LDY #<BASIC	; ROM start
   117  c509 8422               	STY CPTR
   118  c50b a9a0               	LDA #>BASIC
   119  c50d 8523               	STA CPTR+1	; BASIC ROM start
   120  c50f a220               	LDX #>($2000)	; BASIC ROM length in pages
   121  c511 b122               CPYROM	LDA (CPTR),Y	; Read from ROM
   122  c513 9122               	STA (CPTR),Y	; Write to RAM
   123  c515 c8                 	INY
   124  c516 d0f9               	BNE CPYROM
   125  c518 e623               	INC CPTR+1	; Next page
   126  c51a ca                 	DEX		; Page counter
   127  c51b d0f4               	BNE CPYROM
   128                          
   129  c51d a501               	LDA PROCPORT	; Switch to RAM
   130  c51f 29fe               	AND #%11111110	; "BASIC off" mask
   131  c521 8501               	STA PROCPORT
   132                          
   133  c523 a951               	LDA #<HANDLE1	; let JSR in place!
   134  c525 8d6daa             	STA PATCH1+1
   135  c528 a9c5               	LDA #>HANDLE1
   136  c52a 8d6eaa             	STA PATCH1+2
   137                          
   138  c52d a995               	LDA #<HANDLE2	; let JSR in place!
   139  c52f 8d6bb6             	STA PATCH2+1
   140  c532 a9c5               	LDA #>HANDLE2
   141  c534 8d6cb6             	STA PATCH2+2
   142                          
   143  c537 a9ae               	LDA #<HANDLE3	; let JSR in place!
   144  c539 8d27b7             	STA PATCH3+1
   145  c53c a9c5               	LDA #>HANDLE3
   146  c53e 8d28b7             	STA PATCH3+2
   147                          
   148  c541 a9d4               	LDA #<COLLECT	; Write "JMP COLLECT"
   149  c543 8d27b5             	STA GARBCOL+1	; patch code.
   150  c546 a9c5               	LDA #>COLLECT
   151  c548 8d28b5             	STA GARBCOL+2
   152                          
   153  c54b a94c               	LDA #$4C	; The "JMP" opcode
   154  c54d 8d26b5             	STA GARBCOL
   155  c550 60                 	RTS
   156                          
   157                          
   158                          ; *** Handle Patch 1: LET variable overwrite
   159                          ;
   160                          ; Hooks at AA6C
   161                          ; Replacing:
   162                          ;	JSR $B6DB	; Remove only from SDS, but keep string on heap!
   163                          ; Continues at AA6F:
   164                          ;	LDY #$00
   165                          ;	LDA ($50),Y
   166                          ;	STA ($49),Y
   167                          
   168                          HANDLE1
   169  c551 20dbb6             	JSR $B6DB	; Remove descriptor if on top, corresponds to:
   170                          ;	CPY $18		; Descriptor on top of SDS?
   171                          ;	BNE +
   172                          ;	CMP $17
   173                          ;	BNE +
   174                          ;	STA $16		; Yes, remove it from SDS
   175                          ;	SBC #3
   176                          ;	STA $17
   177                          ;+	LDY #0
   178                          	
   179                          	; If destination variable points to string on the heap, free it.
   180                          
   181                          	; $49 points to variable descriptor (in LET's destination variable)
   182  c554 b149               	LDA ($49),Y	; Get string length
   183  c556 f03c               	BEQ LEAVE	; Variable contains no string
   184  c558 aa                 	TAX		; > 0, save it for later
   185  c559 c8                 	INY
   186  c55a b149               	LDA ($49),Y	; String address low
   187  c55c 855a               	STA STR
   188  c55e c8                 	INY
   189  c55f b149               	LDA ($49),Y	; String address high
   190  c561 855b               	STA STR+1
   191                          
   192                          	; Free STR if on heap and return
   193                          
   194                          FREE
   195  c563 a55b               	LDA STR+1	; String address high
   196  c565 c534               	CMP FRETOP+1	; Heap top high
   197  c567 902b               	BCC LEAVE	; String below heap (on on heap)
   198  c569 d008               	BNE ++		; String on heap
   199  c56b a55a               	LDA STR		; String address low
   200  c56d c533               	CMP FRETOP	; Heap top low
   201  c56f 9023               	BCC LEAVE	; Leave when not on heap!
   202                          
   203  c571 a55b               	LDA STR+1	; String address greater or equal FRETOP
   204                          
   205  c573 c538               ++	CMP MEMEND+1	; String above string memory?
   206  c575 9008               	BCC +++		; no
   207  c577 d01b               	BNE LEAVE	; yes
   208  c579 a55a               	LDA STR		; High byte equal, compare low byte
   209  c57b c537               	CMP MEMEND
   210  c57d b015               	BCS LEAVE	; Above heap
   211                          	
   212                          	; String on heap: mark it as free
   213                          
   214  c57f 8a                 +++	TXA		; Restore length
   215  c580 c901               	CMP #1		; String of length 1?
   216  c582 d006               	BNE ++
   217                          
   218  c584 a000               	LDY #0
   219  c586 915a               	STA (STR),Y	; Single byte on heap contains 1
   220  c588 f00a               	BEQ LEAVE	; leave, always (Z=1)
   221                          
   222  c58a a8                 ++	TAY		; Length to Y (> 1!)
   223  c58b 88                 	DEY
   224  c58c 88                 	DEY		; Y: Length - 2
   225  c58d 915a               	STA (STR),Y	; Pre-last byte of string has length
   226  c58f c8                 	INY
   227  c590 a9ff               	LDA #$FF
   228  c592 915a               	STA (STR),Y	; Last byte of string with gap-marker
   229  c594 60                 LEAVE	RTS
   230                          
   231                          
   232                          
   233                          ; String concatenation: 2nd argument is freed before copying!
   234                          
   235                          ;.,B65D 20 75 B4 JSR $B475       copy descriptor pointer and make string space
   236                          ;                                A bytes long
   237                          ;.,B660 20 7A B6 JSR $B67A       copy first string from descriptor to utility pointer
   238                          ;.,B663 A5 50    LDA $50         get second string's descriptor pointer low byte
   239                          ;.,B665 A4 51    LDY $51         get second string's descriptor pointer high byte
   240                          ;.,B667 20 AA B6 JSR $B6AA       pop (YA) 2nd descriptor off stack or from top of
   241                          ;                                string space returns with A = length,
   242                          ;                                X = pointer low byte, Y = pointer high byte
   243                          ;  >>>>>>>>>>>>>>>>>>>>>>>       PATCH here ...
   244                          ;.,B66A 20 8C B6 JSR $B68C       store 2nd string from pointer to utility pointer
   245                          ;  <<<<<<<<<<<<<<<<<<<<<<<
   246                          ;.,B66D A5 6F    LDA $6F         get first string's descriptor pointer low byte
   247                          ;.,B66F A4 70    LDY $70         get first string's descriptor pointer high byte
   248                          ;.,B671 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of
   249                          ;                                string space returns with A = length,
   250                          ;                                X = pointer low byte, Y = pointer high byte
   251                          ;.,B674 20 CA B4 JSR $B4CA       check space on descriptor stack then put
   252                          ;                                string address and length on descriptor stack
   253                          ;                                and update stack pointers
   254                          ;.,B677 4C B8 AD JMP $ADB8       continue evaluation
   255                          
   256                          ; -> 
   257                          
   258                          ;.,B66A 20 8C B6 JSR HANDLE2     store string from pointer to utility pointer
   259                          
   260                          ; Both arguments, or just one or none might lay on the SDS, e.g. if one
   261                          ; of the operands is a string variable, its descriptor is used directly.
   262                          ;
   263                          ; On entry the first string is already copied.
   264                          
   265                          HANDLE2
   266  c595 208cb6             	JSR $B68C	; Copy 2nd string to utility pointer's location
   267  c598 a550               	LDA $50		; Descriptor address of 2nd argument
   268  c59a a451               	LDY $51		; It is never top on heap, so just mark it as free
   269  c59c c516               	CMP $16		; Previously popped element if it was on the SDS
   270  c59e d007               	BNE +
   271  c5a0 c418               	CPY $18		; High byte (normally 0)
   272  c5a2 d003               	BNE +
   273  c5a4 20c5c5             	JSR FREESDS	; Mark already remove element from SDS as free
   274  c5a7 a56f               +	LDA $6F		; Descriptor address of the first argument
   275  c5a9 a470               	LDY $70
   276  c5ab 4cb5c5             	JMP POPSDS	; Remove element from SDS and mark as free
   277                          
   278                          
   279                          
   280                          ; LEFT$(), RIGHT$(), MID$(): Free input string
   281                          
   282                          ;  >>>>>>>>>>>>>>>>>>>>>>>       PATCH here ...
   283                          ;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   284                          ;  <<<<<<<<<<<<<<<<<<<<<<<
   285                          ;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put
   286                          ;                                string address and length on descriptor stack
   287                          ;                                and update stack pointers
   288                          ; -> 
   289                          ;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer
   290                          
   291                          HANDLE3
   292                          	; A: length, copy from ($22) to ($35)
   293  c5ae 208cb6             	JSR $B68C	; Copy string part into allocated space
   294  c5b1 a550               	LDA $50
   295  c5b3 a451               	LDY $51
   296                          
   297                          	; the string itself is not top of heap, just mark as free
   298                          	; and remove from SDS
   299                          
   300                          POPSDS
   301  c5b5 c418               	CPY $18		; Descriptor on top of SDS?
   302  c5b7 d0db               	BNE LEAVE	; RTS
   303  c5b9 c517               	CMP $17
   304  c5bb d0d7               	BNE LEAVE	; RTS
   305                          	; free memory and pull from SDS
   306  c5bd 20c5c5             	JSR FREESDS	; Carry = 1 CMP above, left untouched after JSR
   307  c5c0 a517               	LDA $17		; Top element on SDS
   308  c5c2 4ce3b6             	JMP $B6E3	; Remove from SDS (A has low byte to SDS element)
   309                          			; Carry flag must be set on entry.
   310                          FREESDS
   311                          	; A/Y is pointer to string descriptor on the SDS!
   312  c5c5 aa                 	TAX		; Index in zero-page
   313  c5c6 b501               	LDA 1,X		; String address low
   314  c5c8 855a               	STA STR
   315  c5ca b502               	LDA 2,X		; String address high
   316  c5cc 855b               	STA STR+1
   317  c5ce b500               	LDA 0,X		; String length
   318  c5d0 aa                 	TAX
   319  c5d1 d090               	BNE FREE	; Length X, address STR/STR+1
   320  c5d3 60                 	RTS		; No free if length = 0!
   321                          
   322                          
   323                          
   324                          ; *** Garbage Collector
   325                          
   326                          COLLECT
   327                          
   328                          !ifndef no_indicator {
   329  c5d4 a200               	LDX #0
   330  c5d6 8622               	STX CPTR	; Pointer low byte = 0
   331  c5d8 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   332                          	!if (>MARKOFF) >= 1 {
   333  c5db e8                 	INX
   334                          	!if (>MARKOFF) >= 2 {
   335  c5dc e8                 	INX
   336                          	!if (>MARKOFF) >= 3 {
   337  c5dd e8                 	INX
   338                          	} } }
   339                          	; X contains now the page plus the offset's high byte
   340  c5de 8623               	STX CPTR+1
   341  c5e0 a0e7               	LDY #<(MARKOFF)
   342  c5e2 b122               	LDA (CPTR),Y	; Activity indicator on screen:
   343  c5e4 8d11c8             	STA ORIGVID	; Save current character
   344  c5e7 a92a               	LDA #MARKCHAR
   345  c5e9 9122               	STA (CPTR),Y	; Set mark character
   346  c5eb ade7db             	LDA MARKCPOS	; Same for the color information
   347  c5ee 8d12c8             	STA ORIGCOL	; Save current color
   348  c5f1 a909               	LDA #MARKCOL
   349  c5f3 8de7db             	STA MARKCPOS	; Set mark color
   350                          }
   351                          
   352                          
   353                          
   354                          ; walk through all strings and reorganize them
   355                          
   356                          STAGE1
   357  c5f6 38                         SEC             ; Initialize search
   358                          NEXTSTR
   359  c5f7 203fc7             	JSR GETSA
   360  c5fa f057               	BEQ STAGE2      ; No String found anymore
   361                          			; Address in X/Y, descr. at STRDP + STAT-offset
   362                          
   363  c5fc c434               	CPY FRETOP+1	; String on heap?
   364  c5fe 90f7               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   365  c600 d004               	BNE +
   366  c602 e433               	CPX FRETOP
   367  c604 90f1               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   368                          
   369  c606 865a               +	STX STR		; Start of string which is on heap
   370  c608 845b               	STY STR+1
   371  c60a a55d               	LDA LEN
   372  c60c c901               	CMP #1		; String length 1?
   373  c60e d019               	BNE ++
   374                          
   375                          	; LEN 1: 
   376                          	;	copy string value into descriptor
   377                          	;	overwrite string on heap with value 1
   378                          
   379  c610 a000               	LDY #0
   380  c612 b15a               	LDA (STR),Y	; String value
   381  c614 aa                 	TAX
   382  c615 a901               	LDA #1		; Marker for string with length 1
   383  c617 915a               	STA (STR),Y	; Store marker on heap
   384  c619 a557               	LDA STAT
   385  c61b 4a                 	LSR		; Shift right gives offset, which
   386  c61c a8                 	TAY		; refers to STRDP leading to the descriptor
   387  c61d c8                 	INY		; Position string address low byte
   388  c61e 8a                 	TXA		; String value
   389  c61f 9122               	STA (STRDP),Y	; Store value in descriptor (low address byte)
   390  c621 a900               	LDA #0		; Mark for high byte, means on heap string
   391  c623 c8                 	INY		; String address high byte
   392  c624 9122               	STA (STRDP),Y	; Set to zero
   393  c626 18                 	CLC		; Continuation mode for GETSA
   394  c627 90ce               	BCC NEXTSTR	; Always
   395                          
   396                          	; LEN >1:
   397                          	;	copy backlink bytes to descriptor
   398                          	;	store descriptor pointer to backlink
   399                          
   400  c629 a8                 ++	TAY		; Length
   401  c62a 88                 	DEY		; Index to last byte
   402  c62b b15a               	LDA (STR),Y
   403  c62d 48                 	PHA		; Last byte of string
   404  c62e a623               	LDX STRDP+1
   405  c630 a557               	LDA STAT
   406  c632 4a                 	LSR		; Shift right gives offset to the descriptor
   407  c633 18                 	CLC
   408  c634 6522               	ADC STRDP
   409  c636 9001               	BCC +
   410  c638 e8                 	INX
   411  c639 48                 +	PHA		; STRDP + offset low
   412  c63a 8a                 	TXA		; X STRDP + offset high
   413  c63b 915a               	STA (STR),Y	; Back-link high
   414  c63d 88                 	DEY
   415  c63e b15a               	LDA (STR),Y	; Pre-last byte string
   416  c640 aa                 	TAX
   417  c641 68                 	PLA		; STRDP + offset low
   418  c642 915a               	STA (STR),Y	; Back-link low
   419  c644 a557               	LDA STAT
   420  c646 4a                 	LSR		; Shift right gives offset, which
   421  c647 a8                 	TAY		; refers to STRDP leading to the descriptor
   422  c648 c8                 	INY		; Skip length byte
   423  c649 68                 	PLA		; Last byte of string
   424  c64a 9122               	STA (STRDP),Y	; Store into descriptor address low byte
   425  c64c 8a                 	TXA		; Pre-last byte of string
   426  c64d c8                 	INY		; =2
   427  c64e 9122               	STA (STRDP),Y	; Store into descriptor address high byte
   428  c650 18                 	CLC		; Continuation mode for GETSA
   429  c651 90a4               	BCC NEXTSTR	; Always
   430                          	
   431                          
   432                          
   433                          
   434                          ; walk through heap, remove gaps and move strings
   435                          
   436                          STAGE2
   437  c653 a438               	LDY MEMEND+1	; Top of memory.
   438  c655 a637               	LDX MEMEND	; Set new heap top
   439  c657 8622               	STX NEWHEAP	; to memory end.
   440  c659 8423               	STY NEWHEAP+1
   441                          			; Entry point from no-gap part
   442  c65b 8460               LOOP2R	STY PTR+1	; PTR comes from X
   443  c65d a000               	LDY #0
   444                          LOOP2
   445  c65f 8a                 	TXA		; PTR minus 1
   446  c660 d002               	BNE +
   447  c662 c660               	DEC PTR+1
   448  c664 ca                 +	DEX
   449  c665 865f               -	STX PTR
   450                          
   451  c667 e433               	CPX HEAP	; PTR below top of heap?
   452  c669 a560               	LDA PTR+1
   453  c66b e534               	SBC HEAP+1
   454  c66d b003               	BCS +		; PTR >= HEAP
   455  c66f 4cf3c6             	JMP EXIT2
   456                          +
   457  c672 b15f               	LDA (PTR),Y	; Get back-link high
   458  c674 c901               	CMP #1		; 1-byte gap
   459  c676 f0e7               	BEQ LOOP2	; Skip it, covered later in stage 3.
   460                          
   461  c678 e8                 	INX		; Decrement PTR, but leaving A untouched
   462  c679 ca                 	DEX		; PTR low = 0?
   463  c67a d002               	BNE +
   464  c67c c660               	DEC PTR+1
   465  c67e ca                 +	DEX		; PTR low
   466  c67f 865f               	STX PTR
   467                          
   468  c681 c9ff               	CMP #$FF	; Gap marker? (length >1)
   469  c683 d00f               	BNE NOGAP
   470                          			; Skip gap of a certain length ...
   471  c685 b15f               	LDA (PTR),Y	; Gap length
   472  c687 49ff               	EOR #$FF	; A is > 1
   473                          			; Carry set from CMP above!
   474  c689 6901               	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
   475                          			; Never 0 because gap length > 1
   476  c68b 655f               	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
   477  c68d aa                 	TAX		; PTR low byte
   478  c68e b0d5               	BCS -		; Position on last string byte
   479  c690 c660               	DEC PTR+1	; PTR high byte, always >0
   480  c692 d0d1               	BNE -		; Always, PTR has string address,
   481                          			; pointing to last string byte
   482                          
   483                          ; We have a backlink to the string:
   484  c694 8559               NOGAP	STA DESC+1	; Backlink high and
   485  c696 b15f               	LDA (PTR),Y	; backlink low is the
   486  c698 8558               	STA DESC	; descriptor address.
   487                          
   488  c69a b158               	LDA (DESC),Y	; Length from descriptor
   489  c69c 49ff               	EOR #$FF
   490  c69e 48                 	PHA		; Needed for heap later
   491  c69f a660               	LDX PTR+1	; Transfer to STR ...
   492                          			; Carry clear from CMP #$FF
   493  c6a1 6903               	ADC #3		; -(LEN-2) + PTR -> PTR
   494  c6a3 d002               	BNE +		; PTR already in position
   495                          			; Special case length = 2:
   496  c6a5 e8                 	INX		; compensate for the high byte decrement
   497  c6a6 18                 	CLC		; Adding 0 with carry cleared, leaves PTR unchanged.
   498  c6a7 655f               +	ADC PTR		; Accumulator before add. was in range 0 to FC
   499                          			; which never sets the carry!
   500  c6a9 b001               	BCS +
   501  c6ab ca                 	DEX		; In case of adding 0 X is already compensated.
   502  c6ac 865b               +	STX STR+1	; STR points to string start.
   503  c6ae 855a               	STA STR
   504                          	
   505                          	; make space on heap vor LEN bytes
   506  c6b0 68                 	PLA		; LEN, but only complemented
   507  c6b1 38                 	SEC		; Finalize two's complement (+1 from carry)
   508  c6b2 6522               	ADC NEWHEAP	; HEAP - LEN -> HEAP
   509  c6b4 8522               	STA NEWHEAP
   510  c6b6 b002               	BCS +
   511  c6b8 c623               	DEC NEWHEAP+1
   512                          +	
   513                          	; copy LEN-2 bytes from STR to HEAP, the
   514                          	; remaining bytes are restored from the descriptor later!
   515  c6ba b158               	LDA (DESC),Y	; length from descriptor
   516  c6bc a8                 	TAY		; as index
   517  c6bd 88                 	DEY		; index = length - 2
   518  c6be 88                 	DEY
   519  c6bf f00e               	BEQ +		; 0, nothing to copy
   520  c6c1 88                 	DEY		; -1, index of last byte
   521  c6c2 f007               	BEQ ++		; No loop if index is 0.
   522  c6c4 b15a               -	LDA (STR),Y	; Transfer byte 1 to len-1
   523  c6c6 9122               	STA (NEWHEAP),Y
   524  c6c8 88                 	DEY
   525  c6c9 d0f9               	BNE -
   526  c6cb b15a               ++	LDA (STR),Y	; transfer byte 0
   527  c6cd 9122               	STA (NEWHEAP),Y
   528                          +	
   529                          	; correct descriptor
   530  c6cf a002               	LDY #2		; Offset in descriptor
   531  c6d1 b158               	LDA (DESC),Y	; pre-last string byte 
   532  c6d3 48                 	PHA		; Save
   533  c6d4 a523               	LDA NEWHEAP+1
   534  c6d6 9158               	STA (DESC),Y	; Restore string address low
   535  c6d8 88                 	DEY
   536  c6d9 b158               	LDA (DESC),Y	; last string byte
   537  c6db 48                 	PHA		; Save
   538  c6dc a522               	LDA NEWHEAP	; Restore string address high
   539  c6de 9158               	STA (DESC),Y	; Backlink high
   540                          
   541  c6e0 88                 	DEY		; Y=0
   542                          	; Restore string bytes to backlink
   543  c6e1 b158               	LDA (DESC),Y	; Length byte
   544  c6e3 a8                 	TAY
   545  c6e4 88                 	DEY		; Index of last string byte
   546  c6e5 68                 	PLA
   547  c6e6 9122               	STA (NEWHEAP),Y	; last byte
   548  c6e8 88                 	DEY
   549  c6e9 68                 	PLA		
   550  c6ea 9122               	STA (NEWHEAP),Y	; pre-last byte
   551                          
   552  c6ec a65a               	LDX STR		; PTR low byte in X
   553  c6ee a45b               	LDY STR+1	; always >0
   554  c6f0 4c5bc6             	JMP LOOP2R	; Loop with set PTR and reset Y
   555                          	
   556                          EXIT2
   557  c6f3 a522               	LDA NEWHEAP	; Set rebuilt, compacted heap
   558  c6f5 8533               	STA HEAP	; as new heap.
   559  c6f7 a523               	LDA NEWHEAP+1
   560  c6f9 8534               	STA HEAP+1
   561                          
   562                          
   563                          
   564                          
   565                          ; Put strings (from the heap) with length 1 (stored in the descriptor) 
   566                          ; back on heap. These strings has been marked in a special way.
   567                          
   568                          STAGE3
   569  c6fb 38                         SEC             ; Initialize search for GETSA
   570  c6fc 24                         !byte $24       ; BIT ZP, skip next instruction
   571                          NEXT1STR
   572  c6fd 18                 	CLC		; Continue GETSA from last position
   573  c6fe 203fc7             	JSR GETSA
   574  c701 f022               	BEQ EXIT        ; No String found anymore
   575                          			; Address in X/Y, descr. at STRDP + STAT-offset
   576  c703 c65d               	DEC LEN
   577  c705 d0f6               	BNE NEXT1STR	; Loop if not length 1
   578  c707 98                 	TYA		; Check string address high byte
   579  c708 d0f3               	BNE NEXT1STR	; If not zero, string is not on heap!
   580                          			; Y is always 0.	
   581  c70a 8a                 	TXA		; String addr low is the string byte!
   582  c70b a633               	LDX HEAP
   583  c70d d002               	BNE +		; Heap pointer - 1
   584  c70f c634               	DEC HEAP+1
   585  c711 ca                 +	DEX		; Low byte used later
   586  c712 8633               	STX HEAP
   587  c714 9133               	STA (HEAP),Y	; Stored string byte back to heap
   588                          
   589  c716 a557               	LDA STAT
   590  c718 4a                 	LSR		; Shift right gives offset, which
   591  c719 a8                 	TAY		; refers to STRDP leading to the descriptor
   592  c71a c8                 	INY		; Low byte address in descriptor
   593  c71b 8a                 	TXA		; Heap pointer low
   594  c71c 9122               	STA (STRDP),Y	; stored back into descriptor
   595  c71e c8                 	INY
   596  c71f a534               	LDA HEAP+1	; Heap pointer high
   597  c721 9122               	STA (STRDP),Y	; stored back into descriptor
   598  c723 d0d8               	BNE NEXT1STR	; Branch always, because high byte >0
   599                          	
   600                          
   601                          ; *** Garbage collection finished
   602                          
   603                          EXIT
   604                          
   605                          !ifndef no_indicator {
   606  c725 a200               	LDX #0
   607  c727 8622                       STX CPTR        ; Pointer low byte = 0
   608  c729 ae8802                     LDX VIDPAGE     ; Startpage of video RAM
   609                                  !if (>MARKOFF) >= 1 {
   610  c72c e8                         INX
   611                                  !if (>MARKOFF) >= 2 {
   612  c72d e8                         INX
   613                                  !if (>MARKOFF) >= 3 {
   614  c72e e8                         INX
   615                                  } } }
   616                                  ; X contains now the page plus the offset's high byte
   617  c72f 8623                       STX CPTR+1
   618  c731 a0e7                       LDY #<(MARKOFF)
   619  c733 ad11c8                     LDA ORIGVID     ; Clear activation indicator:
   620  c736 9122                       STA (CPTR),Y    ; restore character on screen
   621  c738 ad12c8                     LDA ORIGCOL     ; and its color.
   622  c73b 8de7db                     STA MARKCPOS
   623                          }
   624  c73e 60                 	RTS
   625                          
   626                          
   627                          ;
   628                          ; *** Get String - fetch next string with length > 0
   629                          ;
   630                          ; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
   631                          ; 
   632                          ; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
   633                          ;
   634                          ; If C=1 start from the beginning at SDS, otherwise
   635                          ; continue with position STRDP and string status STAT.
   636                          ; If the Z-Flag is set no string is available,
   637                          ; otherwise X/Y contains the address and LEN
   638                          ; the length of the string.
   639                          
   640  c73f 905b               GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
   641                                                  ; otherwise start with at SDS ...
   642                          
   643                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   644                          ;
   645                          ;    +-------------+
   646                          ;    |             V
   647                          ;    |    belegt->|<-frei
   648                          ;   +-+     +-----+-----+-----+
   649                          ;   | |     |S|L|H|S|L|H|S|L|H|
   650                          ;   +-+     +-----+-----+-----+
   651                          ;    ^       ^     ^     ^     ^
   652                          ;    $16     $19   $1C   $1F   $22
   653                          ;    TSSP    TOSS
   654                          
   655                          DESCSTACK
   656  c741 a000               	LDY #0
   657  c743 8423               	STY STRDP+1	; Zero descriptor pointer high
   658  c745 a900               	LDA #STAT_SDS	; Set status to SDS
   659  c747 8557               	STA STAT
   660  c749 a219               	LDX #TOSS	; Start of SDS
   661  c74b d005               	BNE ISDSTEND	; branch always
   662  c74d a622               DSTACK	LDX STRDP
   663  c74f e8                 NEXTDST	INX		; next descriptor
   664  c750 e8                 	INX
   665  c751 e8                 	INX
   666                          ISDSTEND
   667  c752 e416               	CPX TSSP	; SDS finished?
   668  c754 f011               	BEQ VARS
   669  c756 b500               	LDA 0,X		; Check string length
   670  c758 f0f5               	BEQ NEXTDST
   671  c75a 855d               	STA LEN		; Return variables:
   672  c75c 8622               	STX STRDP	; length, descriptor address
   673  c75e b502               	LDA 2,X		; String address high
   674  c760 a8                 	TAY
   675  c761 b501               	LDA 1,X		; String address low
   676  c763 aa                 	TAX
   677  c764 a55d               	LDA LEN		; Always not zero, Z=0
   678  c766 60                 	RTS		; Returns address in X/Y
   679                          
   680                          ; *** Look up simple variables: VARTAB to ARYTAB
   681                          
   682  c767 a52d               VARS	LDA VARTAB	; Begin of variables
   683  c769 a62e               	LDX VARTAB+1
   684  c76b 8522               	STA STRDP
   685  c76d 8623               	STX STRDP+1
   686  c76f a004               	LDY #STAT_VAR	; Set status to variables
   687  c771 8457               	STY STAT
   688  c773 d00b               	BNE ISVAREND	; Branch always
   689                          VAR
   690  c775 18                 NEXTVAR	CLC		; Next variable
   691  c776 a522               	LDA STRDP
   692  c778 6907               	ADC #7		; Advance to next variable
   693  c77a 8522               	STA STRDP
   694  c77c 9002               	BCC ISVAREND
   695  c77e e623               	INC STRDP+1	; Overflow high byte
   696                          ISVAREND
   697  c780 c52f               	CMP ARYTAB
   698  c782 d006               	BNE CHECKVAR
   699  c784 a623               	LDX STRDP+1	; Variable end (=array start)?
   700  c786 e430               	CPX ARYTAB+1
   701  c788 f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   702                          CHECKVAR
   703  c78a a000               	LDY #0		; Variable name
   704  c78c b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   705  c78e 30e5               	BMI NEXTVAR	; No string, to next variable
   706  c790 c8                 	INY
   707  c791 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   708  c793 10e0               	BPL NEXTVAR	; No string, to next variable
   709  c795 c8                 	INY
   710  c796 b122               	LDA (STRDP),Y	; String length
   711  c798 f0db               	BEQ NEXTVAR	; = 0, to next variable
   712  c79a d063               	BNE RETGETSA
   713                          
   714                          CHECKTYPE
   715  c79c a557               	LDA STAT	; GETSA intro with C=0
   716  c79e c901               	CMP #STAT_ARY	; String status?
   717  c7a0 f042               	BEQ ARRAY	; =1 -> arrays
   718  c7a2 b0d1               	BCS VAR		; =4 -> variables
   719  c7a4 4c4dc7             	JMP DSTACK	; =0 -> SDS
   720                          
   721                          ; *** Look up arrays: ARYTAB to STREND
   722                          
   723  c7a7 855f               ARRAYS	STA PTR		; A/X set from simple variable processing,
   724  c7a9 8660               	STX PTR+1	; pointing the start of arrays.
   725  c7ab a001               	LDY #STAT_ARY
   726  c7ad 8457               	STY STAT	; Set status to arrays
   727                          ISARREND
   728  c7af a55f               	LDA PTR
   729  c7b1 a660               	LDX PTR+1
   730  c7b3 e432               CHKAEND	CPX STREND+1	; End of array area?
   731  c7b5 d004                       BNE NEXTARR
   732  c7b7 c531               	CMP STREND	; High byte matches, low byte is
   733                          			; less or equal.
   734  c7b9 f051               	BEQ NOSTRING	; Arrays finished -> no string
   735                          NEXTARR
   736                          			; Carry always cleared because of CPX/CMP
   737  c7bb 8522               	STA STRDP	; Start of an array
   738  c7bd 8623               	STX STRDP+1
   739  c7bf a000               	LDY #0
   740  c7c1 b122               	LDA (STRDP),Y	; Array name
   741  c7c3 aa                 	TAX		; Array type, keep it for later
   742  c7c4 c8                 	INY
   743  c7c5 b122               	LDA (STRDP),Y
   744  c7c7 08                 	PHP		; Array type 2nd part, keep also
   745  c7c8 c8                 	INY
   746  c7c9 b122               	LDA (STRDP),Y	; Offset to next array
   747  c7cb 655f               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   748  c7cd 855f               	STA PTR		; Save start of following array
   749  c7cf c8                 	INY
   750  c7d0 b122               	LDA (STRDP),Y
   751  c7d2 6560               	ADC PTR+1
   752  c7d4 8560               	STA PTR+1
   753  c7d6 28                 	PLP		; Fetch array type
   754  c7d7 10d6               	BPL ISARREND	; Not a string array
   755  c7d9 8a                 	TXA		; Fetch array type 2nd part
   756  c7da 30d3               	BMI ISARREND	; Not string array
   757  c7dc c8                 	INY
   758  c7dd b122               	LDA (STRDP),Y	; Number of dimensions
   759  c7df 0a                 	ASL		; *2
   760  c7e0 6905               	ADC #5		; Offset = dimensions*2+5
   761                          			; C=0 as long as dim.. <= 125
   762  c7e2 d003               	BNE ADVDESC	; Branch always
   763                          ARRAY			; Entry on continuation
   764                          NEXTASTR
   765  c7e4 18                 	CLC
   766  c7e5 a903               	LDA #3		; String descriptor length
   767  c7e7 6522               ADVDESC	ADC STRDP	; Advance to next string
   768  c7e9 8522               	STA STRDP
   769  c7eb 9002               	BCC +
   770  c7ed e623               	INC STRDP+1	; Overflow high byte
   771  c7ef c55f               +	CMP PTR		; All array elements processed?
   772  c7f1 d006               	BNE IS0ASTR
   773  c7f3 a623               	LDX STRDP+1
   774  c7f5 e460               	CPX PTR+1
   775  c7f7 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   776                          IS0ASTR
   777  c7f9 a000               	LDY #0
   778  c7fb b122               	LDA (STRDP),Y	; String length
   779  c7fd f0e5               	BEQ NEXTASTR	; Next array element
   780                          RETGETSA
   781  c7ff 855d               	STA LEN		; Return value: length
   782  c801 c8                 	INY
   783  c802 b122               	LDA (STRDP),Y	; String address low
   784  c804 aa                 	TAX
   785  c805 c8                 	INY
   786  c806 b122               	LDA (STRDP),Y	; String address high
   787  c808 a8                 	TAY
   788  c809 a55d               	LDA LEN		; Always not zero, Z=0
   789  c80b 60                 	RTS		; Return address in X/Y
   790                          NOSTRING
   791  c80c a900               	LDA #0		; Length 0 
   792  c80e 855d               	STA LEN		; No string found
   793  c810 60                 	RTS		; Z=1
   794                          
   795                          
   796                          
   797                          
   798                          !ifndef no_indicator {
   799  c811 00                 ORIGVID !byte 0		; Original character of marker position
   800  c812 00                 ORIGCOL !byte 0		; Original color of marker position
   801                          }
   802                          
   803                          
