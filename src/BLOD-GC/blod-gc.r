
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
   137  c52d a997               	LDA #<HANDLE2	; let JSR in place!
   138  c52f 8d6bb6             	STA PATCH2+1
   139  c532 a9c5               	LDA #>HANDLE2
   140  c534 8d6cb6             	STA PATCH2+2
   141                          
   142  c537 a9b0               	LDA #<HANDLE3	; let JSR in place!
   143  c539 8d27b7             	STA PATCH3+1
   144  c53c a9c5               	LDA #>HANDLE3
   145  c53e 8d28b7             	STA PATCH3+2
   146                          
   147  c541 a9d6               	LDA #<COLLECT	; Write "JMP COLLECT"
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
   168  c551 20dbb6             	JSR $B6DB	; Remove descriptor if on top
   169                          ;	CPY $18		; Descriptor on top of SDS?
   170                          ;	BNE +
   171                          ;	CMP $17
   172                          ;	BNE +
   173                          ;	STA $16		; Yes, remove it from SDS
   174                          ;	SBC #3
   175                          ;	STA $17
   176                          	
   177                          	; If destination variable points to string on the heap, free it.
   178                          
   179  c554 a000               +	LDY #0
   180                          	; $49 points to variable descriptor (in LET's destination variable)
   181  c556 b149               	LDA ($49),Y	; Get string length
   182  c558 f03c               	BEQ LEAVE	; Variable contains no string
   183  c55a aa                 	TAX		; > 0, save it for later
   184  c55b c8                 	INY
   185  c55c b149               	LDA ($49),Y	; String address low
   186  c55e 855a               	STA STR
   187  c560 c8                 	INY
   188  c561 b149               	LDA ($49),Y	; String address high
   189  c563 855b               	STA STR+1
   190                          
   191                          	; Free STR if on heap and return
   192                          
   193                          FREE
   194  c565 a55b               	LDA STR+1	; String address high
   195  c567 c534               	CMP FRETOP+1	; Heap top high
   196  c569 902b               	BCC LEAVE	; String below heap (on on heap)
   197  c56b d008               	BNE ++		; String on heap
   198  c56d a55a               	LDA STR		; String address low
   199  c56f c533               	CMP FRETOP	; Heap top low
   200  c571 9023               	BCC LEAVE	; Leave when not on heap!
   201                          
   202  c573 a55b               	LDA STR+1	; String address greater or equal FRETOP
   203                          
   204  c575 c538               ++	CMP MEMEND+1	; String above string memory?
   205  c577 9008               	BCC +++		; no
   206  c579 d01b               	BNE LEAVE	; yes
   207  c57b a55a               	LDA STR		; High byte equal, compare low byte
   208  c57d c537               	CMP MEMEND
   209  c57f b015               	BCS LEAVE	; Above heap
   210                          	
   211                          	; String on heap: mark it as free
   212                          
   213  c581 8a                 +++	TXA		; Restore length
   214  c582 c901               	CMP #1		; String of length 1?
   215  c584 d006               	BNE ++
   216                          
   217  c586 a000               	LDY #0
   218  c588 915a               	STA (STR),Y	; Single byte on heap contains 1
   219  c58a f00a               	BEQ LEAVE	; leave, always (Z=1)
   220                          
   221  c58c a8                 ++	TAY		; Length to Y (> 1!)
   222  c58d 88                 	DEY
   223  c58e 88                 	DEY		; Y: Length - 2
   224  c58f 915a               	STA (STR),Y	; Pre-last byte of string has length
   225  c591 c8                 	INY
   226  c592 a9ff               	LDA #$FF
   227  c594 915a               	STA (STR),Y	; Last byte of string with gap-marker
   228  c596 60                 LEAVE	RTS
   229                          
   230                          
   231                          
   232                          ; String concatenation: free 2nd argument after copying!
   233                          
   234                          ;.,B65D 20 75 B4 JSR $B475       copy descriptor pointer and make string space
   235                          ;                                A bytes long
   236                          ;.,B660 20 7A B6 JSR $B67A       copy string from descriptor to utility pointer
   237                          ;.,B663 A5 50    LDA $50         get descriptor pointer low byte
   238                          ;.,B665 A4 51    LDY $51         get descriptor pointer high byte
   239                          ;.,B667 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of
   240                          ;                                string space returns with A = length, 
   241                          ;                                X = pointer low byte, Y = pointer high byte
   242                          ;.,B66A 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   243                          ;.,B66D A5 6F    LDA $6F         get descriptor pointer low byte
   244                          ;.,B66F A4 70    LDY $70         get descriptor pointer high byte
   245                          ;.,B671 20 AA B6 JSR $B6AA       pop (YA) descriptor off stack or from top of
   246                          ;                                string space returns with A = length, 
   247                          ;                                X = pointer low byte, Y = pointer high byte
   248                          ;.,B674 20 CA B4 JSR $B4CA       check space on descriptor stack then put
   249                          ;                                string address and length on descriptor stack
   250                          ;                                and update stack pointers
   251                          ;.,B677 4C B8 AD JMP $ADB8       continue evaluation
   252                          
   253                          ; -> 
   254                          
   255                          ;.,B66A 20 8C B6 JSR HANDLE2     store string from pointer to utility pointer
   256                          
   257                          ; Only both or none of the arguments are the SDS. If the 2nd (later pushed)
   258                          ; element has been popped, the first argument will be on the SDS also.
   259                          
   260                          HANDLE2
   261  c597 208cb6             	JSR $B68C	; Copy 2nd string to utility pointer's location
   262  c59a a550               	LDA $50		; Descriptor address of 2nd argument
   263  c59c a451               	LDY $51		; It is never top on heap, so just mark it as free
   264  c59e c516               	CMP $16		; Previously popped element
   265  c5a0 d0f4               	BNE LEAVE
   266  c5a2 c418               	CPY $18		; High byte (normally 0)
   267  c5a4 d0f0               	BNE LEAVE
   268  c5a6 20c7c5             	JSR FREESDS	; Mark already remove element from SDS as free
   269  c5a9 a56f               	LDA $6F		; Descriptor address of the first argument
   270  c5ab a470               	LDY $70
   271  c5ad 4cb7c5             	JMP POPSDS	; Remove element from SDS and mark as free
   272                          	
   273                          
   274                          ; LEFT$(), RIGHT$(), MID$(): Free input string
   275                          
   276                          ;.,B726 20 8C B6 JSR $B68C       store string from pointer to utility pointer
   277                          ;.,B729 4C CA B4 JMP $B4CA       check space on descriptor stack then put
   278                          ;                                string address and length on descriptor stack
   279                          ;                                and update stack pointers
   280                          ; -> 
   281                          ;.,B726 20 8C B6 JSR HANDLE3     store string from pointer to utility pointer
   282                          
   283                          HANDLE3
   284                          	; A: length, copy from ($22) to ($35)
   285  c5b0 208cb6             	JSR $B68C	; Copy string part into allocated space
   286  c5b3 a550               	LDA $50
   287  c5b5 a451               	LDY $51
   288                          
   289                          	; the string itself is not top of heap, just mark as free
   290                          	; and remove from SDS
   291                          
   292                          POPSDS
   293  c5b7 c418               	CPY $18		; Descriptor on top of SDS?
   294  c5b9 d0db               	BNE LEAVE	; RTS
   295  c5bb c517               	CMP $17
   296  c5bd d0d7               	BNE LEAVE	; RTS
   297                          	; free memory and pull from SDS
   298  c5bf 20c7c5             	JSR FREESDS	; Carry = 1 CMP above, left untouched after JSR
   299  c5c2 a517               	LDA $17		; Top element on SDS
   300  c5c4 4ce3b6             	JMP $B6E3	; Remove from SDS (A has low byte to SDS element)
   301                          			; Carry flag must be set on entry.
   302                          FREESDS
   303                          	; A/Y is pointer to string descriptor on the SDS!
   304  c5c7 aa                 	TAX		; Index in zero-page
   305  c5c8 b501               	LDA 1,X		; String address low
   306  c5ca 855a               	STA STR
   307  c5cc b502               	LDA 2,X		; String address high
   308  c5ce 855b               	STA STR+1
   309  c5d0 b500               	LDA 0,X		; String length
   310  c5d2 aa                 	TAX
   311  c5d3 d090               	BNE FREE	; Length X, address STR/STR+1
   312  c5d5 60                 	RTS		; No free if length = 0!
   313                          
   314                          
   315                          
   316                          ; *** Garbage Collector
   317                          
   318                          COLLECT
   319                          
   320                          !ifndef no_indicator {
   321  c5d6 a200               	LDX #0
   322  c5d8 8622               	STX CPTR	; Pointer low byte = 0
   323  c5da ae8802             	LDX VIDPAGE	; Startpage of video RAM
   324                          	!if (>MARKOFF) >= 1 {
   325  c5dd e8                 	INX
   326                          	!if (>MARKOFF) >= 2 {
   327  c5de e8                 	INX
   328                          	!if (>MARKOFF) >= 3 {
   329  c5df e8                 	INX
   330                          	} } }
   331                          	; X contains now the page plus the offset's high byte
   332  c5e0 8623               	STX CPTR+1
   333  c5e2 a0e7               	LDY #<(MARKOFF)
   334  c5e4 b122               	LDA (CPTR),Y	; Activity indicator on screen:
   335  c5e6 8d13c8             	STA ORIGVID	; Save current character
   336  c5e9 a92a               	LDA #MARKCHAR
   337  c5eb 9122               	STA (CPTR),Y	; Set mark character
   338  c5ed ade7db             	LDA MARKCPOS	; Same for the color information
   339  c5f0 8d14c8             	STA ORIGCOL	; Save current color
   340  c5f3 a909               	LDA #MARKCOL
   341  c5f5 8de7db             	STA MARKCPOS	; Set mark color
   342                          }
   343                          
   344                          
   345                          
   346                          ; walk through all strings and reorganize them
   347                          
   348                          STAGE1
   349  c5f8 38                         SEC             ; Initialize search
   350                          NEXTSTR
   351  c5f9 2041c7             	JSR GETSA
   352  c5fc f057               	BEQ STAGE2      ; No String found anymore
   353                          			; Address in X/Y, descr. at STRDP + STAT-offset
   354                          
   355  c5fe c434               	CPY FRETOP+1	; String on heap?
   356  c600 90f7               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   357  c602 d004               	BNE +
   358  c604 e433               	CPX FRETOP
   359  c606 90f1               	BCC NEXTSTR	; No, C=0 for GETSA continuation
   360                          
   361  c608 865a               +	STX STR		; Start of string which is on heap
   362  c60a 845b               	STY STR+1
   363  c60c a55d               	LDA LEN
   364  c60e c901               	CMP #1		; String length 1?
   365  c610 d019               	BNE ++
   366                          
   367                          	; LEN 1: 
   368                          	;	copy string value into descriptor
   369                          	;	overwrite string on heap with value 1
   370                          
   371  c612 a000               	LDY #0
   372  c614 b15a               	LDA (STR),Y	; String value
   373  c616 aa                 	TAX
   374  c617 a901               	LDA #1		; Marker for string with length 1
   375  c619 915a               	STA (STR),Y	; Store marker on heap
   376  c61b a557               	LDA STAT
   377  c61d 4a                 	LSR		; Shift right gives offset, which
   378  c61e a8                 	TAY		; refers to STRDP leading to the descriptor
   379  c61f c8                 	INY		; Position string address low byte
   380  c620 8a                 	TXA		; String value
   381  c621 9122               	STA (STRDP),Y	; Store value in descriptor (low address byte)
   382  c623 a900               	LDA #0		; Mark for high byte, means on heap string
   383  c625 c8                 	INY		; String address high byte
   384  c626 9122               	STA (STRDP),Y	; Set to zero
   385  c628 18                 	CLC		; Continuation mode for GETSA
   386  c629 90ce               	BCC NEXTSTR	; Always
   387                          
   388                          	; LEN >1:
   389                          	;	copy backlink bytes to descriptor
   390                          	;	store descriptor pointer to backlink
   391                          
   392  c62b a8                 ++	TAY		; Length
   393  c62c 88                 	DEY		; Index to last byte
   394  c62d b15a               	LDA (STR),Y
   395  c62f 48                 	PHA		; Last byte of string
   396  c630 a623               	LDX STRDP+1
   397  c632 a557               	LDA STAT
   398  c634 4a                 	LSR		; Shift right gives offset to the descriptor
   399  c635 18                 	CLC
   400  c636 6522               	ADC STRDP
   401  c638 9001               	BCC +
   402  c63a e8                 	INX
   403  c63b 48                 +	PHA		; STRDP + offset low
   404  c63c 8a                 	TXA		; X STRDP + offset high
   405  c63d 915a               	STA (STR),Y	; Back-link high
   406  c63f 88                 	DEY
   407  c640 b15a               	LDA (STR),Y	; Pre-last byte string
   408  c642 aa                 	TAX
   409  c643 68                 	PLA		; STRDP + offset low
   410  c644 915a               	STA (STR),Y	; Back-link low
   411  c646 a557               	LDA STAT
   412  c648 4a                 	LSR		; Shift right gives offset, which
   413  c649 a8                 	TAY		; refers to STRDP leading to the descriptor
   414  c64a c8                 	INY		; Skip length byte
   415  c64b 68                 	PLA		; Last byte of string
   416  c64c 9122               	STA (STRDP),Y	; Store into descriptor address low byte
   417  c64e 8a                 	TXA		; Pre-last byte of string
   418  c64f c8                 	INY		; =2
   419  c650 9122               	STA (STRDP),Y	; Store into descriptor address high byte
   420  c652 18                 	CLC		; Continuation mode for GETSA
   421  c653 90a4               	BCC NEXTSTR	; Always
   422                          	
   423                          
   424                          
   425                          
   426                          ; walk through heap, remove gaps and move strings
   427                          
   428                          STAGE2
   429  c655 a438               	LDY MEMEND+1	; Top of memory.
   430  c657 a637               	LDX MEMEND	; Set new heap top
   431  c659 8622               	STX NEWHEAP	; to memory end.
   432  c65b 8423               	STY NEWHEAP+1
   433                          			; Entry point from no-gap part
   434  c65d 8460               LOOP2R	STY PTR+1	; PTR comes from X
   435  c65f a000               	LDY #0
   436                          LOOP2
   437  c661 8a                 	TXA		; PTR minus 1
   438  c662 d002               	BNE +
   439  c664 c660               	DEC PTR+1
   440  c666 ca                 +	DEX
   441  c667 865f               -	STX PTR
   442                          
   443  c669 e433               	CPX HEAP	; PTR below top of heap?
   444  c66b a560               	LDA PTR+1
   445  c66d e534               	SBC HEAP+1
   446  c66f b003               	BCS +		; PTR >= HEAP
   447  c671 4cf5c6             	JMP EXIT2
   448                          +
   449  c674 b15f               	LDA (PTR),Y	; Get back-link high
   450  c676 c901               	CMP #1		; 1-byte gap
   451  c678 f0e7               	BEQ LOOP2	; Skip it, covered later in stage 3.
   452                          
   453  c67a e8                 	INX		; Decrement PTR, but leaving A untouched
   454  c67b ca                 	DEX		; PTR low = 0?
   455  c67c d002               	BNE +
   456  c67e c660               	DEC PTR+1
   457  c680 ca                 +	DEX		; PTR low
   458  c681 865f               	STX PTR
   459                          
   460  c683 c9ff               	CMP #$FF	; Gap marker? (length >1)
   461  c685 d00f               	BNE NOGAP
   462                          			; Skip gap of a certain length ...
   463  c687 b15f               	LDA (PTR),Y	; Gap length
   464  c689 49ff               	EOR #$FF	; A is > 1
   465                          			; Carry set from CMP above!
   466  c68b 6901               	ADC #1		; Two's complement +1 and +1, -(LEN-1) + PTR -> PTR
   467                          			; Never 0 because gap length > 1
   468  c68d 655f               	ADC PTR		; C=0 always because -(LEN-1) could never exceed $FF
   469  c68f aa                 	TAX		; PTR low byte
   470  c690 b0d5               	BCS -		; Position on last string byte
   471  c692 c660               	DEC PTR+1	; PTR high byte, always >0
   472  c694 d0d1               	BNE -		; Always, PTR has string address,
   473                          			; pointing to last string byte
   474                          
   475                          ; We have a backlink to the string:
   476  c696 8559               NOGAP	STA DESC+1	; Backlink high and
   477  c698 b15f               	LDA (PTR),Y	; backlink low is the
   478  c69a 8558               	STA DESC	; descriptor address.
   479                          
   480  c69c b158               	LDA (DESC),Y	; Length from descriptor
   481  c69e 49ff               	EOR #$FF
   482  c6a0 48                 	PHA		; Needed for heap later
   483  c6a1 a660               	LDX PTR+1	; Transfer to STR ...
   484                          			; Carry clear from CMP #$FF
   485  c6a3 6903               	ADC #3		; -(LEN-2) + PTR -> PTR
   486  c6a5 d002               	BNE +		; PTR already in position
   487                          			; Special case length = 2:
   488  c6a7 e8                 	INX		; compensate for the high byte decrement
   489  c6a8 18                 	CLC		; Adding 0 with carry cleared, leaves PTR unchanged.
   490  c6a9 655f               +	ADC PTR		; Accumulator before add. was in range 0 to FC
   491                          			; which never sets the carry!
   492  c6ab b001               	BCS +
   493  c6ad ca                 	DEX		; In case of adding 0 X is already compensated.
   494  c6ae 865b               +	STX STR+1	; STR points to string start.
   495  c6b0 855a               	STA STR
   496                          	
   497                          	; make space on heap vor LEN bytes
   498  c6b2 68                 	PLA		; LEN, but only complemented
   499  c6b3 38                 	SEC		; Finalize two's complement (+1 from carry)
   500  c6b4 6522               	ADC NEWHEAP	; HEAP - LEN -> HEAP
   501  c6b6 8522               	STA NEWHEAP
   502  c6b8 b002               	BCS +
   503  c6ba c623               	DEC NEWHEAP+1
   504                          +	
   505                          	; copy LEN-2 bytes from STR to HEAP, the
   506                          	; remaining bytes are restored from the descriptor later!
   507  c6bc b158               	LDA (DESC),Y	; length from descriptor
   508  c6be a8                 	TAY		; as index
   509  c6bf 88                 	DEY		; index = length - 2
   510  c6c0 88                 	DEY
   511  c6c1 f00e               	BEQ +		; 0, nothing to copy
   512  c6c3 88                 	DEY		; -1, index of last byte
   513  c6c4 f007               	BEQ ++		; No loop if index is 0.
   514  c6c6 b15a               -	LDA (STR),Y	; Transfer byte 1 to len-1
   515  c6c8 9122               	STA (NEWHEAP),Y
   516  c6ca 88                 	DEY
   517  c6cb d0f9               	BNE -
   518  c6cd b15a               ++	LDA (STR),Y	; transfer byte 0
   519  c6cf 9122               	STA (NEWHEAP),Y
   520                          +	
   521                          	; correct descriptor
   522  c6d1 a002               	LDY #2		; Offset in descriptor
   523  c6d3 b158               	LDA (DESC),Y	; pre-last string byte 
   524  c6d5 48                 	PHA		; Save
   525  c6d6 a523               	LDA NEWHEAP+1
   526  c6d8 9158               	STA (DESC),Y	; Restore string address low
   527  c6da 88                 	DEY
   528  c6db b158               	LDA (DESC),Y	; last string byte
   529  c6dd 48                 	PHA		; Save
   530  c6de a522               	LDA NEWHEAP	; Restore string address high
   531  c6e0 9158               	STA (DESC),Y	; Backlink high
   532                          
   533  c6e2 88                 	DEY		; Y=0
   534                          	; Restore string bytes to backlink
   535  c6e3 b158               	LDA (DESC),Y	; Length byte
   536  c6e5 a8                 	TAY
   537  c6e6 88                 	DEY		; Index of last string byte
   538  c6e7 68                 	PLA
   539  c6e8 9122               	STA (NEWHEAP),Y	; last byte
   540  c6ea 88                 	DEY
   541  c6eb 68                 	PLA		
   542  c6ec 9122               	STA (NEWHEAP),Y	; pre-last byte
   543                          
   544  c6ee a65a               	LDX STR		; PTR low byte in X
   545  c6f0 a45b               	LDY STR+1	; always >0
   546  c6f2 4c5dc6             	JMP LOOP2R	; Loop with set PTR and reset Y
   547                          	
   548                          EXIT2
   549  c6f5 a522               	LDA NEWHEAP	; Set rebuilt, compacted heap
   550  c6f7 8533               	STA HEAP	; as new heap.
   551  c6f9 a523               	LDA NEWHEAP+1
   552  c6fb 8534               	STA HEAP+1
   553                          
   554                          
   555                          
   556                          
   557                          ; Put strings (from the heap) with length 1 (stored in the descriptor) 
   558                          ; back on heap. These strings has been marked in a special way.
   559                          
   560                          STAGE3
   561  c6fd 38                         SEC             ; Initialize search for GETSA
   562  c6fe 24                         !byte $24       ; BIT ZP, skip next instruction
   563                          NEXT1STR
   564  c6ff 18                 	CLC		; Continue GETSA from last position
   565  c700 2041c7             	JSR GETSA
   566  c703 f022               	BEQ EXIT        ; No String found anymore
   567                          			; Address in X/Y, descr. at STRDP + STAT-offset
   568  c705 c65d               	DEC LEN
   569  c707 d0f6               	BNE NEXT1STR	; Loop if not length 1
   570  c709 98                 	TYA		; Check string address high byte
   571  c70a d0f3               	BNE NEXT1STR	; If not zero, string is not on heap!
   572                          			; Y is always 0.	
   573  c70c 8a                 	TXA		; String addr low is the string byte!
   574  c70d a633               	LDX HEAP
   575  c70f d002               	BNE +		; Heap pointer - 1
   576  c711 c634               	DEC HEAP+1
   577  c713 ca                 +	DEX		; Low byte used later
   578  c714 8633               	STX HEAP
   579  c716 9133               	STA (HEAP),Y	; Stored string byte back to heap
   580                          
   581  c718 a557               	LDA STAT
   582  c71a 4a                 	LSR		; Shift right gives offset, which
   583  c71b a8                 	TAY		; refers to STRDP leading to the descriptor
   584  c71c c8                 	INY		; Low byte address in descriptor
   585  c71d 8a                 	TXA		; Heap pointer low
   586  c71e 9122               	STA (STRDP),Y	; stored back into descriptor
   587  c720 c8                 	INY
   588  c721 a534               	LDA HEAP+1	; Heap pointer high
   589  c723 9122               	STA (STRDP),Y	; stored back into descriptor
   590  c725 d0d8               	BNE NEXT1STR	; Branch always, because high byte >0
   591                          	
   592                          
   593                          ; *** Garbage collection finished
   594                          
   595                          EXIT
   596                          
   597                          !ifndef no_indicator {
   598  c727 a200               	LDX #0
   599  c729 8622                       STX CPTR        ; Pointer low byte = 0
   600  c72b ae8802                     LDX VIDPAGE     ; Startpage of video RAM
   601                                  !if (>MARKOFF) >= 1 {
   602  c72e e8                         INX
   603                                  !if (>MARKOFF) >= 2 {
   604  c72f e8                         INX
   605                                  !if (>MARKOFF) >= 3 {
   606  c730 e8                         INX
   607                                  } } }
   608                                  ; X contains now the page plus the offset's high byte
   609  c731 8623                       STX CPTR+1
   610  c733 a0e7                       LDY #<(MARKOFF)
   611  c735 ad13c8                     LDA ORIGVID     ; Clear activation indicator:
   612  c738 9122                       STA (CPTR),Y    ; restore character on screen
   613  c73a ad14c8                     LDA ORIGCOL     ; and its color.
   614  c73d 8de7db                     STA MARKCPOS
   615                          }
   616  c740 60                 	RTS
   617                          
   618                          
   619                          ;
   620                          ; *** Get String - fetch next string with length > 0
   621                          ;
   622                          ; ( C-flag, STAT, STRDP, PTR -> STRDP, LEN, STAT, X, Y, Z-flag )
   623                          ; 
   624                          ; STAT >> 1 -> offset to descriptor relative to pointer STRDP.
   625                          ;
   626                          ; If C=1 start from the beginning at SDS, otherwise
   627                          ; continue with position STRDP and string status STAT.
   628                          ; If the Z-Flag is set no string is available,
   629                          ; otherwise X/Y contains the address and LEN
   630                          ; the length of the string.
   631                          
   632  c741 905b               GETSA   BCC CHECKTYPE   ; C=0 -> continue with string according to STAT
   633                                                  ; otherwise start with at SDS ...
   634                          
   635                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   636                          ;
   637                          ;    +-------------+
   638                          ;    |             V
   639                          ;    |    belegt->|<-frei
   640                          ;   +-+     +-----+-----+-----+
   641                          ;   | |     |S|L|H|S|L|H|S|L|H|
   642                          ;   +-+     +-----+-----+-----+
   643                          ;    ^       ^     ^     ^     ^
   644                          ;    $16     $19   $1C   $1F   $22
   645                          ;    TSSP    TOSS
   646                          
   647                          DESCSTACK
   648  c743 a000               	LDY #0
   649  c745 8423               	STY STRDP+1	; Zero descriptor pointer high
   650  c747 a900               	LDA #STAT_SDS	; Set status to SDS
   651  c749 8557               	STA STAT
   652  c74b a219               	LDX #TOSS	; Start of SDS
   653  c74d d005               	BNE ISDSTEND	; branch always
   654  c74f a622               DSTACK	LDX STRDP
   655  c751 e8                 NEXTDST	INX		; next descriptor
   656  c752 e8                 	INX
   657  c753 e8                 	INX
   658                          ISDSTEND
   659  c754 e416               	CPX TSSP	; SDS finished?
   660  c756 f011               	BEQ VARS
   661  c758 b500               	LDA 0,X		; Check string length
   662  c75a f0f5               	BEQ NEXTDST
   663  c75c 855d               	STA LEN		; Return variables:
   664  c75e 8622               	STX STRDP	; length, descriptor address
   665  c760 b502               	LDA 2,X		; String address high
   666  c762 a8                 	TAY
   667  c763 b501               	LDA 1,X		; String address low
   668  c765 aa                 	TAX
   669  c766 a55d               	LDA LEN		; Always not zero, Z=0
   670  c768 60                 	RTS		; Returns address in X/Y
   671                          
   672                          ; *** Look up simple variables: VARTAB to ARYTAB
   673                          
   674  c769 a52d               VARS	LDA VARTAB	; Begin of variables
   675  c76b a62e               	LDX VARTAB+1
   676  c76d 8522               	STA STRDP
   677  c76f 8623               	STX STRDP+1
   678  c771 a004               	LDY #STAT_VAR	; Set status to variables
   679  c773 8457               	STY STAT
   680  c775 d00b               	BNE ISVAREND	; Branch always
   681                          VAR
   682  c777 18                 NEXTVAR	CLC		; Next variable
   683  c778 a522               	LDA STRDP
   684  c77a 6907               	ADC #7		; Advance to next variable
   685  c77c 8522               	STA STRDP
   686  c77e 9002               	BCC ISVAREND
   687  c780 e623               	INC STRDP+1	; Overflow high byte
   688                          ISVAREND
   689  c782 c52f               	CMP ARYTAB
   690  c784 d006               	BNE CHECKVAR
   691  c786 a623               	LDX STRDP+1	; Variable end (=array start)?
   692  c788 e430               	CPX ARYTAB+1
   693  c78a f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   694                          CHECKVAR
   695  c78c a000               	LDY #0		; Variable name
   696  c78e b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   697  c790 30e5               	BMI NEXTVAR	; No string, to next variable
   698  c792 c8                 	INY
   699  c793 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   700  c795 10e0               	BPL NEXTVAR	; No string, to next variable
   701  c797 c8                 	INY
   702  c798 b122               	LDA (STRDP),Y	; String length
   703  c79a f0db               	BEQ NEXTVAR	; = 0, to next variable
   704  c79c d063               	BNE RETGETSA
   705                          
   706                          CHECKTYPE
   707  c79e a557               	LDA STAT	; GETSA intro with C=0
   708  c7a0 c901               	CMP #STAT_ARY	; String status?
   709  c7a2 f042               	BEQ ARRAY	; =1 -> arrays
   710  c7a4 b0d1               	BCS VAR		; =4 -> variables
   711  c7a6 4c4fc7             	JMP DSTACK	; =0 -> SDS
   712                          
   713                          ; *** Look up arrays: ARYTAB to STREND
   714                          
   715  c7a9 855f               ARRAYS	STA PTR		; A/X set from simple variable processing,
   716  c7ab 8660               	STX PTR+1	; pointing the start of arrays.
   717  c7ad a001               	LDY #STAT_ARY
   718  c7af 8457               	STY STAT	; Set status to arrays
   719                          ISARREND
   720  c7b1 a55f               	LDA PTR
   721  c7b3 a660               	LDX PTR+1
   722  c7b5 e432               CHKAEND	CPX STREND+1	; End of array area?
   723  c7b7 d004                       BNE NEXTARR
   724  c7b9 c531               	CMP STREND	; High byte matches, low byte is
   725                          			; less or equal.
   726  c7bb f051               	BEQ NOSTRING	; Arrays finished -> no string
   727                          NEXTARR
   728                          			; Carry always cleared because of CPX/CMP
   729  c7bd 8522               	STA STRDP	; Start of an array
   730  c7bf 8623               	STX STRDP+1
   731  c7c1 a000               	LDY #0
   732  c7c3 b122               	LDA (STRDP),Y	; Array name
   733  c7c5 aa                 	TAX		; Array type, keep it for later
   734  c7c6 c8                 	INY
   735  c7c7 b122               	LDA (STRDP),Y
   736  c7c9 08                 	PHP		; Array type 2nd part, keep also
   737  c7ca c8                 	INY
   738  c7cb b122               	LDA (STRDP),Y	; Offset to next array
   739  c7cd 655f               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   740  c7cf 855f               	STA PTR		; Save start of following array
   741  c7d1 c8                 	INY
   742  c7d2 b122               	LDA (STRDP),Y
   743  c7d4 6560               	ADC PTR+1
   744  c7d6 8560               	STA PTR+1
   745  c7d8 28                 	PLP		; Fetch array type
   746  c7d9 10d6               	BPL ISARREND	; Not a string array
   747  c7db 8a                 	TXA		; Fetch array type 2nd part
   748  c7dc 30d3               	BMI ISARREND	; Not string array
   749  c7de c8                 	INY
   750  c7df b122               	LDA (STRDP),Y	; Number of dimensions
   751  c7e1 0a                 	ASL		; *2
   752  c7e2 6905               	ADC #5		; Offset = dimensions*2+5
   753                          			; C=0 as long as dim.. <= 125
   754  c7e4 d003               	BNE ADVDESC	; Branch always
   755                          ARRAY			; Entry on continuation
   756                          NEXTASTR
   757  c7e6 18                 	CLC
   758  c7e7 a903               	LDA #3		; String descriptor length
   759  c7e9 6522               ADVDESC	ADC STRDP	; Advance to next string
   760  c7eb 8522               	STA STRDP
   761  c7ed 9002               	BCC +
   762  c7ef e623               	INC STRDP+1	; Overflow high byte
   763  c7f1 c55f               +	CMP PTR		; All array elements processed?
   764  c7f3 d006               	BNE IS0ASTR
   765  c7f5 a623               	LDX STRDP+1
   766  c7f7 e460               	CPX PTR+1
   767  c7f9 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   768                          IS0ASTR
   769  c7fb a000               	LDY #0
   770  c7fd b122               	LDA (STRDP),Y	; String length
   771  c7ff f0e5               	BEQ NEXTASTR	; Next array element
   772                          RETGETSA
   773  c801 855d               	STA LEN		; Return value: length
   774  c803 c8                 	INY
   775  c804 b122               	LDA (STRDP),Y	; String address low
   776  c806 aa                 	TAX
   777  c807 c8                 	INY
   778  c808 b122               	LDA (STRDP),Y	; String address high
   779  c80a a8                 	TAY
   780  c80b a55d               	LDA LEN		; Always not zero, Z=0
   781  c80d 60                 	RTS		; Return address in X/Y
   782                          NOSTRING
   783  c80e a900               	LDA #0		; Length 0 
   784  c810 855d               	STA LEN		; No string found
   785  c812 60                 	RTS		; Z=1
   786                          
   787                          
   788                          
   789                          
   790                          !ifndef no_indicator {
   791  c813 00                 ORIGVID !byte 0		; Original character of marker position
   792  c814 00                 ORIGCOL !byte 0		; Original color of marker position
   793                          }
   794                          
   795                          
