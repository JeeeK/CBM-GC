
; ******** Source: jk-gc.asm
     1                          ;
     2                          ; *************************
     3                          ; *  GARBAGE  COLLECTION  *
     4                          ; *   from Johann Klasek  *
     5                          ; *   j AT klasek DOT at  *
     6                          ; * 1985-12-27 VERS. 1.1  *
     7                          ; * 2013-11-24 VERS. 2.0  *
     8                          ; * 2019-02-15 VERS. 2.1  *
     9                          ; * 2020-12-14 VERS. 2.2  *
    10                          ; *************************
    11                          ;
    12                          ; Collects unused (garbage) strings on the string heap,
    13                          ; replacing the BASIC 2.0 garbage collector on a C64.
    14                          ; Only those locations which is used by the legacy garbage
    15                          ; collector are in use here, some other are restored after
    16                          ; finishing the run.
    17                          
    18                          ; Start of code ...
    19                          
    20                          !ifdef start {
    21                          	*=start
    22                          } else {
    23                          	*= $C500
    24                          }
    25                          
    26                          ; Options:
    27                          
    28                          ; Enable the patch hook (the BASIC ROM copy in RAM from $A000 to $BFFF)
    29                          ; - otherwise the IRQ hook method is used.
    30                          ;basic_patch = 1
    31                          ;
    32                          ;	Space usage notes:
    33                          ;	 * The normal hook method into BASIC RAM copy takes  48 bytes.
    34                          ;	 * The IRQ hook is more complex and consumes 244 bytes,
    35                          ;	   at least 60 bytes are needed just to provide the
    36                          ;	   a block-copy routine because the ROM routine is not
    37                          ;	   available
    38                          
    39                          ; If active the RAM area below the BASIC ROM is used as buffer, 
    40                          ; otherwise the RAM under the KERNAL ROM.
    41                          ; This opens the possibility to use the RAM area from $E000 to $FFFF,
    42                          ; e.g. as memory for hires graphic.
    43                          ;basic_rom_buffer = 1
    44                          
    45                          ; Use the original MOVBLOCK routine (a copy from the ROM), which
    46                          ; is only allowed if option basic_patch is not selected.
    47                          ; Otherwise an optimized version is used.
    48                          ;orig_movblock = 1
    49                          
    50                          ; Do not display an activiation mark on screen
    51                          ;no_indicator = 1
    52                          
    53                          
    54                          ; Option dependencies:
    55                          
    56                          ; If the BASIC patch hook is selected, the original MOVBLOCK routine
    57                          ; could be easily used.
    58                          !ifdef basic_patch {
    59                          orig_movblock = 1
    60                          }
    61                          
    62                          ; In case of the BASIC patch variant the buffer must not placed in 
    63                          ; the RAM area below the BASIC ROM!
    64                          !ifdef basic_patch {
    65                            !ifdef basic_rom_buffer {
    66                              !error "Invalid option: basic_rom_buffer and basic_patch must no active at the same time!"
    67                            }
    68                          }
    69                          
    70                          
    71                          ; BASIC system variables
    72                          
    73                          TOSS     = $19		; Top of String Descriptor Stack
    74                          EOSS     = $22		; End of String Descriptor Stack +1
    75                          TSSP     = $16		; Current String Descriptor Stack pointer
    76                          
    77                          VARTAB   = $2D		; End of BASIC program = begin of variable area
    78                          ARYTAB   = $2F		; End of variables = begin of arrays
    79                          STREND   = $31		; End of arrays = lowest possible string heap address
    80                          FRETOP   = $33		; Current string heap address
    81                          MEMSIZ   = $37		; Highest RAM address for BASIC, start of
    82                          			; string heap growing downwards.
    83                          MEMBEG   = STREND	; String heap memory begin = STREND
    84                          MEMEND   = MEMSIZ	; String heap memory end
    85                          
    86                          V_IRQ    = $0314	; IRQ vektor, 2 bytes
    87                          
    88                          ; variables
    89                          
    90                          STRPTR   = FRETOP	; String pointer = FRETOP
    91                          STRDP    = $22		; String descriptor address,
    92                          			; overwritten by original MOVBLOCK routine!
    93                          RNGBEG   = $4C		; Region start
    94                          NEWPTR	 = $4E		; New string pointer
    95                          PTR      = $50		; Array pointer
    96                          LEN      = $52		; string length
    97                          ; $54-$56 belegt
    98                          STAT     = $57		; String status, for values in use,
    99                          			; see STAT_* below
   100                          ; $58-5B is overwritten by MOVBLOCK!
   101                          STRADR   = $58		; String address (temp.)
   102                          			; (MOVBLOCK: destination end address +1/destination start
   103                          RNGEND   = $5D		; Region end
   104                          BUFPTR   = $5F		; Buffer pointer
   105                          			; (MOVBLOCK: source start!)
   106                          
   107                          CPTR     = $22		; Pointer for installer routine
   108                          
   109                          ; Zeropage area to be saved
   110                          ZPSTART  = $4C		; First byte to save
   111                          ZPEND    = $52		; Last byte to save
   112                          ZPLEN    = ZPEND-ZPSTART+1
   113                          			; Count of bytes to save
   114                          
   115                          ; Constants
   116                          
   117                          ; for variable STAT (string status):
   118                          STAT_SDS = 5		; Is on String Descriptor Stack
   119                          STAT_VAR = 3		; Is a simple variable
   120                          STAT_ARY = 1		; Is in a array
   121                          
   122                          ; Memory configuration for PROCPORT:
   123                          MEMROM   = %00110111	; BASIC+KERNAL ROM, $37
   124                          MEMBAS   = %00110110	; BASIC RAM+KERNAL ROM, $34
   125                          MEMRAM   = %00110101	; BASIC+KERNAL RAM, $35
   126                          
   127                          ; for activity indicator
   128                          MARKCHAR = "*"          ; Indicator character
   129                          MARKCOL  = 9            ; Indicator color (red)
   130                          MARKOFF  = 40*25-1      ; indicator position (lower right corner)
   131                          MARKVPOS = VIDBASE+MARKOFF
   132                          MARKCPOS = COLBASE+MARKOFF
   133                          
   134                          
   135                          ; Memory locations
   136                          
   137                          GARBCOL  = $B526	; Entry point of the legacy GC
   138                          
   139                          BASIC    = $A000        ; BASIC ROM
   140                          KERNAL   = $E000        ; KERNAL ROM
   141                          ROMSIZE  = $2000        ; ROM length, 8 Kbyte
   142                          
   143                          			; Buffer:
   144                          !ifndef basic_rom_buffer {
   145                          BUF	 = KERNAL	; Buffer under KERNAL ROM
   146                          } else {
   147                          BUF	 = BASIC	; Buffer under BASIC ROM
   148                          }
   149                          BUFSIZE  = ROMSIZE	; Buffer size
   150                          
   151                          VIDPAGE	 = $288		; Page of video RAM
   152                          VIDBASE  = $0400	; Video RAM
   153                          COLBASE  = $D800	; Color RAM
   154                          
   155                          PROCPORT = $01		; Processor port
   156                          
   157                          
   158                          ; Debugging
   159                          
   160                          ;!set debug=1
   161                          
   162                          
   163                          ; Installer
   164                          
   165                          INSTALL
   166                          
   167  c500 2c                 	!byte $2C	; Opcode BIT absolute, Argument 
   168                          			; contains the signature, acts as NOP.
   169  c501 4743               	!text "GC"	; Signature for the loader,
   170                          			; the same an on a fixed
   171                          			; location for all variants!
   172                          !ifdef basic_patch {
   173                          
   174                          	; BASIC-ROM/RAM patch hook
   175                          
   176                          MOVBLOCK = $A3BF	; Move-block in BASIC ROM
   177                          			; destroys $58/$59/$5A/$5B/$22
   178                          
   179                          	; copy BASIC into RAM to patch the GC routine
   180                          	LDA #MEMROM
   181                          	STA PROCPORT	; All ROM (where to copy from)
   182                          	LDY #<BASIC	; ROM start
   183                          	STY CPTR
   184                          	LDA #>BASIC
   185                          	STA CPTR+1	; BASIC ROM start
   186                          	LDX #>($2000)	; BASIC ROM length in pages
   187                          CPYROM	LDA (CPTR),Y	; Read from ROM
   188                          	STA (CPTR),Y	; Write to RAM
   189                          	INY
   190                          	BNE CPYROM
   191                          	INC CPTR+1	; Next page
   192                          	DEX		; Page counter
   193                          	BNE CPYROM
   194                          	LDA PROCPORT	; Switch to RAM
   195                          	AND #%11111110	; "BASIC off" mask
   196                          	STA PROCPORT
   197                          	LDA #<COLLECT	; Write "JMP COLLECT"
   198                          	STA GARBCOL+1	; patch code.
   199                          	LDA #>COLLECT
   200                          	STA GARBCOL+2
   201                          	LDA #$4C	; The "JMP" opcode
   202                          	STA GARBCOL
   203                          	RTS
   204                          } else {
   205                          
   206                          	; IRQ hook
   207                          
   208  c503 78                 	SEI
   209  c504 ad1403             	LDA V_IRQ	; Current IRQ routine
   210  c507 ae1503             	LDX V_IRQ+1
   211  c50a c924               	CMP #<(IRQ)	; Already hooked in?
   212  c50c d004               	BNE HOOK
   213  c50e e0c5               	CPX #>(IRQ)
   214  c510 f010               	BEQ INSTEXIT	; Vektor bereits installiert
   215                          HOOK
   216  c512 8dddc7             	STA ORIGIRQ	; Keep old vector
   217  c515 8edec7             	STX ORIGIRQ+1
   218  c518 a924               	LDA #<(IRQ)	; New GC routine ...
   219  c51a a2c5               	LDX #>(IRQ)
   220  c51c 8d1403             	STA V_IRQ	; hooked in.
   221  c51f 8e1503             	STX V_IRQ+1
   222                          INSTEXIT
   223  c522 58                 	CLI
   224  c523 60                 	RTS
   225                          
   226                          ; The IRQ routine tests if the old GC is running with following
   227                          ; conditions:
   228                          ;   * the stack contains the caller PC for the Open-Space-for-Memory routine ($B62B-1)
   229                          ;   * on the stack is one of three caller PC for the Search-for-Next-String routine
   230                          ;   * the PC lies within the range from $B526 to $B63D.
   231                          ; Particular states have to be handled with a correction of the state
   232                          ; for these cases:
   233                          ;   1. If the PC lies within the range from GC_CRIT_START ot GC_CRIT_END
   234                          ;      the string descriptor is inconsistent.
   235                          ;      $59 is already incremented if GC_CRIT_59 is reached, otherwise
   236                          ;      an additional
   237                          ;         INC $59
   238                          ;      is needed.
   239                          ;      Set the high byte:
   240                          ;         LDY $55
   241                          ;         INY
   242                          ;         INY
   243                          ;         LDA $59
   244                          ;         STA ($4E),y
   245                          ;      Point 3 below corrects the whole descriptor already
   246                          ;      (after tranfering the string), but it doesn't help much
   247                          ;      - save only one byte, but extents the code complexity.
   248                          ;   2. If the PC lies within the range from GC_PHP_START to GC_PHP_END
   249                          ;      the stack is inconsistent -> one byte must be removed from stack.
   250                          ;   3. If the subroutine "Open Space in Memory" is interrupted
   251                          ;      (recognised by the calling address), the current transfer
   252                          ;      of the string has to be finished. This is accomplished by
   253                          ;      normaly return from interrupt, but the changed caller address
   254                          ;      for the subroutine's RTS transfers the control to the
   255                          ;      correction code, which fixes the descriptor and passes
   256                          ;      the control to the new GC.
   257                          ;   4. If the subroutine "Search for Next String" is interrupted
   258                          ;      (recognised by one of the three possible calling addresses)
   259                          ;      the address of the caller is still on stack. The RTI
   260                          ;      passes control to a correction routine, which removes
   261                          ;      two bytes (the caller address) from stack and branches
   262                          ;      to the new GC.
   263                          ;
   264                          ; Otherwise, if the interrupted PC lies within the GC code range
   265                          ; the new GC can be called directly.
   266                          ;
   267                          ; We don't have to bother with the value of Top of String Heap ($33/$34),
   268                          ; no matter where it is interrupted, because the value is already
   269                          ; overwritten and will be recalculated by the new GC.
   270                          ; 
   271                          
   272                          GC_START      = $B526	; PC within this range -> GC active
   273                          GC_END        = $B63C
   274                          GC_CRIT_START = $B632	; PC within this range -> 
   275                          			; descriptor has to be corrected!
   276                          GC_CRIT_END   = $B638
   277                          GC_CRIT_59    = $B635   ; PC beginning with this addr.: $59 is correct
   278                          GC_PHP_START  = $B58A	; PC within this range -> 
   279                          			; remove SR (coming from a PHP) from stack!
   280                          GC_PHP_END    = $B598	; Location of the PLP instruction
   281                          
   282                          CALLER_OPSP   = $B628+2	; PC-1 of the return address from a JSR $A3BF
   283                          			; call (Open Space for Memory) at $B628.
   284                          CALLER_SNS1   = $B561+2 ; PC-1 of the return addresses possible
   285                          CALLER_SNS2   = $B548+2 ; if in routine "Search for Next String"
   286                          CALLER_SNS3   = $B5B8+2
   287                          
   288                          IRQ
   289                          	; IRQ stack frame:
   290                          	; $104,X status register
   291                          	; $105,X low byte PC
   292                          	; $106,X high byte PC
   293                          	; possibly the calling routine:
   294                          	; $107,X low byte of the caller (return-PC)-1
   295                          	; $108,X high byte of the caller (return-PC)-1
   296                          
   297  c524 bd0801             	LDA $108,X	; Caller's PC high
   298  c527 a8                 	TAY
   299  c528 bd0701             	LDA $107,X	; Caller's PC low
   300                          	; callers return-PC-1 in A/Y
   301                          			; Are we in "Open Space"?
   302  c52b c92a               	CMP #<(CALLER_OPSP)
   303  c52d d020               	BNE CHK_SNS
   304  c52f c0b6               	CPY #>(CALLER_OPSP)
   305  c531 d01c               	BNE CHK_SNS
   306                          IN_OPSP
   307                          	; Go back with RTI to the interrupted
   308                          	; copy-routine to ensure that the string
   309                          	; is completely moved. The return address
   310                          	; used by the following RTS passes control
   311                          	; to the correction routine (for the
   312                          	; descriptor) to finally start with the
   313                          	; new GC.
   314  c533 a93e               	LDA #<(CORR_STR-1)
   315  c535 9d0701             	STA $107,X	; RTS expects the address-1 on stack
   316  c538 a9c5               	LDA #>(CORR_STR-1)
   317  c53a 9d0801             	STA $108,X	; Always >0
   318  c53d d02a               	BNE CONT	; Always go back by RTI
   319                          
   320                          CORR_STR
   321  c53f a455               	LDY $55		; Descriptor offset
   322  c541 c8                 	INY		; String length
   323  c542 a558               	LDA $58		; Put string address low
   324  c544 914e               	STA ($4E),Y	; into the descriptor
   325  c546 c8                 	INY
   326  c547 e659               	INC $59		; String address high from MOVBLOCK
   327  c549 a559               	LDA $59		; is one page below, so correct it
   328  c54b 914e               	STA ($4E),Y	; and put it into the descriptor
   329  c54d d070               	BNE START_COLLECT
   330                          			; Branch always, because always >0
   331                          
   332                          	
   333                          CHK_SNS			; We are in "Search for Next String"?
   334  c54f c0b5               	CPY #>(CALLER_SNS1)
   335                          			; High byte is the same for all three addresses!
   336                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   337                          	  !error "High-Byte of CALLER_SNS* are different. They must be all the same!"
   338                          	}
   339  c551 d019               	BNE CHK_PC	; Check only the low byte of these addresses ...
   340  c553 c963               	CMP #<(CALLER_SNS1)
   341  c555 f008               	BEQ IN_SUB
   342  c557 c94a               	CMP #<(CALLER_SNS2)
   343  c559 f004               	BEQ IN_SUB
   344  c55b c9ba               	CMP #<(CALLER_SNS3)
   345  c55d d00d               	BNE CHK_PC
   346                          
   347                          IN_SUB
   348  c55f a9bd               	LDA #<(SKIPSUB) ; Redirect by changing the interruption PC
   349                          IN_PHP
   350  c561 9d0501             	STA $105,X	; Low byte
   351  c564 a9c5               	LDA #>(SKIPSUB)
   352  c566 9d0601             	STA $106,X	; High byte
   353                          			; The following RTI branches to SKIPSUB
   354                          			; where the caller's address is taken from stack.
   355                          
   356  c569 6cddc7             CONT	JMP (ORIGIRQ)	; Pass control to the pre-hooked code (chain).
   357                          
   358                          CHK_PC
   359  c56c bd0601             	LDA $106,X	; Check interruption PC
   360  c56f a8                 	TAY		; High byte
   361  c570 bd0501             	LDA $105,X	; Low byte
   362  c573 c0b5               	CPY #>(GC_START)
   363  c575 90f2               	BCC CONT	; Below GC routine
   364  c577 d004               	BNE +		; Past GC beginning
   365  c579 c926               	CMP #<(GC_START)
   366  c57b 90ec               	BCC CONT	; Below GC routine
   367  c57d c0b6               +	CPY #>(GC_END+1)
   368  c57f 9006               	BCC ++		; In GC routine!
   369  c581 d0e6               	BNE CONT	; Above GC routine
   370  c583 c93d               	CMP #<(GC_END+1)
   371  c585 b0e2               	BCS CONT	; Above GC routine
   372                          ++
   373                          	; The old GC routine has been interrupted!
   374                          	; Are there any special ranges where further action is required?
   375                          
   376                          	; In PHP/PLP section?
   377                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   378                          	  !error "High byte of GC_PHP_START and GC_PHP_END differs!"
   379                          	}
   380  c587 c0b5               	CPY #>(GC_PHP_START)
   381  c589 d00c               	BNE +		; Not in right page, to next range test
   382  c58b c98a               	CMP #<(GC_PHP_START)
   383  c58d 9008               	BCC +		; Below, no stack correction
   384  c58f c999               	CMP #<(GC_PHP_END+1)
   385  c591 b004               	BCS +		; Above, no stack correction
   386  c593 a9be               	LDA #<(SKIPPHP) ; RTI called routine - removes one byte from stack
   387  c595 90ca               	BCC IN_PHP	; C always 0, delays SKIPHP to past RTI
   388                          +
   389                          	; In critical section?
   390                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   391                          	  !error "High byte of GC_CRIT_START and GC_CRIT_END differs!"
   392                          	}
   393  c597 c0b6               	CPY #>(GC_CRIT_START)
   394  c599 d016               	BNE +		; Not in right page, to next range test
   395  c59b c932               	CMP #<(GC_CRIT_START)
   396  c59d 9012               	BCC +		; Below, no stack correction
   397  c59f c939               	CMP #<(GC_CRIT_END+1)
   398  c5a1 b00e               	BCS +		; Above, no stack correction
   399                          
   400                          	; Descriptor correction: set the descriptor's high byte with the
   401                          	; string address, the low byte has been already set.
   402                          	; Otherwise the address in the descriptor would be incosistent!
   403                          	; Caution: The content of $59 is starting with GC_CRIT_59 already
   404                          	; right, but otherwise it has to be corrected by adding 1.
   405                           
   406  c5a3 c935               	CMP #<(GC_CRIT_59)
   407  c5a5 b002               	BCS ++          ; $59 already incremented,
   408  c5a7 e659               	INC $59         ; otherwise correct it!
   409  c5a9 a455               ++	LDY $55		; Descriptor offset
   410  c5ab c8                 	INY		; String length
   411  c5ac c8                 	INY		; String address low (is already set!)
   412  c5ad a559               	LDA $59		; MOVBLOCK high byte, one page to below!
   413  c5af 914e               	STA ($4E),Y	; Just set string address high
   414                          
   415                          	; The previous part could theoretically use the descriptor
   416                          	; correction code at CORR_STR, but this is normally called
   417                          	; in IRQ context which prevents direct usage. It would be
   418                          	; possible if RTI calls CORR_STR instead of START_COLLECT.
   419                          	; But this would also consume 7 bytes to accomplish, saving
   420                          	; only one byte at the expense of readability. Hence, this
   421                          	; way is not taken.
   422                          +
   423                          	; call COLLECT by means of RTI:
   424                          TO_COLLECT
   425  c5b1 a9bf               	LDA #<(START_COLLECT)
   426                          			; Change the return-address on stack for RTI
   427  c5b3 9d0501             	STA $0105,X	; Low byte
   428  c5b6 a9c5               	LDA #>(START_COLLECT)
   429  c5b8 9d0601             	STA $0106,X	; High byte
   430  c5bb d0ac               	BNE CONT	; IRQ continued, RTI starts the new GC
   431                          
   432                          SKIPSUB			; Open-Space or Search-for-Next-String routine aborted:
   433  c5bd 68                 	PLA		; Called by RTI, remove the caller PC (for RTS)
   434                          			; or
   435                          SKIPPHP			; remove the PHP
   436  c5be 68                 	PLA		; transfer execution directly to the new GC (COLLECT).
   437                          START_COLLECT
   438  c5bf a903               	LDA #3
   439  c5c1 8553               	STA $53		; Step size to the next descriptor set to
   440                          			; the defined start value (is not initialized
   441                          			; by old GC!).
   442                          }
   443                          
   444                          ; *** Garbage Collector
   445                          
   446                          COLLECT
   447                          !ifdef debug {
   448                          	JSR gra_on	; Enable graphic (buffer visualization)
   449                          }
   450                          
   451                          	; save zero-page
   452  c5c3 a207               	LDX #ZPLEN	; Counter and index
   453  c5c5 b54b               SAVLOOP	LDA ZPSTART-1,X	; Index runs from count to 1
   454  c5c7 9ddec7             	STA SAVE-1,X	; Save area
   455  c5ca ca                 	DEX
   456  c5cb d0f8               	BNE SAVLOOP
   457                          
   458                          !ifndef no_indicator {
   459                          			; X is zero from before!
   460  c5cd 8622               	STX CPTR	; Pointer low byte = 0
   461  c5cf ae8802             	LDX VIDPAGE	; Startpage of video RAM
   462                          	!if (>MARKOFF) >= 1 {
   463  c5d2 e8                 	INX
   464                          	!if (>MARKOFF) >= 2 {
   465  c5d3 e8                 	INX
   466                          	!if (>MARKOFF) >= 3 {
   467  c5d4 e8                 	INX
   468                          	} } }
   469                          	; X contains now the page plus the offset's high byte
   470  c5d5 8623               	STX CPTR+1
   471  c5d7 a0e7               	LDY #<(MARKOFF)
   472  c5d9 b122               	LDA (CPTR),Y	; Activity indicator on screen:
   473  c5db 8ddbc7             	STA ORIGVID	; Save current character
   474  c5de a92a               	LDA #MARKCHAR
   475  c5e0 9122               	STA (CPTR),Y	; Set mark character
   476  c5e2 ade7db             	LDA MARKCPOS	; Same for the color information
   477  c5e5 8ddcc7             	STA ORIGCOL	; Save current color
   478  c5e8 a909               	LDA #MARKCOL
   479  c5ea 8de7db             	STA MARKCPOS	; Set mark color
   480                          }
   481                          
   482  c5ed a537               	LDA MEMEND	; Set string pointer
   483  c5ef a638               	LDX MEMEND+1	; and region start
   484  c5f1 8533               	STA STRPTR	; to memory end.
   485  c5f3 8634               	STX STRPTR+1
   486  c5f5 854c               	STA RNGBEG
   487  c5f7 864d               	STX RNGBEG+1
   488                          
   489                          ; *** The region where the strings are searched
   490                          
   491                          ;                        STRADR
   492                          ;       +-------------------------------------+
   493                          ;       |                                     |
   494                          ;       |                                     V
   495                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   496                          ;   |L|PTR|      |        not yet        | searched | free |   treated  |
   497                          ;   | |   |      |   handled strings     | strings  |      |   strings  |
   498                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   499                          ;    ^            ^                       ^          ^      ^            ^
   500                          ;    |            |                       |          |      |            |
   501                          ;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
   502                          ;                                                           =FRETOP
   503                          ;   SDS,VAR,ARY  |<-------------------- string heap -------------------->|
   504                          ;
   505                          ; The region RNGBEG to RNGEND (searched strings) has to be reduced by 256 bytes 
   506                          ; from the buffer size, because a string might start on the end of the region
   507                          ; and could exceed the region by maximal 254 bytes. This "overflow"
   508                          ; needs to fit into the buffer and so a page is reserved for this!
   509                          
   510                          NEXTBLOCK
   511  c5f9 a533               	LDA STRPTR	; NEWPTR pulled along
   512  c5fb 854e               	STA NEWPTR	; with BUFPTR in parallel.
   513  c5fd a534               	LDA STRPTR+1
   514  c5ff 854f               	STA NEWPTR+1
   515  c601 a64c               	LDX RNGBEG	; Region already at end
   516  c603 a54d               	LDA RNGBEG+1	; of string heap?
   517  c605 e431               	CPX STREND
   518  c607 d004               	BNE +
   519  c609 c532               	CMP STREND+1
   520  c60b f01b               	BEQ EXIT	; Yes -> finished
   521                          +
   522  c60d 865d               	STX RNGEND	; Move by buffer length - 256
   523  c60f 855e               	STA RNGEND+1	; down to lower addresses.
   524                          	!if <BUFSIZE > 0 {
   525                          	  !error "BUFSIZE is not a multiple of 256 ($100)!"
   526                          	}
   527  c611 38                 	SEC		
   528  c612 e91f               	SBC #(>BUFSIZE-1)
   529                          			; Region length in pages,
   530                          			; could be exceeded by max. 254 bytes!
   531  c614 9008               	BCC LASTRANGE	; < 0 = underflow (for sure <STREND)
   532  c616 854d               	STA RNGBEG+1
   533  c618 e431               	CPX STREND	; End of string heap reached?
   534  c61a e532               	SBC STREND+1
   535  c61c b02c               	BCS STRINRANGE	; Start of region >= bottom of string heap
   536                          LASTRANGE
   537  c61e a531               	LDA STREND	; Start of region = start of free
   538  c620 a632               	LDX STREND+1	; Memory area (bottom of string heap)
   539  c622 854c               	STA RNGBEG	; 
   540  c624 864d               	STX RNGBEG+1	; 
   541  c626 d022               	BNE STRINRANGE	; Always, because high byte >0
   542                          
   543                          
   544                          ; *** Garbage collection finished
   545                          
   546                          EXIT
   547                          	; Zero-page registers
   548  c628 a207               	LDX #ZPLEN	; Count and index
   549  c62a bddec7             RESLOOP	LDA SAVE-1,X	; Index runs from count to 1
   550  c62d 954b               	STA ZPSTART-1,X	; Restore from save area
   551  c62f ca                 	DEX
   552  c630 d0f8               	BNE RESLOOP
   553                          
   554                          !ifndef no_indicator {
   555                          			; X is zero from before!
   556  c632 8622               	STX CPTR	; Pointer low byte = 0
   557  c634 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   558                          	!if (>MARKOFF) >= 1 {
   559  c637 e8                 	INX
   560                          	!if (>MARKOFF) >= 2 {
   561  c638 e8                 	INX
   562                          	!if (>MARKOFF) >= 3 {
   563  c639 e8                 	INX
   564                          	} } }
   565                          	; X contains now the page plus the offset's high byte
   566  c63a 8623               	STX CPTR+1
   567  c63c a0e7               	LDY #<(MARKOFF)
   568  c63e addbc7             	LDA ORIGVID	; Clear activation indicator:
   569  c641 9122               	STA (CPTR),Y	; restore character on screen
   570  c643 addcc7             	LDA ORIGCOL	; and its color.
   571  c646 8de7db             	STA MARKCPOS
   572                          }
   573                          !ifdef debug {
   574                          	JSR gra_off
   575                          }
   576  c649 60                 	RTS
   577                          
   578                          
   579                          ; *** Find all strings within the region
   580                          
   581                          STRINRANGE
   582                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   583  c64a a9c0               	LDA #>(BUF+BUFSIZE)
   584  c64c 8560               	STA BUFPTR+1
   585  c64e a900               	LDA #<(BUF+BUFSIZE)
   586  c650 855f               	STA BUFPTR
   587                          } else {
   588                          			; Special case: buffer end $FFFF
   589                          	LDA #0		; Set buffer pointer start value 
   590                          	STA BUFPTR	; to $10000 (65536) = 0
   591                          	STA BUFPTR+1
   592                          }
   593  c652 38                 	SEC		; Initialize search
   594  c653 24                 	!byte $24	; BIT ZP, skip next instruction
   595                          NEXTSTR	
   596  c654 18                 	CLC		; Continue search
   597                          NEXTSTR1
   598  c655 20c7c6             	JSR GETSA	; Fetch next string address
   599  c658 f03b               	BEQ COPYBACK	; No String found anymore
   600                          			; Address in X/Y
   601                          
   602  c65a 98                 	TYA		; High byte
   603  c65b e45d               	CPX RNGEND	; X/A >= RNGEND:
   604  c65d e55e               	SBC RNGEND+1	; Above region, try
   605  c65f b0f3               	BCS NEXTSTR	; next string!
   606                          
   607  c661 98                 	TYA		; High Byte
   608  c662 e44c               	CPX RNGBEG	; X/A < RNGBEG:
   609  c664 e54d               	SBC RNGBEG+1	; Below the region, so
   610  c666 90ed               	BCC NEXTSTR1	; next string!
   611                          			; Within the region:
   612  c668 a55f               	LDA BUFPTR	; Carry flag always set
   613  c66a e552               	SBC LEN		; Buffer pointer moved
   614  c66c 855f               	STA BUFPTR	; down by string length.
   615  c66e b002               	BCS +		; Check high byte overflow
   616  c670 c660               	DEC BUFPTR+1
   617                          
   618  c672 8459               +	STY STRADR+1	; Save as string address
   619  c674 8658               	STX STRADR	; for copy action.
   620                          
   621  c676 a452               	LDY LEN		; String length (always > 0)
   622  c678 d004               	BNE NBENTRY	; Always start with decrement
   623  c67a b158               NEXTBYT	LDA (STRADR),Y	; Copy string to buffer,
   624  c67c 915f               	STA (BUFPTR),Y	; write through to RAM below ROM.
   625  c67e 88                 NBENTRY	DEY		; Index and counter
   626  c67f d0f9               	BNE NEXTBYT
   627  c681 b158               	LDA (STRADR),Y	; Also the 0th byte,
   628  c683 915f               	STA (BUFPTR),Y	; extra handled
   629                          
   630  c685 38                 	SEC		; New string address:
   631  c686 a54e               	LDA NEWPTR	; Simply pull along
   632  c688 e552               	SBC LEN		; the pointer, by
   633  c68a 854e               	STA NEWPTR	; subtract the length.
   634  c68c b002               	BCS +		; 
   635  c68e c64f               	DEC NEWPTR+1	; High byte overflow
   636                          +
   637  c690 2096c7             	JSR CORR	; Fix the string address in the
   638                          			; descriptor, Z=0 on leave.
   639  c693 d0bf               	BNE NEXTSTR	; Always, continue with next string
   640                          
   641                          
   642                          ; *** Transfer buffer back to the string heap
   643                          
   644                          ; 0 ------------------------------------------- FFFF	
   645                          ;      destination                  source
   646                          ;          +--------------------------+
   647                          ;          |                          |
   648                          ;          V                         /^\
   649                          ;     |||||||||||                |||||||||||
   650                          ;     ^          ^               ^          ^ 
   651                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   652                          
   653                          COPYBACK
   654                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   655  c695 a55f               	LDA BUFPTR	; Buffer is empty ...
   656  c697 c900               	CMP #<(BUF+BUFSIZE)
   657  c699 d006               	BNE +
   658  c69b a560               	LDA BUFPTR+1	; if pointer is still on end
   659  c69d c9c0               	CMP #>(BUF+BUFSIZE)
   660  c69f f023               	BEQ NOCOPY	; Skip copy if buffer is empty,
   661                          +			; to NEXTBLOCK, far branch needed.
   662                          } else {
   663                          			; Special case: buffer end at $FFFF
   664                          	LDA BUFPTR	; Buffer empty
   665                          	ORA BUFPTR+1	; if pointer is 0 (at end).
   666                          	BEQ NOCOPY	; Skip copy if buffer is empty,
   667                          			; to NEXTBLOCK, far branch needed.
   668                          }
   669                          
   670                          !ifdef orig_movblock {
   671                          	LDA STRPTR	; Original MOVBLOCK needs
   672                          	LDX STRPTR+1	; destination block end +1
   673                          } else {
   674  c6a1 a54e               	LDA NEWPTR	; Optimized MOVBLOCK needs
   675  c6a3 a64f               	LDX NEWPTR+1	; destination block start
   676                          }
   677  c6a5 8558               	STA $58		; = STRADR,
   678  c6a7 8659               	STX $59		; Depending on MOVBLOCK variant
   679                          			; end+1 or begin of destination block.
   680                          
   681                          !ifdef orig_movblock {
   682                          	LDA NEWPTR	; For original MOVBLOCK only,
   683                          	LDX NEWPTR+1	; otherwise already in A/X.
   684                          }
   685  c6a9 8533               	STA STRPTR	; New FRETOP so far
   686  c6ab 8634               	STX STRPTR+1
   687                          
   688                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   689  c6ad a900               	LDA #<(BUF+BUFSIZE)
   690  c6af 855a               	STA $5A		; Source block end+1
   691  c6b1 a9c0               	LDA #>(BUF+BUFSIZE)
   692  c6b3 855b               	STA $5B
   693                          } else {
   694                          			; Special case buffer end at $FFFF
   695                          	LDA #$00	; Source block end+1
   696                          	STA $5A
   697                          	STA $5B
   698                          }
   699                          			; Source block begin = BUFPTR
   700                          
   701  c6b5 78                 	SEI		; Don't allow interrupts while
   702  c6b6 a501               	LDA PROCPORT	; KERNAL ROM is switched off
   703                          			; to gain access to the RAM under ROM.
   704  c6b8 48                 	PHA		; Save previous memory configuration
   705  c6b9 a935               	LDA #MEMRAM	; With KERNAL also BASIC ROM is off!
   706                          			; Both buffer $F000 as well as $A000
   707                          			; will have both ROMs switched off.
   708  c6bb 8501               	STA PROCPORT
   709                          
   710  c6bd 20a5c7             	JSR MOVBLOCK	; BASIC's routine to move a block,
   711                          			; with IRQ hook a own routine must be used
   712                          			; because the BASIC ROM isn't switched in!
   713                          			; Otherwise we have the ROM copy in RAM
   714                          			; Z=1
   715  c6c0 68                 	PLA		; Restore previous memory configuration
   716  c6c1 8501               	STA PROCPORT	; KERNAL ROM should be active again
   717  c6c3 58                 	CLI
   718                          NOCOPY
   719  c6c4 4cf9c5             	JMP NEXTBLOCK	; next region
   720                          
   721                          
   722                          ;
   723                          ; *** Get String - fetch next string with length > 0
   724                          ;
   725                          ; ( C-flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-flag )
   726                          ;
   727                          ; If C=1 start from the beginning at SDS, otherwise
   728                          ; continue with position STRDP and string status STAT.
   729                          ; If the Z-Flag is set no string is available,
   730                          ; otherwise X/Y contains the address and LEN
   731                          ; the length of the string.
   732                          
   733  c6c7 905a               GETSA	BCC CHECKTYPE	; C=0 -> continue with string according to STAT
   734                          			; otherwise start with at SDS ...
   735                          
   736                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   737                          ;
   738                          ;    +-------------+
   739                          ;    |             V
   740                          ;    |    belegt->|<-frei
   741                          ;   +-+     +-----+-----+-----+
   742                          ;   | |     |S|L|H|S|L|H|S|L|H|
   743                          ;   +-+     +-----+-----+-----+
   744                          ;    ^       ^     ^     ^     ^
   745                          ;    $16     $19   $1C   $1F   $22
   746                          ;    TSSP    TOSS
   747                          
   748                          DESCSTACK
   749  c6c9 a000               	LDY #0
   750  c6cb 8423               	STY STRDP+1	; Zero descriptor pointer high
   751  c6cd a905               	LDA #STAT_SDS	; Set status to SDS
   752  c6cf 8557               	STA STAT
   753  c6d1 a219               	LDX #TOSS	; Start of SDS
   754  c6d3 d005               	BNE ISDSTEND	; branch always
   755  c6d5 a622               DSTACK	LDX STRDP
   756  c6d7 e8                 NEXTDST	INX		; next descriptor
   757  c6d8 e8                 	INX
   758  c6d9 e8                 	INX
   759                          ISDSTEND
   760  c6da e416               	CPX TSSP	; SDS finished?
   761  c6dc f010               	BEQ VARS
   762  c6de b500               	LDA 0,X		; Check string length
   763  c6e0 f0f5               	BEQ NEXTDST
   764  c6e2 8552               	STA LEN		; Return variables:
   765  c6e4 8622               	STX STRDP	; length, descriptor address
   766  c6e6 b502               	LDA 2,X		; String address high
   767  c6e8 a8                 	TAY
   768  c6e9 b501               	LDA 1,X		; String address low
   769  c6eb aa                 	TAX
   770  c6ec 98                 	TYA		; Always not zero, Z=0
   771  c6ed 60                 	RTS		; Returns address in X/Y
   772                          
   773                          ; *** Look up simple variables: VARTAB to ARYTAB
   774                          
   775  c6ee a52d               VARS	LDA VARTAB	; Begin of variables
   776  c6f0 a62e               	LDX VARTAB+1
   777  c6f2 8522               	STA STRDP
   778  c6f4 8623               	STX STRDP+1
   779  c6f6 a003               	LDY #STAT_VAR	; Set status to variables
   780  c6f8 8457               	STY STAT
   781  c6fa d00b               	BNE ISVAREND	; Branch always
   782                          VAR
   783  c6fc 18                 NEXTVAR	CLC		; Next variable
   784  c6fd a522               	LDA STRDP
   785  c6ff 6907               	ADC #7		; Advance to next variable
   786  c701 8522               	STA STRDP
   787  c703 9002               	BCC ISVAREND
   788  c705 e623               	INC STRDP+1	; Overflow high byte
   789                          ISVAREND
   790  c707 c52f               	CMP ARYTAB
   791  c709 d006               	BNE CHECKVAR
   792  c70b a623               	LDX STRDP+1	; Variable end (=array start)?
   793  c70d e430               	CPX ARYTAB+1
   794  c70f f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   795                          CHECKVAR
   796  c711 a000               	LDY #0		; Variable name
   797  c713 b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   798  c715 30e5               	BMI NEXTVAR	; No string, to next variable
   799  c717 c8                 	INY
   800  c718 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   801  c71a 10e0               	BPL NEXTVAR	; No string, to next variable
   802  c71c c8                 	INY
   803  c71d b122               	LDA (STRDP),Y	; String length
   804  c71f f0db               	BEQ NEXTVAR	; = 0, to next variable
   805  c721 d063               	BNE RETGETSA
   806                          
   807                          CHECKTYPE
   808  c723 a557               	LDA STAT	; GETSA intro with C=0
   809  c725 c903               	CMP #STAT_VAR	; String status?
   810  c727 9042               	BCC ARRAY	; =1 -> arrays
   811  c729 f0d1               	BEQ VAR		; =3 -> variables
   812  c72b 4cd5c6             	JMP DSTACK	; =5 -> SDS
   813                          
   814                          ; *** Look up arrays: ARYTAB to STREND
   815                          
   816  c72e 8550               ARRAYS	STA PTR		; A/X set from simple variable processing,
   817  c730 8651               	STX PTR+1	; pointing the start of arrays.
   818  c732 a001               	LDY #STAT_ARY
   819  c734 8457               	STY STAT	; Set status to arrays
   820                          ISARREND
   821  c736 a550               	LDA PTR
   822  c738 a651               	LDX PTR+1
   823  c73a e432               CHKAEND	CPX STREND+1	; End of array area?
   824  c73c d004                       BNE NEXTARR
   825  c73e c531               	CMP STREND	; High byte matches, low byte is
   826                          			; less or equal.
   827  c740 f04f               	BEQ NOSTRING	; Arrays finished -> no string
   828                          NEXTARR
   829                          			; Carry always cleared because of CPX/CMP
   830  c742 8522               	STA STRDP	; Start of an array
   831  c744 8623               	STX STRDP+1
   832  c746 a000               	LDY #0
   833  c748 b122               	LDA (STRDP),Y	; Array name
   834  c74a aa                 	TAX		; Array type, keep it for later
   835  c74b c8                 	INY
   836  c74c b122               	LDA (STRDP),Y
   837  c74e 08                 	PHP		; Array type 2nd part, keep also
   838  c74f c8                 	INY
   839  c750 b122               	LDA (STRDP),Y	; Offset to next array
   840  c752 6550               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   841  c754 8550               	STA PTR		; Save start of following array
   842  c756 c8                 	INY
   843  c757 b122               	LDA (STRDP),Y
   844  c759 6551               	ADC PTR+1
   845  c75b 8551               	STA PTR+1
   846  c75d 28                 	PLP		; Fetch array type
   847  c75e 10d6               	BPL ISARREND	; Not a string array
   848  c760 8a                 	TXA		; Fetch array type 2nd part
   849  c761 30d3               	BMI ISARREND	; Not string array
   850  c763 c8                 	INY
   851  c764 b122               	LDA (STRDP),Y	; Number of dimensions
   852  c766 0a                 	ASL		; *2
   853  c767 6905               	ADC #5		; Offset = dimensions*2+5
   854                          			; C=0 as long as dim.. <= 125
   855  c769 d003               	BNE ADVDESC	; Branch always
   856                          ARRAY			; Entry on continuation
   857                          NEXTASTR
   858  c76b 18                 	CLC
   859  c76c a903               	LDA #3		; String descriptor length
   860  c76e 6522               ADVDESC	ADC STRDP	; Advance to next string
   861  c770 8522               	STA STRDP
   862  c772 9002               	BCC +
   863  c774 e623               	INC STRDP+1	; Overflow high byte
   864  c776 c550               +	CMP PTR		; All array elements processed?
   865  c778 d006               	BNE IS0ASTR
   866  c77a a623               	LDX STRDP+1
   867  c77c e451               	CPX PTR+1
   868  c77e f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   869                          IS0ASTR
   870  c780 a000               	LDY #0
   871  c782 b122               	LDA (STRDP),Y	; String length
   872  c784 f0e5               	BEQ NEXTASTR	; Next array element
   873                          RETGETSA
   874  c786 8552               	STA LEN		; Return value: length
   875  c788 c8                 	INY
   876  c789 b122               	LDA (STRDP),Y	; String address low
   877  c78b aa                 	TAX
   878  c78c c8                 	INY
   879  c78d b122               	LDA (STRDP),Y	; String address high
   880  c78f a8                 	TAY		; Always not zero, Z=0
   881  c790 60                 	RTS		; Return address in X/Y
   882                          NOSTRING
   883  c791 a900               	LDA #0		; Length 0 
   884  c793 8552               	STA LEN		; No string found
   885  c795 60                 	RTS		; Z=1
   886                          
   887                          ;
   888                          ; *** Correct string Address in descriptor
   889                          ;
   890                          ; ( NEWPTR, STRDP, STAT -> )
   891                          ;
   892  c796 a557               CORR	LDA STAT	; String status
   893  c798 2903               	AND #%011	; Just 2 bits, giving the
   894  c79a a8                 	TAY		; offset to the descriptor ...
   895  c79b a54e               	LDA NEWPTR	;
   896  c79d 9122               	STA (STRDP),Y	; which differs for SDS
   897  c79f c8                 	INY		; and array elements!
   898  c7a0 a54f               	LDA NEWPTR+1
   899  c7a2 9122               	STA (STRDP),Y
   900  c7a4 60                 	RTS
   901                          
   902                          
   903                          ;
   904                          ; ***  MOVBLOCK routines (needed for IRQ hook method)
   905                          ;
   906                          
   907                          !ifndef basic_patch {
   908                          
   909                            !ifndef orig_movblock {
   910                          	; The optimized implementation:
   911                          	;
   912                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   913                          	; isn't available while switching off the KERNAL ROM, which
   914                          	; switches the BASIC ROM off also. In this case we have to redefine
   915                          	; this task with a separate routine which is shorter and slightly
   916                          	; faster than the original from the ROM.
   917                          
   918                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   919                          	; Overlapping works as long as the destination address is below the
   920                          	; source block.
   921                          	; Input: $5F/$60 source start address
   922                          	;	 $5A/$5B source end address+1
   923                          	;	 $58/$59 destination start address
   924                          	; Destroyed: A, X, Y
   925                          	; Output: $58/$59 has the value destination end address +1
   926                          	;          X = 0
   927                          	;	   Y = 0 (if the block length is greater 0)
   928                          	;	   Z-flag = 1
   929                          MOVBLOCK
   930  c7a5 38                         SEC
   931  c7a6 a55a                       LDA $5A       ; Source end address low
   932  c7a8 e55f                       SBC $5F       ; Minus source begin address low
   933  c7aa a8                         TAY           ; Block length low
   934  c7ab a55b                       LDA $5B       ; Source end address high
   935  c7ad e560                       SBC $60       ; Minus source begin address high
   936  c7af aa                         TAX           ; Block length high, usage as DEC-DEC counter
   937  c7b0 98                         TYA           ; Length low
   938  c7b1 f019                       BEQ copy      ; Nothing more to do if 0
   939  c7b3 18                         CLC           ; Length in A
   940  c7b4 655f                       ADC $5F       ; Source begin address corrected by 
   941  c7b6 855f                       STA $5F       ; low-byte offset: -(-length) -> +length
   942  c7b8 b002                       BCS +         ; Because this is like a subtraction
   943  c7ba c660                       DEC $60       ; C=0 means to correct begin address high.
   944                          +
   945  c7bc 98                         TYA           ; Length low
   946  c7bd 18                         CLC
   947  c7be 6558                       ADC $58       ; Destination begin address corrected by
   948  c7c0 8558                       STA $58       ; low-byte offset: -(-length) -> +length
   949  c7c2 b002                       BCS +         ; Because this is like a subtraction
   950  c7c4 c659                       DEC $59       ; C=0 means to correct begin address high.
   951                          +
   952  c7c6 e8                 	INX           ; Page counter (all full and one partial)
   953  c7c7 98                 	TYA           ; Length low
   954  c7c8 49ff               	EOR #$FF      ; Negate (two's complement):
   955  c7ca a8                 	TAY           ; NOT(X)+1
   956  c7cb c8                 	INY
   957  c7cc b15f               copy    LDA ($5F),Y   ; Copy source 
   958  c7ce 9158                       STA ($58),Y   ; to destination, with increasing addresses
   959  c7d0 c8                         INY
   960  c7d1 d0f9                       BNE copy
   961  c7d3 e660                       INC $60       ; Source high
   962  c7d5 e659                       INC $59       ; Destination high
   963  c7d7 ca                         DEX           ; Block counter
   964  c7d8 d0f2                       BNE copy
   965  c7da 60                         RTS
   966                          	; Takes 6 bytes less compared to the original routine.
   967                          
   968                            } else {
   969                          
   970                          	; The original routine taken from the ROM:
   971                          	;
   972                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   973                          	; isn't available while switching off the KERNAL ROM, which
   974                          	; switches the BASIC ROM off also. In this case we have to redefine
   975                          	; this task with a separate routine which is shorter and slightly
   976                          	; faster than the original from the ROM.
   977                          
   978                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   979                          	; Overlapping works as long as the destination's end address is 
   980                          	; above the source block.
   981                          	; Input: $5F/$60 source start address
   982                          	;	 $5A/$5B source end address+1
   983                          	;	 $58/$59 destination end address+1
   984                          	; Destroyed: A, X, Y
   985                          	; Output: $58/$59 has the value destination end address+1-256
   986                          	;          X = 0
   987                          	;	   Y = 0 (if the block length is greater 0)
   988                          	;	   Z-flag = 1
   989                          
   990                          MOVBLOCK
   991                                  SEC
   992                                  LDA $5A       ; End address low
   993                                  SBC $5F       ; Minus begin address low
   994                                  STA $22       ; Length low
   995                                  TAY
   996                                  LDA $5B       ; End address high
   997                                  SBC $60       ; Minus begin address high
   998                                  TAX           ; Length high
   999                                  INX           ; Length as DEC-DEC counter
  1000                                  TYA           ; Length low
  1001                                  BEQ +         ; If not zero, then correct
  1002                                  LDA $5A       ; the end address by low-byte offset
  1003                                  SEC           ; from length.
  1004                                  SBC $22       ; Length low
  1005                                  STA $5A       ;
  1006                                  BCS ++
  1007                                  DEC $5B       ; Overflow high byte 
  1008                                  SEC
  1009                          ++      LDA $58       ; Correct the destination end address
  1010                                  SBC $22       ; by low-byte offset (length low).
  1011                                  STA $58
  1012                                  BCS +++	      ; Overflow high byte
  1013                                  DEC $59       ; 
  1014                                  BCC +++
  1015                          -       LDA ($5A),Y   ; Copy source to destination
  1016                                  STA ($58),Y   ; from higher to lower addresses.
  1017                          +++     DEY
  1018                                  BNE -
  1019                                  LDA ($5A),Y   ; Source
  1020                                  STA ($58),Y   ; Destination
  1021                          +       DEC $5B       ; Source high
  1022                                  DEC $59       ; Destination high
  1023                                  DEX           ; Block counter
  1024                                  BNE --
  1025                                  RTS
  1026                            }
  1027                          }
  1028                          
  1029                          
  1030                          !ifdef debug {
  1031                          !source "debug.asm"
  1032                          }
  1033                          
  1034                          !ifndef no_indicator {
  1035  c7db 00                 ORIGVID !byte 0		; Original character of marker position
  1036  c7dc 00                 ORIGCOL !byte 0		; Original color of marker position
  1037                          }
  1038                          !ifndef basic_patch {
  1039  c7dd 0000               ORIGIRQ !byte 0,0	; Original IRQ vector
  1040                          }
  1041  c7df 00                 SAVE	!byte 0		; Saved zero-page variables
  1042                          *=*+ZPLEN-1
  1043                          
  1044                          
