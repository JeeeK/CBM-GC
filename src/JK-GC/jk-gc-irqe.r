
; ******** Source: jk-gc.asm
     1                          ;
     2                          ; *************************
     3                          ; *  GARBAGE  COLLECTION  *
     4                          ; *   from Johann Klasek  *
     5                          ; *   j AT klasek DOT at  *
     6                          ; * 1985-12-27 VERS. 1.1  *
     7                          ; * 2013-11-24 VERS. 2.0  *
     8                          ; * 2019-02-15 VERS. 2.1  *
     9                          ; * 2020-12-12 VERS. 2.2  *
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
   216  c512 8dd3c7             	STA ORIGIRQ	; Keep old vector
   217  c515 8ed4c7             	STX ORIGIRQ+1
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
   289  c524 78                 	SEI
   290                          
   291                          	; IRQ stack frame:
   292                          	; $104,X status register
   293                          	; $105,X low byte PC
   294                          	; $106,X high byte PC
   295                          	; possibly the calling routine:
   296                          	; $107,X low byte of the caller (return-PC)-1
   297                          	; $108,X high byte of the caller (return-PC)-1
   298                          
   299  c525 bd0801             	LDA $108,X	; Caller's PC high
   300  c528 a8                 	TAY
   301  c529 bd0701             	LDA $107,X	; Caller's PC low
   302                          	; callers return-PC-1 in A/Y
   303                          			; Are we in "Open Space"?
   304  c52c c92a               	CMP #<(CALLER_OPSP)
   305  c52e d020               	BNE CHK_SNS
   306  c530 c0b6               	CPY #>(CALLER_OPSP)
   307  c532 d01c               	BNE CHK_SNS
   308                          IN_OPSP
   309                          	; Go back with RTI to the interrupted
   310                          	; copy-routine to ensure that the string
   311                          	; is completely moved. The return address
   312                          	; used by the following RTS passes control
   313                          	; to the correction routine (for the
   314                          	; descriptor) to finally start with the
   315                          	; new GC.
   316  c534 a93f               	LDA #<(CORR_STR-1)
   317  c536 9d0701             	STA $107,X	; RTS expects the address-1 on stack
   318  c539 a9c5               	LDA #>(CORR_STR-1)
   319  c53b 9d0801             	STA $108,X	; Always >0
   320  c53e d02a               	BNE CONT	; Always go back by RTI
   321                          
   322                          CORR_STR
   323  c540 a455               	LDY $55		; Descriptor offset
   324  c542 c8                 	INY		; String length
   325  c543 a558               	LDA $58		; Put string address low
   326  c545 914e               	STA ($4E),Y	; into the descriptor
   327  c547 c8                 	INY
   328  c548 e659               	INC $59		; String address high from MOVBLOCK
   329  c54a a559               	LDA $59		; is one page below, so correct it
   330  c54c 914e               	STA ($4E),Y	; and put it into the descriptor
   331  c54e d06f               	BNE START_COLLECT
   332                          			; Branch always, because always >0
   333                          
   334                          	
   335                          CHK_SNS			; We are in "Search for Next String"?
   336  c550 c0b5               	CPY #>(CALLER_SNS1)
   337                          			; High byte is the same for all three addresses!
   338                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   339                          	  !error "High-Byte of CALLER_SNS* are different. They must be all the same!"
   340                          	}
   341  c552 d019               	BNE CHK_PC	; Check only the low byte of these addresses ...
   342  c554 c963               	CMP #<(CALLER_SNS1)
   343  c556 f008               	BEQ IN_SUB
   344  c558 c94a               	CMP #<(CALLER_SNS2)
   345  c55a f004               	BEQ IN_SUB
   346  c55c c9ba               	CMP #<(CALLER_SNS3)
   347  c55e d00d               	BNE CHK_PC
   348                          
   349                          IN_SUB
   350  c560 a9bd               	LDA #<(SKIPSUB) ; Redirect by changing the interruption PC
   351  c562 9d0501             	STA $105,X	; Low byte
   352  c565 a9c5               	LDA #>(SKIPSUB)
   353  c567 9d0601             	STA $106,X	; High byte
   354                          			; The following RTI branches to SKIPSUB
   355                          			; where the caller's address is taken from stack.
   356                          
   357  c56a 6cd3c7             CONT	JMP (ORIGIRQ)	; Pass control to the pre-hooked code (chain).
   358                          
   359                          CHK_PC
   360  c56d bd0601             	LDA $106,X	; Check interruption PC
   361  c570 a8                 	TAY		; High byte
   362  c571 bd0501             	LDA $105,X	; Low byte
   363  c574 c0b5               	CPY #>(GC_START)
   364  c576 90f2               	BCC CONT	; Below GC routine
   365  c578 d004               	BNE +		; Past GC beginning
   366  c57a c926               	CMP #<(GC_START)
   367  c57c 90ec               	BCC CONT	; Below GC routine
   368  c57e c0b6               +	CPY #>(GC_END+1)
   369  c580 9006               	BCC ++		; In GC routine!
   370  c582 d0e6               	BNE CONT	; Above GC routine
   371  c584 c93d               	CMP #<(GC_END+1)
   372  c586 b0e2               	BCS CONT	; Above GC routine
   373                          ++
   374                          	; The old GC routine has been interrupted!
   375                          	; Are there any special ranges where further action is required?
   376                          
   377                          	; In PHP/PLP section?
   378                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   379                          	  !error "High byte of GC_PHP_START and GC_PHP_END differs!"
   380                          	}
   381  c588 c0b5               	CPY #>(GC_PHP_START)
   382  c58a d00b               	BNE +		; Not in right page, to next range test
   383  c58c c98a               	CMP #<(GC_PHP_START)
   384  c58e 9007               	BCC +		; Below, no stack correction
   385  c590 c999               	CMP #<(GC_PHP_END+1)
   386  c592 b003               	BCS +		; Above, no stack correction
   387                          	; Stack-Korrektur:
   388  c594 68                 	PLA		; Remove SR from stack, N and Z changed, but not C
   389  c595 901a               	BCC TO_COLLECT	; C always 0, RTI passes control to COLLECT
   390                          +
   391                          	; In critical section?
   392                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   393                          	  !error "High byte of GC_CRIT_START and GC_CRIT_END differs!"
   394                          	}
   395  c597 c0b6               	CPY #>(GC_CRIT_START)
   396  c599 d016               	BNE +		; Not in right page, to next range test
   397  c59b c932               	CMP #<(GC_CRIT_START)
   398  c59d 9012               	BCC +		; Below, no stack correction
   399  c59f c939               	CMP #<(GC_CRIT_END+1)
   400  c5a1 b00e               	BCS +		; Above, no stack correction
   401                          
   402                          	; Descriptor correction: set the descriptor's high byte with the
   403                          	; string address, the low byte has been already set.
   404                          	; Otherwise the address in the descriptor would be incosistent!
   405                          	; Caution: The content of $59 is starting with GC_CRIT_59 already
   406                          	; right, but otherwise it has to be corrected by adding 1.
   407                           
   408  c5a3 c935               	CMP #<(GC_CRIT_59)
   409  c5a5 b002               	BCS ++          ; $59 already incremented,
   410  c5a7 e659               	INC $59         ; otherwise correct it!
   411  c5a9 a455               ++	LDY $55		; Descriptor offset
   412  c5ab c8                 	INY		; String length
   413  c5ac c8                 	INY		; String address low (is already set!)
   414  c5ad a559               	LDA $59		; MOVBLOCK high byte, one page to below!
   415  c5af 914e               	STA ($4E),Y	; Just set string address high
   416                          
   417                          	; The previous part could theoretically use the descriptor
   418                          	; correction code at CORR_STR, but this is normally called
   419                          	; in IRQ context which prevents direct usage. It would be
   420                          	; possible if RTI calls CORR_STR instead of START_COLLECT.
   421                          	; But this would also consume 7 bytes to accomplish, saving
   422                          	; only one byte at the expense of readability. Hence, this
   423                          	; way is not taken.
   424                          +
   425                          	; call COLLECT by means of RTI:
   426                          TO_COLLECT
   427  c5b1 a9bf               	LDA #<(START_COLLECT)
   428                          			; Change the return-address on stack for RTI
   429  c5b3 9d0501             	STA $0105,X	; Low byte
   430  c5b6 a9c5               	LDA #>(START_COLLECT)
   431  c5b8 9d0601             	STA $0106,X	; High byte
   432  c5bb d0ad               	BNE CONT	; IRQ continued, RTI starts the new GC
   433                          
   434                          SKIPSUB			; Open-Space or Search-for-Next-String routine aborted:
   435  c5bd 68                 	PLA		; Called by RTI, remove the caller PC (for RTS)
   436  c5be 68                 	PLA		; leading back into the GC, transfer execution directly
   437                          			; to the new GC (COLLECT).
   438                          START_COLLECT
   439  c5bf a903               	LDA #3
   440  c5c1 8553               	STA $53		; Step size to the next descriptor set to
   441                          			; the defined start value (is not initialized
   442                          			; by old GC!).
   443                          }
   444                          
   445                          ; *** Garbage Collector
   446                          
   447                          COLLECT
   448                          !ifdef debug {
   449                          	JSR gra_on	; Enable graphic (buffer visualization)
   450                          }
   451                          
   452                          	; save zero-page
   453  c5c3 a207               	LDX #ZPLEN	; Counter and index
   454  c5c5 b54b               SAVLOOP	LDA ZPSTART-1,X	; Index runs from count to 1
   455  c5c7 9dd4c7             	STA SAVE-1,X	; Save area
   456  c5ca ca                 	DEX
   457  c5cb d0f8               	BNE SAVLOOP
   458                          
   459                          !ifndef no_indicator {
   460                          			; X is zero from before!
   461  c5cd 8622               	STX CPTR	; Pointer low byte = 0
   462  c5cf ae8802             	LDX VIDPAGE	; Startpage of video RAM
   463                          	!if (>MARKOFF) >= 1 {
   464  c5d2 e8                 	INX
   465                          	!if (>MARKOFF) >= 2 {
   466  c5d3 e8                 	INX
   467                          	!if (>MARKOFF) >= 3 {
   468  c5d4 e8                 	INX
   469                          	} } }
   470                          	; X contains now the page plus the offset's high byte
   471  c5d5 8623               	STX CPTR+1
   472  c5d7 a0e7               	LDY #<(MARKOFF)
   473  c5d9 b122               	LDA (CPTR),Y	; Activity indicator on screen:
   474  c5db 8dd1c7             	STA ORIGVID	; Save current character
   475  c5de a92a               	LDA #MARKCHAR
   476  c5e0 9122               	STA (CPTR),Y	; Set mark character
   477  c5e2 ade7db             	LDA MARKCPOS	; Same for the color information
   478  c5e5 8dd2c7             	STA ORIGCOL	; Save current color
   479  c5e8 a909               	LDA #MARKCOL
   480  c5ea 8de7db             	STA MARKCPOS	; Set mark color
   481                          }
   482                          
   483  c5ed a537               	LDA MEMEND	; Set string pointer
   484  c5ef a638               	LDX MEMEND+1	; and region start
   485  c5f1 8533               	STA STRPTR	; to memory end.
   486  c5f3 8634               	STX STRPTR+1
   487  c5f5 854c               	STA RNGBEG
   488  c5f7 864d               	STX RNGBEG+1
   489                          
   490                          ; *** The region where the strings are searched
   491                          
   492                          ;                        STRADR
   493                          ;       +-------------------------------------+
   494                          ;       |                                     |
   495                          ;       |                                     V
   496                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   497                          ;   |L|PTR|      |        not yet        | searched | free |   treated  |
   498                          ;   | |   |      |   handled strings     | strings  |      |   strings  |
   499                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   500                          ;    ^            ^                       ^          ^      ^            ^
   501                          ;    |            |                       |          |      |            |
   502                          ;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
   503                          ;                                                           =FRETOP
   504                          ;   SDS,VAR,ARY  |<-------------------- string heap -------------------->|
   505                          ;
   506                          ; The region RNGBEG to RNGEND (searched strings) has to be reduced by 256 bytes 
   507                          ; from the buffer size, because a string might start on the end of the region
   508                          ; and could exceed the region by maximal 254 bytes. This "overflow"
   509                          ; needs to fit into the buffer and so a page is reserved for this!
   510                          
   511                          NEXTBLOCK
   512  c5f9 a533               	LDA STRPTR	; NEWPTR pulled along
   513  c5fb 854e               	STA NEWPTR	; with BUFPTR in parallel.
   514  c5fd a534               	LDA STRPTR+1
   515  c5ff 854f               	STA NEWPTR+1
   516  c601 a64c               	LDX RNGBEG	; Region already at end
   517  c603 a54d               	LDA RNGBEG+1	; of string heap?
   518  c605 e431               	CPX STREND
   519  c607 d004               	BNE +
   520  c609 c532               	CMP STREND+1
   521  c60b f01b               	BEQ EXIT	; Yes -> finished
   522                          +
   523  c60d 865d               	STX RNGEND	; Move by buffer length - 256
   524  c60f 855e               	STA RNGEND+1	; down to lower addresses.
   525                          	!if <BUFSIZE > 0 {
   526                          	  !error "BUFSIZE is not a multiple of 256 ($100)!"
   527                          	}
   528  c611 38                 	SEC		
   529  c612 e91f               	SBC #(>BUFSIZE-1)
   530                          			; Region length in pages,
   531                          			; could be exceeded by max. 254 bytes!
   532  c614 9008               	BCC LASTRANGE	; < 0 = underflow (for sure <STREND)
   533  c616 854d               	STA RNGBEG+1
   534  c618 e431               	CPX STREND	; End of string heap reached?
   535  c61a e532               	SBC STREND+1
   536  c61c b02c               	BCS STRINRANGE	; Start of region >= bottom of string heap
   537                          LASTRANGE
   538  c61e a531               	LDA STREND	; Start of region = start of free
   539  c620 a632               	LDX STREND+1	; Memory area (bottom of string heap)
   540  c622 854c               	STA RNGBEG	; 
   541  c624 864d               	STX RNGBEG+1	; 
   542  c626 d022               	BNE STRINRANGE	; Always, because high byte >0
   543                          
   544                          
   545                          ; *** Garbage collection finished
   546                          
   547                          EXIT
   548                          	; Zero-page registers
   549  c628 a207               	LDX #ZPLEN	; Count and index
   550  c62a bdd4c7             RESLOOP	LDA SAVE-1,X	; Index runs from count to 1
   551  c62d 954b               	STA ZPSTART-1,X	; Restore from save area
   552  c62f ca                 	DEX
   553  c630 d0f8               	BNE RESLOOP
   554                          
   555                          !ifndef no_indicator {
   556                          			; X is zero from before!
   557  c632 8622               	STX CPTR	; Pointer low byte = 0
   558  c634 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   559                          	!if (>MARKOFF) >= 1 {
   560  c637 e8                 	INX
   561                          	!if (>MARKOFF) >= 2 {
   562  c638 e8                 	INX
   563                          	!if (>MARKOFF) >= 3 {
   564  c639 e8                 	INX
   565                          	} } }
   566                          	; X contains now the page plus the offset's high byte
   567  c63a 8623               	STX CPTR+1
   568  c63c a0e7               	LDY #<(MARKOFF)
   569  c63e add1c7             	LDA ORIGVID	; Clear activation indicator:
   570  c641 9122               	STA (CPTR),Y	; restore character on screen
   571  c643 add2c7             	LDA ORIGCOL	; and its color.
   572  c646 8de7db             	STA MARKCPOS
   573                          }
   574                          !ifdef debug {
   575                          	JSR gra_off
   576                          }
   577  c649 60                 	RTS
   578                          
   579                          
   580                          ; *** Find all strings within the region
   581                          
   582                          STRINRANGE
   583                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   584                          	LDA #>(BUF+BUFSIZE)
   585                          	STA BUFPTR+1
   586                          	LDA #<(BUF+BUFSIZE)
   587                          	STA BUFPTR
   588                          } else {
   589                          			; Special case: buffer end $FFFF
   590  c64a a900               	LDA #0		; Set buffer pointer start value 
   591  c64c 855f               	STA BUFPTR	; to $10000 (65536) = 0
   592  c64e 8560               	STA BUFPTR+1
   593                          }
   594  c650 38                 	SEC		; Initialize search
   595  c651 24                 	!byte $24	; BIT ZP, skip next instruction
   596                          NEXTSTR	
   597  c652 18                 	CLC		; Continue search
   598                          NEXTSTR1
   599  c653 20bdc6             	JSR GETSA	; Fetch next string address
   600  c656 f03b               	BEQ COPYBACK	; No String found anymore
   601                          			; Address in X/Y
   602                          
   603  c658 98                 	TYA		; High byte
   604  c659 e45d               	CPX RNGEND	; X/A >= RNGEND:
   605  c65b e55e               	SBC RNGEND+1	; Above region, try
   606  c65d b0f3               	BCS NEXTSTR	; next string!
   607                          
   608  c65f 98                 	TYA		; High Byte
   609  c660 e44c               	CPX RNGBEG	; X/A < RNGBEG:
   610  c662 e54d               	SBC RNGBEG+1	; Below the region, so
   611  c664 90ed               	BCC NEXTSTR1	; next string!
   612                          			; Within the region:
   613  c666 a55f               	LDA BUFPTR	; Carry flag always set
   614  c668 e552               	SBC LEN		; Buffer pointer moved
   615  c66a 855f               	STA BUFPTR	; down by string length.
   616  c66c b002               	BCS +		; Check high byte overflow
   617  c66e c660               	DEC BUFPTR+1
   618                          
   619  c670 8459               +	STY STRADR+1	; Save as string address
   620  c672 8658               	STX STRADR	; for copy action.
   621                          
   622  c674 a452               	LDY LEN		; String length (always > 0)
   623  c676 d004               	BNE NBENTRY	; Always start with decrement
   624  c678 b158               NEXTBYT	LDA (STRADR),Y	; Copy string to buffer,
   625  c67a 915f               	STA (BUFPTR),Y	; write through to RAM below ROM.
   626  c67c 88                 NBENTRY	DEY		; Index and counter
   627  c67d d0f9               	BNE NEXTBYT
   628  c67f b158               	LDA (STRADR),Y	; Also the 0th byte,
   629  c681 915f               	STA (BUFPTR),Y	; extra handled
   630                          
   631  c683 38                 	SEC		; New string address:
   632  c684 a54e               	LDA NEWPTR	; Simply pull along
   633  c686 e552               	SBC LEN		; the pointer, by
   634  c688 854e               	STA NEWPTR	; subtract the length.
   635  c68a b002               	BCS +		; 
   636  c68c c64f               	DEC NEWPTR+1	; High byte overflow
   637                          +
   638  c68e 208cc7             	JSR CORR	; Fix the string address in the
   639                          			; descriptor, Z=0 on leave.
   640  c691 d0bf               	BNE NEXTSTR	; Always, continue with next string
   641                          
   642                          
   643                          ; *** Transfer buffer back to the string heap
   644                          
   645                          ; 0 ------------------------------------------- FFFF	
   646                          ;      destination                  source
   647                          ;          +--------------------------+
   648                          ;          |                          |
   649                          ;          V                         /^\
   650                          ;     |||||||||||                |||||||||||
   651                          ;     ^          ^               ^          ^ 
   652                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   653                          
   654                          COPYBACK
   655                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   656                          	LDA BUFPTR	; Buffer is empty ...
   657                          	CMP #<(BUF+BUFSIZE)
   658                          	BNE +
   659                          	LDA BUFPTR+1	; if pointer is still on end
   660                          	CMP #>(BUF+BUFSIZE)
   661                          	BEQ NOCOPY	; Skip copy if buffer is empty,
   662                          +			; to NEXTBLOCK, far branch needed.
   663                          } else {
   664                          			; Special case: buffer end at $FFFF
   665  c693 a55f               	LDA BUFPTR	; Buffer empty
   666  c695 0560               	ORA BUFPTR+1	; if pointer is 0 (at end).
   667  c697 f021               	BEQ NOCOPY	; Skip copy if buffer is empty,
   668                          			; to NEXTBLOCK, far branch needed.
   669                          }
   670                          
   671                          !ifdef orig_movblock {
   672                          	LDA STRPTR	; Original MOVBLOCK needs
   673                          	LDX STRPTR+1	; destination block end +1
   674                          } else {
   675  c699 a54e               	LDA NEWPTR	; Optimized MOVBLOCK needs
   676  c69b a64f               	LDX NEWPTR+1	; destination block start
   677                          }
   678  c69d 8558               	STA $58		; = STRADR,
   679  c69f 8659               	STX $59		; Depending on MOVBLOCK variant
   680                          			; end+1 or begin of destination block.
   681                          
   682                          !ifdef orig_movblock {
   683                          	LDA NEWPTR	; For original MOVBLOCK only,
   684                          	LDX NEWPTR+1	; otherwise already in A/X.
   685                          }
   686  c6a1 8533               	STA STRPTR	; New FRETOP so far
   687  c6a3 8634               	STX STRPTR+1
   688                          
   689                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   690                          	LDA #<(BUF+BUFSIZE)
   691                          	STA $5A		; Source block end+1
   692                          	LDA #>(BUF+BUFSIZE)
   693                          	STA $5B
   694                          } else {
   695                          			; Special case buffer end at $FFFF
   696  c6a5 a900               	LDA #$00	; Source block end+1
   697  c6a7 855a               	STA $5A
   698  c6a9 855b               	STA $5B
   699                          }
   700                          			; Source block begin = BUFPTR
   701                          
   702  c6ab 78                 	SEI		; Don't allow interrupts while
   703  c6ac a501               	LDA PROCPORT	; KERNAL ROM is switched off
   704                          			; to gain access to the RAM under ROM.
   705  c6ae 48                 	PHA		; Save previous memory configuration
   706  c6af a935               	LDA #MEMRAM	; With KERNAL also BASIC ROM is off!
   707                          			; Both buffer $F000 as well as $A000
   708                          			; will have both ROMs switched off.
   709  c6b1 8501               	STA PROCPORT
   710                          
   711  c6b3 209bc7             	JSR MOVBLOCK	; BASIC's routine to move a block,
   712                          			; with IRQ hook a own routine must be used
   713                          			; because the BASIC ROM isn't switched in!
   714                          			; Otherwise we have the ROM copy in RAM
   715                          			; Z=1
   716  c6b6 68                 	PLA		; Restore previous memory configuration
   717  c6b7 8501               	STA PROCPORT	; KERNAL ROM should be active again
   718  c6b9 58                 	CLI
   719                          NOCOPY
   720  c6ba 4cf9c5             	JMP NEXTBLOCK	; next region
   721                          
   722                          
   723                          ;
   724                          ; *** Get String - fetch next string with length > 0
   725                          ;
   726                          ; ( C-flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-flag )
   727                          ;
   728                          ; If C=1 start from the beginning at SDS, otherwise
   729                          ; continue with position STRDP and string status STAT.
   730                          ; If the Z-Flag is set no string is available,
   731                          ; otherwise X/Y contains the address and LEN
   732                          ; the length of the string.
   733                          
   734  c6bd 905a               GETSA	BCC CHECKTYPE	; C=0 -> continue with string according to STAT
   735                          			; otherwise start with at SDS ...
   736                          
   737                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   738                          ;
   739                          ;    +-------------+
   740                          ;    |             V
   741                          ;    |    belegt->|<-frei
   742                          ;   +-+     +-----+-----+-----+
   743                          ;   | |     |S|L|H|S|L|H|S|L|H|
   744                          ;   +-+     +-----+-----+-----+
   745                          ;    ^       ^     ^     ^     ^
   746                          ;    $16     $19   $1C   $1F   $22
   747                          ;    TSSP    TOSS
   748                          
   749                          DESCSTACK
   750  c6bf a000               	LDY #0
   751  c6c1 8423               	STY STRDP+1	; Zero descriptor pointer high
   752  c6c3 a905               	LDA #STAT_SDS	; Set status to SDS
   753  c6c5 8557               	STA STAT
   754  c6c7 a219               	LDX #TOSS	; Start of SDS
   755  c6c9 d005               	BNE ISDSTEND	; branch always
   756  c6cb a622               DSTACK	LDX STRDP
   757  c6cd e8                 NEXTDST	INX		; next descriptor
   758  c6ce e8                 	INX
   759  c6cf e8                 	INX
   760                          ISDSTEND
   761  c6d0 e416               	CPX TSSP	; SDS finished?
   762  c6d2 f010               	BEQ VARS
   763  c6d4 b500               	LDA 0,X		; Check string length
   764  c6d6 f0f5               	BEQ NEXTDST
   765  c6d8 8552               	STA LEN		; Return variables:
   766  c6da 8622               	STX STRDP	; length, descriptor address
   767  c6dc b502               	LDA 2,X		; String address high
   768  c6de a8                 	TAY
   769  c6df b501               	LDA 1,X		; String address low
   770  c6e1 aa                 	TAX
   771  c6e2 98                 	TYA		; Always not zero, Z=0
   772  c6e3 60                 	RTS		; Returns address in X/Y
   773                          
   774                          ; *** Look up simple variables: VARTAB to ARYTAB
   775                          
   776  c6e4 a52d               VARS	LDA VARTAB	; Begin of variables
   777  c6e6 a62e               	LDX VARTAB+1
   778  c6e8 8522               	STA STRDP
   779  c6ea 8623               	STX STRDP+1
   780  c6ec a003               	LDY #STAT_VAR	; Set status to variables
   781  c6ee 8457               	STY STAT
   782  c6f0 d00b               	BNE ISVAREND	; Branch always
   783                          VAR
   784  c6f2 18                 NEXTVAR	CLC		; Next variable
   785  c6f3 a522               	LDA STRDP
   786  c6f5 6907               	ADC #7		; Advance to next variable
   787  c6f7 8522               	STA STRDP
   788  c6f9 9002               	BCC ISVAREND
   789  c6fb e623               	INC STRDP+1	; Overflow high byte
   790                          ISVAREND
   791  c6fd c52f               	CMP ARYTAB
   792  c6ff d006               	BNE CHECKVAR
   793  c701 a623               	LDX STRDP+1	; Variable end (=array start)?
   794  c703 e430               	CPX ARYTAB+1
   795  c705 f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   796                          CHECKVAR
   797  c707 a000               	LDY #0		; Variable name
   798  c709 b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   799  c70b 30e5               	BMI NEXTVAR	; No string, to next variable
   800  c70d c8                 	INY
   801  c70e b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   802  c710 10e0               	BPL NEXTVAR	; No string, to next variable
   803  c712 c8                 	INY
   804  c713 b122               	LDA (STRDP),Y	; String length
   805  c715 f0db               	BEQ NEXTVAR	; = 0, to next variable
   806  c717 d063               	BNE RETGETSA
   807                          
   808                          CHECKTYPE
   809  c719 a557               	LDA STAT	; GETSA intro with C=0
   810  c71b c903               	CMP #STAT_VAR	; String status?
   811  c71d 9042               	BCC ARRAY	; =1 -> arrays
   812  c71f f0d1               	BEQ VAR		; =3 -> variables
   813  c721 4ccbc6             	JMP DSTACK	; =5 -> SDS
   814                          
   815                          ; *** Look up arrays: ARYTAB to STREND
   816                          
   817  c724 8550               ARRAYS	STA PTR		; A/X set from simple variable processing,
   818  c726 8651               	STX PTR+1	; pointing the start of arrays.
   819  c728 a001               	LDY #STAT_ARY
   820  c72a 8457               	STY STAT	; Set status to arrays
   821                          ISARREND
   822  c72c a550               	LDA PTR
   823  c72e a651               	LDX PTR+1
   824  c730 e432               CHKAEND	CPX STREND+1	; End of array area?
   825  c732 d004                       BNE NEXTARR
   826  c734 c531               	CMP STREND	; High byte matches, low byte is
   827                          			; less or equal.
   828  c736 f04f               	BEQ NOSTRING	; Arrays finished -> no string
   829                          NEXTARR
   830                          			; Carry always cleared because of CPX/CMP
   831  c738 8522               	STA STRDP	; Start of an array
   832  c73a 8623               	STX STRDP+1
   833  c73c a000               	LDY #0
   834  c73e b122               	LDA (STRDP),Y	; Array name
   835  c740 aa                 	TAX		; Array type, keep it for later
   836  c741 c8                 	INY
   837  c742 b122               	LDA (STRDP),Y
   838  c744 08                 	PHP		; Array type 2nd part, keep also
   839  c745 c8                 	INY
   840  c746 b122               	LDA (STRDP),Y	; Offset to next array
   841  c748 6550               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   842  c74a 8550               	STA PTR		; Save start of following array
   843  c74c c8                 	INY
   844  c74d b122               	LDA (STRDP),Y
   845  c74f 6551               	ADC PTR+1
   846  c751 8551               	STA PTR+1
   847  c753 28                 	PLP		; Fetch array type
   848  c754 10d6               	BPL ISARREND	; Not a string array
   849  c756 8a                 	TXA		; Fetch array type 2nd part
   850  c757 30d3               	BMI ISARREND	; Not string array
   851  c759 c8                 	INY
   852  c75a b122               	LDA (STRDP),Y	; Number of dimensions
   853  c75c 0a                 	ASL		; *2
   854  c75d 6905               	ADC #5		; Offset = dimensions*2+5
   855                          			; C=0 as long as dim.. <= 125
   856  c75f d003               	BNE ADVDESC	; Branch always
   857                          ARRAY			; Entry on continuation
   858                          NEXTASTR
   859  c761 18                 	CLC
   860  c762 a903               	LDA #3		; String descriptor length
   861  c764 6522               ADVDESC	ADC STRDP	; Advance to next string
   862  c766 8522               	STA STRDP
   863  c768 9002               	BCC +
   864  c76a e623               	INC STRDP+1	; Overflow high byte
   865  c76c c550               +	CMP PTR		; All array elements processed?
   866  c76e d006               	BNE IS0ASTR
   867  c770 a623               	LDX STRDP+1
   868  c772 e451               	CPX PTR+1
   869  c774 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   870                          IS0ASTR
   871  c776 a000               	LDY #0
   872  c778 b122               	LDA (STRDP),Y	; String length
   873  c77a f0e5               	BEQ NEXTASTR	; Next array element
   874                          RETGETSA
   875  c77c 8552               	STA LEN		; Return value: length
   876  c77e c8                 	INY
   877  c77f b122               	LDA (STRDP),Y	; String address low
   878  c781 aa                 	TAX
   879  c782 c8                 	INY
   880  c783 b122               	LDA (STRDP),Y	; String address high
   881  c785 a8                 	TAY		; Always not zero, Z=0
   882  c786 60                 	RTS		; Return address in X/Y
   883                          NOSTRING
   884  c787 a900               	LDA #0		; Length 0 
   885  c789 8552               	STA LEN		; No string found
   886  c78b 60                 	RTS		; Z=1
   887                          
   888                          ;
   889                          ; *** Correct string Address in descriptor
   890                          ;
   891                          ; ( NEWPTR, STRDP, STAT -> )
   892                          ;
   893  c78c a557               CORR	LDA STAT	; String status
   894  c78e 2903               	AND #%011	; Just 2 bits, giving the
   895  c790 a8                 	TAY		; offset to the descriptor ...
   896  c791 a54e               	LDA NEWPTR	;
   897  c793 9122               	STA (STRDP),Y	; which differs for SDS
   898  c795 c8                 	INY		; and array elements!
   899  c796 a54f               	LDA NEWPTR+1
   900  c798 9122               	STA (STRDP),Y
   901  c79a 60                 	RTS
   902                          
   903                          
   904                          ;
   905                          ; ***  MOVBLOCK routines (needed for IRQ hook method)
   906                          ;
   907                          
   908                          !ifndef basic_patch {
   909                          
   910                            !ifndef orig_movblock {
   911                          	; The optimized implementation:
   912                          	;
   913                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   914                          	; isn't available while switching off the KERNAL ROM, which
   915                          	; switches the BASIC ROM off also. In this case we have to redefine
   916                          	; this task with a separate routine which is shorter and slightly
   917                          	; faster than the original from the ROM.
   918                          
   919                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   920                          	; Overlapping works as long as the destination address is below the
   921                          	; source block.
   922                          	; Input: $5F/$60 source start address
   923                          	;	 $5A/$5B source end address+1
   924                          	;	 $58/$59 destination start address
   925                          	; Destroyed: A, X, Y
   926                          	; Output: $58/$59 has the value destination end address +1
   927                          	;          X = 0
   928                          	;	   Y = 0 (if the block length is greater 0)
   929                          	;	   Z-flag = 1
   930                          MOVBLOCK
   931  c79b 38                         SEC
   932  c79c a55a                       LDA $5A       ; Source end address low
   933  c79e e55f                       SBC $5F       ; Minus source begin address low
   934  c7a0 a8                         TAY           ; Block length low
   935  c7a1 a55b                       LDA $5B       ; Source end address high
   936  c7a3 e560                       SBC $60       ; Minus source begin address high
   937  c7a5 aa                         TAX           ; Block length high, usage as DEC-DEC counter
   938  c7a6 98                         TYA           ; Length low
   939  c7a7 f019                       BEQ copy      ; Nothing more to do if 0
   940  c7a9 18                         CLC           ; Length in A
   941  c7aa 655f                       ADC $5F       ; Source begin address corrected by 
   942  c7ac 855f                       STA $5F       ; low-byte offset: -(-length) -> +length
   943  c7ae b002                       BCS +         ; Because this is like a subtraction
   944  c7b0 c660                       DEC $60       ; C=0 means to correct begin address high.
   945                          +
   946  c7b2 98                         TYA           ; Length low
   947  c7b3 18                         CLC
   948  c7b4 6558                       ADC $58       ; Destination begin address corrected by
   949  c7b6 8558                       STA $58       ; low-byte offset: -(-length) -> +length
   950  c7b8 b002                       BCS +         ; Because this is like a subtraction
   951  c7ba c659                       DEC $59       ; C=0 means to correct begin address high.
   952                          +
   953  c7bc e8                 	INX           ; Page counter (all full and one partial)
   954  c7bd 98                 	TYA           ; Length low
   955  c7be 49ff               	EOR #$FF      ; Negate (two's complement):
   956  c7c0 a8                 	TAY           ; NOT(X)+1
   957  c7c1 c8                 	INY
   958  c7c2 b15f               copy    LDA ($5F),Y   ; Copy source 
   959  c7c4 9158                       STA ($58),Y   ; to destination, with increasing addresses
   960  c7c6 c8                         INY
   961  c7c7 d0f9                       BNE copy
   962  c7c9 e660                       INC $60       ; Source high
   963  c7cb e659                       INC $59       ; Destination high
   964  c7cd ca                         DEX           ; Block counter
   965  c7ce d0f2                       BNE copy
   966  c7d0 60                         RTS
   967                          	; Takes 6 bytes less compared to the original routine.
   968                          
   969                            } else {
   970                          
   971                          	; The original routine taken from the ROM:
   972                          	;
   973                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   974                          	; isn't available while switching off the KERNAL ROM, which
   975                          	; switches the BASIC ROM off also. In this case we have to redefine
   976                          	; this task with a separate routine which is shorter and slightly
   977                          	; faster than the original from the ROM.
   978                          
   979                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   980                          	; Overlapping works as long as the destination's end address is 
   981                          	; above the source block.
   982                          	; Input: $5F/$60 source start address
   983                          	;	 $5A/$5B source end address+1
   984                          	;	 $58/$59 destination end address+1
   985                          	; Destroyed: A, X, Y
   986                          	; Output: $58/$59 has the value destination end address+1-256
   987                          	;          X = 0
   988                          	;	   Y = 0 (if the block length is greater 0)
   989                          	;	   Z-flag = 1
   990                          
   991                          MOVBLOCK
   992                                  SEC
   993                                  LDA $5A       ; End address low
   994                                  SBC $5F       ; Minus begin address low
   995                                  STA $22       ; Length low
   996                                  TAY
   997                                  LDA $5B       ; End address high
   998                                  SBC $60       ; Minus begin address high
   999                                  TAX           ; Length high
  1000                                  INX           ; Length as DEC-DEC counter
  1001                                  TYA           ; Length low
  1002                                  BEQ +         ; If not zero, then correct
  1003                                  LDA $5A       ; the end address by low-byte offset
  1004                                  SEC           ; from length.
  1005                                  SBC $22       ; Length low
  1006                                  STA $5A       ;
  1007                                  BCS ++
  1008                                  DEC $5B       ; Overflow high byte 
  1009                                  SEC
  1010                          ++      LDA $58       ; Correct the destination end address
  1011                                  SBC $22       ; by low-byte offset (length low).
  1012                                  STA $58
  1013                                  BCS +++	      ; Overflow high byte
  1014                                  DEC $59       ; 
  1015                                  BCC +++
  1016                          -       LDA ($5A),Y   ; Copy source to destination
  1017                                  STA ($58),Y   ; from higher to lower addresses.
  1018                          +++     DEY
  1019                                  BNE -
  1020                                  LDA ($5A),Y   ; Source
  1021                                  STA ($58),Y   ; Destination
  1022                          +       DEC $5B       ; Source high
  1023                                  DEC $59       ; Destination high
  1024                                  DEX           ; Block counter
  1025                                  BNE --
  1026                                  RTS
  1027                            }
  1028                          }
  1029                          
  1030                          
  1031                          !ifdef debug {
  1032                          !source "debug.asm"
  1033                          }
  1034                          
  1035                          !ifndef no_indicator {
  1036  c7d1 00                 ORIGVID !byte 0		; Original character of marker position
  1037  c7d2 00                 ORIGCOL !byte 0		; Original color of marker position
  1038                          }
  1039                          !ifndef basic_patch {
  1040  c7d3 0000               ORIGIRQ !byte 0,0	; Original IRQ vector
  1041                          }
  1042  c7d5 00                 SAVE	!byte 0		; Saved zero-page variables
  1043                          *=*+ZPLEN-1
  1044                          
  1045                          
