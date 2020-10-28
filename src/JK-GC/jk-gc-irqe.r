
; ******** Source: jk-gc.asm
     1                          ;
     2                          ; *************************
     3                          ; *  GARBAGE  COLLECTION  *
     4                          ; *   from Johann Klasek  *
     5                          ; *   j AT klasek DOT at  *
     6                          ; * 1985-12-27 VERS. 1.1  *
     7                          ; * 2013-11-24 VERS. 2.0  *
     8                          ; * 2019-02-15 VERS. 2.1  *
     9                          ; * 2020-10-10 VERS. 2.2  *
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
   216  c512 8db6c7             	STA ORIGIRQ	; Keep old vector
   217  c515 8eb7c7             	STX ORIGIRQ+1
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
   234                          ;      the string descriptor is inconsistent -> set the high byte:
   235                          ;         LDY $55
   236                          ;         INY
   237                          ;         INY
   238                          ;         LDA $59
   239                          ;         STA ($4E),y
   240                          ;      Point 3 below corrects the whole descriptor already
   241                          ;      (after tranfering the string), but it doesn't help much
   242                          ;      - save only one byte, but extents the code complexity.
   243                          ;   2. If the PC lies within the range from GC_PHP_START to GC_PHP_END
   244                          ;      the stack is inconsistent -> one byte must be removed from stack.
   245                          ;   3. If the subroutine "Open Space in Memory" is interrupted
   246                          ;      (recognised by the calling address), the current transfer
   247                          ;      of the string has to be finished. This is accomplished by
   248                          ;      normaly return from interrupt, but the changed caller address
   249                          ;      for the subroutine's RTS transfers the control to the
   250                          ;      correction code, which fixes the descriptor and passes
   251                          ;      the control to the new GC.
   252                          ;   4. If the subroutine "Search for Next String" is interrupted
   253                          ;      (recognised by one of the three possible calling addresses)
   254                          ;      the address of the caller is still on stack. The RTI
   255                          ;      passes control to a correction routine, which removes
   256                          ;      two bytes (the caller address) from stack and branches
   257                          ;      to the new GC.
   258                          ;
   259                          ; Otherwise, if the interrupted PC lies within the GC code range
   260                          ; the new GC can be called directly.
   261                          ;
   262                          ; We don't have to bother with the value of Top of String Heap ($33/$34),
   263                          ; no matter where it is interrupted, because the value is already
   264                          ; overwritten and will be recalculated by the new GC.
   265                          ; 
   266                          
   267                          GC_START      = $B526	; PC within this range -> GC active
   268                          GC_END        = $B63C
   269                          GC_CRIT_START = $B632	; PC within this range -> 
   270                          			; descriptor has to be corrected!
   271                          GC_CRIT_END   = $B638
   272                          GC_PHP_START  = $B58A	; PC within this range -> 
   273                          			; remove SR (coming from a PHP) from stack!
   274                          GC_PHP_END    = $B598	; Location of the PLP instruction
   275                          
   276                          CALLER_OPSP   = $B628+2	; PC-1 of the return address from a JSR $A3BF
   277                          			; call (Open Space for Memory) at $B628.
   278                          CALLER_SNS1   = $B561+2 ; PC-1 of the return addresses possible
   279                          CALLER_SNS2   = $B548+2 ; if in routine "Search for Next String"
   280                          CALLER_SNS3   = $B5B8+2
   281                          
   282                          IRQ
   283  c524 78                 	SEI
   284                          
   285                          	; IRQ stack frame:
   286                          	; $104,X status register
   287                          	; $105,X low byte PC
   288                          	; $106,X high byte PC
   289                          	; possibly the calling routine:
   290                          	; $107,X low byte of the caller (return-PC)-1
   291                          	; $108,X high byte of the caller (return-PC)-1
   292                          
   293  c525 bd0801             	LDA $108,X	; Caller's PC high
   294  c528 a8                 	TAY
   295  c529 bd0701             	LDA $107,X	; Caller's PC low
   296                          	; callers return-PC-1 in A/Y
   297                          			; Are we in "Open Space"?
   298  c52c c92a               	CMP #<(CALLER_OPSP)
   299  c52e d01e               	BNE CHK_SNS
   300  c530 c0b6               	CPY #>(CALLER_OPSP)
   301  c532 d01a               	BNE CHK_SNS
   302                          IN_OPSP
   303                          	; Go back with RTI to the interrupted
   304                          	; copy-routine to ensure that the string
   305                          	; is completely moved. The return address
   306                          	; used by the following RTS passes control
   307                          	; to the correction routine (for the
   308                          	; descriptor) to finally start with the
   309                          	; new GC.
   310  c534 a93f               	LDA #<(CORR_STR-1)
   311  c536 9d0701             	STA $107,X	; RTS expects the address-1 on stack
   312  c539 a9c5               	LDA #>(CORR_STR-1)
   313  c53b 9d0801             	STA $108,X	; Always >0
   314  c53e d028               	BNE CONT	; Always go back by RTI
   315                          
   316                          CORR_STR
   317  c540 a455               	LDY $55		; Descriptor offset
   318  c542 c8                 	INY		; String length
   319  c543 a558               	LDA $58		; Put string address low
   320  c545 914e               	STA ($4E),Y	; into the descriptor
   321  c547 c8                 	INY
   322  c548 a559               	LDA $59		; Put string address high
   323  c54a 914e               	STA ($4E),Y	; into the descriptor
   324  c54c d069               	BNE START_COLLECT
   325                          			; Branch always, because always >0
   326                          
   327                          	
   328                          CHK_SNS			; We are in "Search for Next String"?
   329  c54e c0b5               	CPY #>(CALLER_SNS1)
   330                          			; High byte is the same for all three addresses!
   331                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   332                          	  !error "High-Byte of CALLER_SNS* are different. They must be all the same!"
   333                          	}
   334  c550 d019               	BNE CHK_PC	; Check only the low byte of these addresses ...
   335  c552 c963               	CMP #<(CALLER_SNS1)
   336  c554 f008               	BEQ IN_SUB
   337  c556 c94a               	CMP #<(CALLER_SNS2)
   338  c558 f004               	BEQ IN_SUB
   339  c55a c9ba               	CMP #<(CALLER_SNS3)
   340  c55c d00d               	BNE CHK_PC
   341                          
   342                          IN_SUB
   343  c55e a9b5               	LDA #<(SKIPSUB) ; Redirect by changing the interruption PC
   344  c560 9d0501             	STA $105,X	; Low byte
   345  c563 a9c5               	LDA #>(SKIPSUB)
   346  c565 9d0601             	STA $106,X	; High byte
   347                          			; The following RTI branches to SKIPSUB
   348                          			; where the caller's address is taken from stack.
   349                          
   350  c568 6cb6c7             CONT	JMP (ORIGIRQ)	; Pass control to the pre-hooked code (chain).
   351                          
   352                          CHK_PC
   353  c56b bd0601             	LDA $106,X	; Check interruption PC
   354  c56e a8                 	TAY		; High byte
   355  c56f bd0501             	LDA $105,X	; Low byte
   356  c572 c0b5               	CPY #>(GC_START)
   357  c574 90f2               	BCC CONT	; Below GC routine
   358  c576 d004               	BNE +		; Past GC beginning
   359  c578 c926               	CMP #<(GC_START)
   360  c57a 90ec               	BCC CONT	; Below GC routine
   361  c57c c0b6               +	CPY #>(GC_END+1)
   362  c57e 9006               	BCC ++		; In GC routine!
   363  c580 d0e6               	BNE CONT	; Above GC routine
   364  c582 c93d               	CMP #<(GC_END+1)
   365  c584 b0e2               	BCS CONT	; Above GC routine
   366                          ++
   367                          	; The old GC routine has been interrupted!
   368                          	; Are there any special ranges where further action is required?
   369                          
   370                          	; In PHP/PLP section?
   371                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   372                          	  !error "High byte of GC_PHP_START and GC_PHP_END differs!"
   373                          	}
   374  c586 c0b5               	CPY #>(GC_PHP_START)
   375  c588 d00b               	BNE +		; Not in right page, to next range test
   376  c58a c98a               	CMP #<(GC_PHP_START)
   377  c58c 9007               	BCC +		; Below, no stack correction
   378  c58e c999               	CMP #<(GC_PHP_END+1)
   379  c590 b003               	BCS +		; Above, no stack correction
   380                          	; Stack-Korrektur:
   381  c592 68                 	PLA		; Remove SR from stack, N and Z changed, but not C
   382  c593 9014               	BCC TO_COLLECT	; C always 0, RTI passes control to COLLECT
   383                          +
   384                          	; In critical section?
   385                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   386                          	  !error "High byte of GC_CRIT_START and GC_CRIT_END differs!"
   387                          	}
   388  c595 c0b6               	CPY #>(GC_CRIT_START)
   389  c597 d010               	BNE +		; Not in right page, to next range test
   390  c599 c932               	CMP #<(GC_CRIT_START)
   391  c59b 900c               	BCC +		; Below, no stack correction
   392  c59d c939               	CMP #<(GC_CRIT_END+1)
   393  c59f b008               	BCS +		; Above, no stack correction
   394                          
   395                          	; Descriptor correction: set the descriptor's high byte with the
   396                          	; string address, the low byte has been already set.
   397                          	; Otherwise the address in the descriptor would be incosistent!
   398  c5a1 a455               	LDY $55		; Descriptor offset
   399  c5a3 c8                 	INY		; String length
   400  c5a4 c8                 	INY		; String address low (is already set!)
   401  c5a5 a559               	LDA $59
   402  c5a7 914e               	STA ($4E),Y	; Just set string address high
   403                          
   404                          	; The previous part could theoretically use the descriptor
   405                          	; correction code at CORR_STR, but this is normally called
   406                          	; in IRQ context which prevents direct usage. It would be
   407                          	; possible if RTI calls CORR_STR instead of START_COLLECT.
   408                          	; But this would also consume 7 bytes to accomplish, saving
   409                          	; only one byte at the expense of readability. Hence, this
   410                          	; way is not taken.
   411                          +
   412                          	; call COLLECT by means of RTI:
   413                          TO_COLLECT
   414  c5a9 a9b7               	LDA #<(START_COLLECT)
   415                          			; Change the return-address on stack for RTI
   416  c5ab 9d0501             	STA $0105,X	; Low byte
   417  c5ae a9c5               	LDA #>(START_COLLECT)
   418  c5b0 9d0601             	STA $0106,X	; High byte
   419  c5b3 d0b3               	BNE CONT	; IRQ continued, RTI starts the new GC
   420                          
   421                          SKIPSUB			; Open-Space or Search-for-Next-String routine aborted:
   422  c5b5 68                 	PLA		; Called by RTI, remove the caller PC (for RTS)
   423  c5b6 68                 	PLA		; leading back into the GC, transfer execution directly
   424                          			; to the new GC (COLLECT).
   425                          START_COLLECT
   426  c5b7 a903               	LDA #3
   427  c5b9 8553               	STA $53		; Step size to the next descriptor set to
   428                          			; the defined start value (is not initialized
   429                          			; by old GC!).
   430                          }
   431                          
   432                          ; *** Garbage Collector
   433                          
   434                          COLLECT
   435                          !ifdef debug {
   436                          	JSR gra_on	; Enable graphic (buffer visualization)
   437                          }
   438                          !ifndef no_indicator {
   439  c5bb ade707             	LDA MARKVPOS	; Activity indicator on screen:
   440  c5be 8db4c7             	STA ORIGVID	; Save current character
   441  c5c1 a92a               	LDA #MARKCHAR
   442  c5c3 8de707             	STA MARKVPOS	; Set mark character
   443  c5c6 ade7db             	LDA MARKCPOS	; Same for the color information
   444  c5c9 8db5c7             	STA ORIGCOL	; Save current color
   445  c5cc a909               	LDA #MARKCOL
   446  c5ce 8de7db             	STA MARKCPOS	; Set mark color
   447                          }
   448                          	; save zero-page
   449  c5d1 a207               	LDX #ZPLEN	; Counter and index
   450  c5d3 b54b               SAVLOOP	LDA ZPSTART-1,X	; Index runs from count to 1
   451  c5d5 9db7c7             	STA SAVE-1,X	; Save area
   452  c5d8 ca                 	DEX
   453  c5d9 d0f8               	BNE SAVLOOP
   454                          
   455  c5db a537               	LDA MEMEND	; Set string pointer
   456  c5dd a638               	LDX MEMEND+1	; and region start
   457  c5df 8533               	STA STRPTR	; to memory end.
   458  c5e1 8634               	STX STRPTR+1
   459  c5e3 854c               	STA RNGBEG
   460  c5e5 864d               	STX RNGBEG+1
   461                          
   462                          ; *** The region where the strings are searched
   463                          
   464                          ;                        STRADR
   465                          ;       +-------------------------------------+
   466                          ;       |                                     |
   467                          ;       |                                     V
   468                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   469                          ;   |L|PTR|      |        not yet        | searched | free |   treated  |
   470                          ;   | |   |      |   handled strings     | strings  |      |   strings  |
   471                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   472                          ;    ^            ^                       ^          ^      ^            ^
   473                          ;    |            |                       |          |      |            |
   474                          ;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
   475                          ;                                                           =FRETOP
   476                          ;   SDS,VAR,ARY  |<-------------------- string heap -------------------->|
   477                          ;
   478                          ; The region RNGBEG to RNGEND (searched strings) has to be reduced by 256 bytes 
   479                          ; from the buffer size, because a string might start on the end of the region
   480                          ; and could exceed the region by maximal 254 bytes. This "overflow"
   481                          ; needs to fit into the buffer and so a page is reserved for this!
   482                          
   483                          NEXTBLOCK
   484  c5e7 a533               	LDA STRPTR	; NEWPTR pulled along
   485  c5e9 854e               	STA NEWPTR	; with BUFPTR in parallel.
   486  c5eb a534               	LDA STRPTR+1
   487  c5ed 854f               	STA NEWPTR+1
   488  c5ef a64c               	LDX RNGBEG	; Region already at end
   489  c5f1 a54d               	LDA RNGBEG+1	; of string heap?
   490  c5f3 e431               	CPX STREND
   491  c5f5 d004               	BNE +
   492  c5f7 c532               	CMP STREND+1
   493  c5f9 f01b               	BEQ EXIT	; Yes -> finished
   494                          +
   495  c5fb 865d               	STX RNGEND	; Move by buffer length - 256
   496  c5fd 855e               	STA RNGEND+1	; down to lower addresses.
   497                          	!if <BUFSIZE > 0 {
   498                          	  !error "BUFSIZE is not a multiple of 256 ($100)!"
   499                          	}
   500  c5ff 38                 	SEC		
   501  c600 e91f               	SBC #(>BUFSIZE-1)
   502                          			; Region length in pages,
   503                          			; could be exceeded by max. 254 bytes!
   504  c602 9008               	BCC LASTRANGE	; < 0 = underflow (for sure <STREND)
   505  c604 854d               	STA RNGBEG+1
   506  c606 e431               	CPX STREND	; End of string heap reached?
   507  c608 e532               	SBC STREND+1
   508  c60a b021               	BCS STRINRANGE	; Start of region >= bottom of string heap
   509                          LASTRANGE
   510  c60c a531               	LDA STREND	; Start of region = start of free
   511  c60e a632               	LDX STREND+1	; Memory area (bottom of string heap)
   512  c610 854c               	STA RNGBEG	; 
   513  c612 864d               	STX RNGBEG+1	; 
   514  c614 d017               	BNE STRINRANGE	; Always, because high byte >0
   515                          
   516                          
   517                          ; *** Garbage collection finished
   518                          
   519                          EXIT
   520                          	; Zero-page registers
   521  c616 a207               	LDX #ZPLEN	; Count and index
   522  c618 bdb7c7             RESLOOP	LDA SAVE-1,X	; Index runs from count to 1
   523  c61b 954b               	STA ZPSTART-1,X	; Restore from save area
   524  c61d ca                 	DEX
   525  c61e d0f8               	BNE RESLOOP
   526                          
   527                          !ifndef no_indicator {
   528  c620 adb4c7             	LDA ORIGVID	; Clear activation indicator:
   529  c623 8de707             	STA MARKVPOS	; restore character and its
   530  c626 adb5c7             	LDA ORIGCOL	; color.
   531  c629 8de7db             	STA MARKCPOS
   532                          }
   533                          !ifdef debug {
   534                          	JSR gra_off
   535                          }
   536  c62c 60                 	RTS
   537                          
   538                          
   539                          ; *** Find all strings within the region
   540                          
   541                          STRINRANGE
   542                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   543                          	LDA #>(BUF+BUFSIZE)
   544                          	STA BUFPTR+1
   545                          	LDA #<(BUF+BUFSIZE)
   546                          	STA BUFPTR
   547                          } else {
   548                          			; Special case: buffer end $FFFF
   549  c62d a900               	LDA #0		; Set buffer pointer start value 
   550  c62f 855f               	STA BUFPTR	; to $10000 (65536) = 0
   551  c631 8560               	STA BUFPTR+1
   552                          }
   553  c633 38                 	SEC		; Initialize search
   554  c634 24                 	!byte $24	; BIT ZP, skip next instruction
   555                          NEXTSTR	
   556  c635 18                 	CLC		; Continue search
   557                          NEXTSTR1
   558  c636 20a0c6             	JSR GETSA	; Fetch next string address
   559  c639 f03b               	BEQ COPYBACK	; No String found anymore
   560                          			; Address in X/Y
   561                          
   562  c63b 98                 	TYA		; High byte
   563  c63c e45d               	CPX RNGEND	; X/A >= RNGEND:
   564  c63e e55e               	SBC RNGEND+1	; Above region, try
   565  c640 b0f3               	BCS NEXTSTR	; next string!
   566                          
   567  c642 98                 	TYA		; High Byte
   568  c643 e44c               	CPX RNGBEG	; X/A < RNGBEG:
   569  c645 e54d               	SBC RNGBEG+1	; Below the region, so
   570  c647 90ed               	BCC NEXTSTR1	; next string!
   571                          			; Within the region:
   572  c649 a55f               	LDA BUFPTR	; Carry flag always set
   573  c64b e552               	SBC LEN		; Buffer pointer moved
   574  c64d 855f               	STA BUFPTR	; down by string length.
   575  c64f b002               	BCS +		; Check high byte overflow
   576  c651 c660               	DEC BUFPTR+1
   577                          
   578  c653 8459               +	STY STRADR+1	; Save as string address
   579  c655 8658               	STX STRADR	; for copy action.
   580                          
   581  c657 a452               	LDY LEN		; String length (always > 0)
   582  c659 d004               	BNE NBENTRY	; Always start with decrement
   583  c65b b158               NEXTBYT	LDA (STRADR),Y	; Copy string to buffer,
   584  c65d 915f               	STA (BUFPTR),Y	; write through to RAM below ROM.
   585  c65f 88                 NBENTRY	DEY		; Index and counter
   586  c660 d0f9               	BNE NEXTBYT
   587  c662 b158               	LDA (STRADR),Y	; Also the 0th byte,
   588  c664 915f               	STA (BUFPTR),Y	; extra handled
   589                          
   590  c666 38                 	SEC		; New string address:
   591  c667 a54e               	LDA NEWPTR	; Simply pull along
   592  c669 e552               	SBC LEN		; the pointer, by
   593  c66b 854e               	STA NEWPTR	; subtract the length.
   594  c66d b002               	BCS +		; 
   595  c66f c64f               	DEC NEWPTR+1	; High byte overflow
   596                          +
   597  c671 206fc7             	JSR CORR	; Fix the string address in the
   598                          			; descriptor, Z=0 on leave.
   599  c674 d0bf               	BNE NEXTSTR	; Always, continue with next string
   600                          
   601                          
   602                          ; *** Transfer buffer back to the string heap
   603                          
   604                          ; 0 ------------------------------------------- FFFF	
   605                          ;      destination                  source
   606                          ;          +--------------------------+
   607                          ;          |                          |
   608                          ;          V                         /^\
   609                          ;     |||||||||||                |||||||||||
   610                          ;     ^          ^               ^          ^ 
   611                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   612                          
   613                          COPYBACK
   614                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   615                          	LDA BUFPTR	; Buffer is empty ...
   616                          	CMP #<(BUF+BUFSIZE)
   617                          	BNE +
   618                          	LDA BUFPTR+1	; if pointer is still on end
   619                          	CMP #>(BUF+BUFSIZE)
   620                          	BEQ NOCOPY	; Skip copy if buffer is empty,
   621                          +			; to NEXTBLOCK, far branch needed.
   622                          } else {
   623                          			; Special case: buffer end at $FFFF
   624  c676 a55f               	LDA BUFPTR	; Buffer empty
   625  c678 0560               	ORA BUFPTR+1	; if pointer is 0 (at end).
   626  c67a f021               	BEQ NOCOPY	; Skip copy if buffer is empty,
   627                          			; to NEXTBLOCK, far branch needed.
   628                          }
   629                          
   630                          !ifdef orig_movblock {
   631                          	LDA STRPTR	; Original MOVBLOCK needs
   632                          	LDX STRPTR+1	; destination block end +1
   633                          } else {
   634  c67c a54e               	LDA NEWPTR	; Optimized MOVBLOCK needs
   635  c67e a64f               	LDX NEWPTR+1	; destination block start
   636                          }
   637  c680 8558               	STA $58		; = STRADR,
   638  c682 8659               	STX $59		; Depending on MOVBLOCK variant
   639                          			; end+1 or begin of destination block.
   640                          
   641                          !ifdef orig_movblock {
   642                          	LDA NEWPTR	; For original MOVBLOCK only,
   643                          	LDX NEWPTR+1	; otherwise already in A/X.
   644                          }
   645  c684 8533               	STA STRPTR	; New FRETOP so far
   646  c686 8634               	STX STRPTR+1
   647                          
   648                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   649                          	LDA #<(BUF+BUFSIZE)
   650                          	STA $5A		; Source block end+1
   651                          	LDA #>(BUF+BUFSIZE)
   652                          	STA $5B
   653                          } else {
   654                          			; Special case buffer end at $FFFF
   655  c688 a900               	LDA #$00	; Source block end+1
   656  c68a 855a               	STA $5A
   657  c68c 855b               	STA $5B
   658                          }
   659                          			; Source block begin = BUFPTR
   660                          
   661  c68e 78                 	SEI		; Don't allow interrupts while
   662  c68f a501               	LDA PROCPORT	; KERNAL ROM is switched off
   663                          			; to gain access to the RAM under ROM.
   664  c691 48                 	PHA		; Save previous memory configuration
   665  c692 a935               	LDA #MEMRAM	; With KERNAL also BASIC ROM is off!
   666                          			; Both buffer $F000 as well as $A000
   667                          			; will have both ROMs switched off.
   668  c694 8501               	STA PROCPORT
   669                          
   670  c696 207ec7             	JSR MOVBLOCK	; BASIC's routine to move a block,
   671                          			; with IRQ hook a own routine must be used
   672                          			; because the BASIC ROM isn't switched in!
   673                          			; Otherwise we have the ROM copy in RAM
   674                          			; Z=1
   675  c699 68                 	PLA		; Restore previous memory configuration
   676  c69a 8501               	STA PROCPORT	; KERNAL ROM should be active again
   677  c69c 58                 	CLI
   678                          NOCOPY
   679  c69d 4ce7c5             	JMP NEXTBLOCK	; next region
   680                          
   681                          
   682                          ;
   683                          ; *** Get String - fetch next string with length > 0
   684                          ;
   685                          ; ( C-flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-flag )
   686                          ;
   687                          ; If C=1 start from the beginning at SDS, otherwise
   688                          ; continue with position STRDP and string status STAT.
   689                          ; If the Z-Flag is set no string is available,
   690                          ; otherwise X/Y contains the address and LEN
   691                          ; the length of the string.
   692                          
   693  c6a0 905a               GETSA	BCC CHECKTYPE	; C=0 -> continue with string according to STAT
   694                          			; otherwise start with at SDS ...
   695                          
   696                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   697                          ;
   698                          ;    +-------------+
   699                          ;    |             V
   700                          ;    |    belegt->|<-frei
   701                          ;   +-+     +-----+-----+-----+
   702                          ;   | |     |S|L|H|S|L|H|S|L|H|
   703                          ;   +-+     +-----+-----+-----+
   704                          ;    ^       ^     ^     ^     ^
   705                          ;    $16     $19   $1C   $1F   $22
   706                          ;    TSSP    TOSS
   707                          
   708                          DESCSTACK
   709  c6a2 a000               	LDY #0
   710  c6a4 8423               	STY STRDP+1	; Zero descriptor pointer high
   711  c6a6 a905               	LDA #STAT_SDS	; Set status to SDS
   712  c6a8 8557               	STA STAT
   713  c6aa a219               	LDX #TOSS	; Start of SDS
   714  c6ac d005               	BNE ISDSTEND	; branch always
   715  c6ae a622               DSTACK	LDX STRDP
   716  c6b0 e8                 NEXTDST	INX		; next descriptor
   717  c6b1 e8                 	INX
   718  c6b2 e8                 	INX
   719                          ISDSTEND
   720  c6b3 e416               	CPX TSSP	; SDS finished?
   721  c6b5 f010               	BEQ VARS
   722  c6b7 b500               	LDA 0,X		; Check string length
   723  c6b9 f0f5               	BEQ NEXTDST
   724  c6bb 8552               	STA LEN		; Return variables:
   725  c6bd 8622               	STX STRDP	; length, descriptor address
   726  c6bf b502               	LDA 2,X		; String address high
   727  c6c1 a8                 	TAY
   728  c6c2 b501               	LDA 1,X		; String address low
   729  c6c4 aa                 	TAX
   730  c6c5 98                 	TYA		; Always not zero, Z=0
   731  c6c6 60                 	RTS		; Returns address in X/Y
   732                          
   733                          ; *** Look up simple variables: VARTAB to ARYTAB
   734                          
   735  c6c7 a52d               VARS	LDA VARTAB	; Begin of variables
   736  c6c9 a62e               	LDX VARTAB+1
   737  c6cb 8522               	STA STRDP
   738  c6cd 8623               	STX STRDP+1
   739  c6cf a003               	LDY #STAT_VAR	; Set status to variables
   740  c6d1 8457               	STY STAT
   741  c6d3 d00b               	BNE ISVAREND	; Branch always
   742                          VAR
   743  c6d5 18                 NEXTVAR	CLC		; Next variable
   744  c6d6 a522               	LDA STRDP
   745  c6d8 6907               	ADC #7		; Advance to next variable
   746  c6da 8522               	STA STRDP
   747  c6dc 9002               	BCC ISVAREND
   748  c6de e623               	INC STRDP+1	; Overflow high byte
   749                          ISVAREND
   750  c6e0 c52f               	CMP ARYTAB
   751  c6e2 d006               	BNE CHECKVAR
   752  c6e4 a623               	LDX STRDP+1	; Variable end (=array start)?
   753  c6e6 e430               	CPX ARYTAB+1
   754  c6e8 f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   755                          CHECKVAR
   756  c6ea a000               	LDY #0		; Variable name
   757  c6ec b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   758  c6ee 30e5               	BMI NEXTVAR	; No string, to next variable
   759  c6f0 c8                 	INY
   760  c6f1 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   761  c6f3 10e0               	BPL NEXTVAR	; No string, to next variable
   762  c6f5 c8                 	INY
   763  c6f6 b122               	LDA (STRDP),Y	; String length
   764  c6f8 f0db               	BEQ NEXTVAR	; = 0, to next variable
   765  c6fa d063               	BNE RETGETSA
   766                          
   767                          CHECKTYPE
   768  c6fc a557               	LDA STAT	; GETSA intro with C=0
   769  c6fe c903               	CMP #STAT_VAR	; String status?
   770  c700 9042               	BCC ARRAY	; =1 -> arrays
   771  c702 f0d1               	BEQ VAR		; =3 -> variables
   772  c704 4caec6             	JMP DSTACK	; =5 -> SDS
   773                          
   774                          ; *** Look up arrays: ARYTAB to STREND
   775                          
   776  c707 8550               ARRAYS	STA PTR		; A/X set from simple variable processing,
   777  c709 8651               	STX PTR+1	; pointing the start of arrays.
   778  c70b a001               	LDY #STAT_ARY
   779  c70d 8457               	STY STAT	; Set status to arrays
   780                          ISARREND
   781  c70f a550               	LDA PTR
   782  c711 a651               	LDX PTR+1
   783  c713 e432               CHKAEND	CPX STREND+1	; End of array area?
   784  c715 d004                       BNE NEXTARR
   785  c717 c531               	CMP STREND	; High byte matches, low byte is
   786                          			; less or equal.
   787  c719 f04f               	BEQ NOSTRING	; Arrays finished -> no string
   788                          NEXTARR
   789                          			; Carry always cleared because of CPX/CMP
   790  c71b 8522               	STA STRDP	; Start of an array
   791  c71d 8623               	STX STRDP+1
   792  c71f a000               	LDY #0
   793  c721 b122               	LDA (STRDP),Y	; Array name
   794  c723 aa                 	TAX		; Array type, keep it for later
   795  c724 c8                 	INY
   796  c725 b122               	LDA (STRDP),Y
   797  c727 08                 	PHP		; Array type 2nd part, keep also
   798  c728 c8                 	INY
   799  c729 b122               	LDA (STRDP),Y	; Offset to next array
   800  c72b 6550               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   801  c72d 8550               	STA PTR		; Save start of following array
   802  c72f c8                 	INY
   803  c730 b122               	LDA (STRDP),Y
   804  c732 6551               	ADC PTR+1
   805  c734 8551               	STA PTR+1
   806  c736 28                 	PLP		; Fetch array type
   807  c737 10d6               	BPL ISARREND	; Not a string array
   808  c739 8a                 	TXA		; Fetch array type 2nd part
   809  c73a 30d3               	BMI ISARREND	; Not string array
   810  c73c c8                 	INY
   811  c73d b122               	LDA (STRDP),Y	; Number of dimensions
   812  c73f 0a                 	ASL		; *2
   813  c740 6905               	ADC #5		; Offset = dimensions*2+5
   814                          			; C=0 as long as dim.. <= 125
   815  c742 d003               	BNE ADVDESC	; Branch always
   816                          ARRAY			; Entry on continuation
   817                          NEXTASTR
   818  c744 18                 	CLC
   819  c745 a903               	LDA #3		; String descriptor length
   820  c747 6522               ADVDESC	ADC STRDP	; Advance to next string
   821  c749 8522               	STA STRDP
   822  c74b 9002               	BCC +
   823  c74d e623               	INC STRDP+1	; Overflow high byte
   824  c74f c550               +	CMP PTR		; All array elements processed?
   825  c751 d006               	BNE IS0ASTR
   826  c753 a623               	LDX STRDP+1
   827  c755 e451               	CPX PTR+1
   828  c757 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   829                          IS0ASTR
   830  c759 a000               	LDY #0
   831  c75b b122               	LDA (STRDP),Y	; String length
   832  c75d f0e5               	BEQ NEXTASTR	; Next array element
   833                          RETGETSA
   834  c75f 8552               	STA LEN		; Return value: length
   835  c761 c8                 	INY
   836  c762 b122               	LDA (STRDP),Y	; String address low
   837  c764 aa                 	TAX
   838  c765 c8                 	INY
   839  c766 b122               	LDA (STRDP),Y	; String address high
   840  c768 a8                 	TAY		; Always not zero, Z=0
   841  c769 60                 	RTS		; Return address in X/Y
   842                          NOSTRING
   843  c76a a900               	LDA #0		; Length 0 
   844  c76c 8552               	STA LEN		; No string found
   845  c76e 60                 	RTS		; Z=1
   846                          
   847                          ;
   848                          ; *** Correct string Address in descriptor
   849                          ;
   850                          ; ( NEWPTR, STRDP, STAT -> )
   851                          ;
   852  c76f a557               CORR	LDA STAT	; String status
   853  c771 2903               	AND #%011	; Just 2 bits, giving the
   854  c773 a8                 	TAY		; offset to the descriptor ...
   855  c774 a54e               	LDA NEWPTR	;
   856  c776 9122               	STA (STRDP),Y	; which differs for SDS
   857  c778 c8                 	INY		; and array elements!
   858  c779 a54f               	LDA NEWPTR+1
   859  c77b 9122               	STA (STRDP),Y
   860  c77d 60                 	RTS
   861                          
   862                          
   863                          ;
   864                          ; ***  MOVBLOCK routines (needed for IRQ hook method)
   865                          ;
   866                          
   867                          !ifndef basic_patch {
   868                          
   869                            !ifndef orig_movblock {
   870                          	; The optimized implementation:
   871                          	;
   872                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   873                          	; isn't available while switching off the KERNAL ROM, which
   874                          	; switches the BASIC ROM off also. In this case we have to redefine
   875                          	; this task with a separate routine which is shorter and slightly
   876                          	; faster than the original from the ROM.
   877                          
   878                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   879                          	; Overlapping works as long as the destination address is below the
   880                          	; source block.
   881                          	; Input: $5F/$60 source start address
   882                          	;	 $5A/$5B source end address+1
   883                          	;	 $58/$59 destination start address
   884                          	; Destroyed: A, X, Y
   885                          	; Output: $58/$59 has the value destination end address +1
   886                          	;          X = 0
   887                          	;	   Y = 0 (if the block length is greater 0)
   888                          	;	   Z-flag = 1
   889                          MOVBLOCK
   890  c77e 38                         SEC
   891  c77f a55a                       LDA $5A       ; Source end address low
   892  c781 e55f                       SBC $5F       ; Minus source begin address low
   893  c783 a8                         TAY           ; Block length low
   894  c784 a55b                       LDA $5B       ; Source end address high
   895  c786 e560                       SBC $60       ; Minus source begin address high
   896  c788 aa                         TAX           ; Block length high, usage as DEC-DEC counter
   897  c789 98                         TYA           ; Length low
   898  c78a f019                       BEQ copy      ; Nothing more to do if 0
   899  c78c 18                         CLC           ; Length in A
   900  c78d 655f                       ADC $5F       ; Source begin address corrected by 
   901  c78f 855f                       STA $5F       ; low-byte offset: -(-length) -> +length
   902  c791 b002                       BCS +         ; Because this is like a subtraction
   903  c793 c660                       DEC $60       ; C=0 means to correct begin address high.
   904                          +
   905  c795 98                         TYA           ; Length low
   906  c796 18                         CLC
   907  c797 6558                       ADC $58       ; Destination begin address corrected by
   908  c799 8558                       STA $58       ; low-byte offset: -(-length) -> +length
   909  c79b b002                       BCS +         ; Because this is like a subtraction
   910  c79d c659                       DEC $59       ; C=0 means to correct begin address high.
   911                          +
   912  c79f e8                 	INX           ; Page counter (all full and one partial)
   913  c7a0 98                 	TYA           ; Length low
   914  c7a1 49ff               	EOR #$FF      ; Negate (two's complement):
   915  c7a3 a8                 	TAY           ; NOT(X)+1
   916  c7a4 c8                 	INY
   917  c7a5 b15f               copy    LDA ($5F),Y   ; Copy source 
   918  c7a7 9158                       STA ($58),Y   ; to destination, with increasing addresses
   919  c7a9 c8                         INY
   920  c7aa d0f9                       BNE copy
   921  c7ac e660                       INC $60       ; Source high
   922  c7ae e659                       INC $59       ; Destination high
   923  c7b0 ca                         DEX           ; Block counter
   924  c7b1 d0f2                       BNE copy
   925  c7b3 60                         RTS
   926                          	; Takes 6 bytes less compared to the original routine.
   927                          
   928                            } else {
   929                          
   930                          	; The original routine taken from the ROM:
   931                          	;
   932                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   933                          	; isn't available while switching off the KERNAL ROM, which
   934                          	; switches the BASIC ROM off also. In this case we have to redefine
   935                          	; this task with a separate routine which is shorter and slightly
   936                          	; faster than the original from the ROM.
   937                          
   938                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   939                          	; Overlapping works as long as the destination's end address is 
   940                          	; above the source block.
   941                          	; Input: $5F/$60 source start address
   942                          	;	 $5A/$5B source end address+1
   943                          	;	 $58/$59 destination end address+1
   944                          	; Destroyed: A, X, Y
   945                          	; Output: $58/$59 has the value destination end address +1
   946                          	;          X = 0
   947                          	;	   Y = 0 (if the block length is greater 0)
   948                          	;	   Z-flag = 1
   949                          
   950                          MOVBLOCK
   951                                  SEC
   952                                  LDA $5A       ; End address low
   953                                  SBC $5F       ; Minus begin address low
   954                                  STA $22       ; Length low
   955                                  TAY
   956                                  LDA $5B       ; End address high
   957                                  SBC $60       ; Minus begin address high
   958                                  TAX           ; Length high
   959                                  INX           ; Length as DEC-DEC counter
   960                                  TYA           ; Length low
   961                                  BEQ +         ; If not zero, then correct
   962                                  LDA $5A       ; the end address by low-byte offset
   963                                  SEC           ; from length.
   964                                  SBC $22       ; Length low
   965                                  STA $5A       ;
   966                                  BCS ++
   967                                  DEC $5B       ; Overflow high byte 
   968                                  SEC
   969                          ++      LDA $58       ; Correct the destination end address
   970                                  SBC $22       ; by low-byte offset (length low).
   971                                  STA $58
   972                                  BCS +++	      ; Overflow high byte
   973                                  DEC $59       ; 
   974                                  BCC +++
   975                          -       LDA ($5A),Y   ; Copy source to destination
   976                                  STA ($58),Y   ; from higher to lower addresses.
   977                          +++     DEY
   978                                  BNE -
   979                                  LDA ($5A),Y   ; Source
   980                                  STA ($58),Y   ; Destination
   981                          +       DEC $5B       ; Source high
   982                                  DEC $59       ; Destination high
   983                                  DEX           ; Block counter
   984                                  BNE --
   985                                  RTS
   986                            }
   987                          }
   988                          
   989                          
   990                          !ifdef debug {
   991                          !source "debug.asm"
   992                          }
   993                          
   994                          !ifndef no_indicator {
   995  c7b4 00                 ORIGVID !byte 0		; Original character of marker position
   996  c7b5 00                 ORIGCOL !byte 0		; Original color of marker position
   997                          }
   998                          !ifndef basic_patch {
   999  c7b6 0000               ORIGIRQ !byte 0,0	; Original IRQ vector
  1000                          }
  1001  c7b8 00                 SAVE	!byte 0		; Saved zero-page variables
  1002                          *=*+ZPLEN-1
  1003                          
  1004                          
