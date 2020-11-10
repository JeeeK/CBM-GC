
; ******** Source: jk-gc.asm
     1                          ;
     2                          ; *************************
     3                          ; *  GARBAGE  COLLECTION  *
     4                          ; *   from Johann Klasek  *
     5                          ; *   j AT klasek DOT at  *
     6                          ; * 1985-12-27 VERS. 1.1  *
     7                          ; * 2013-11-24 VERS. 2.0  *
     8                          ; * 2019-02-15 VERS. 2.1  *
     9                          ; * 2020-10-28 VERS. 2.2  *
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
   180  c503 a937               	LDA #MEMROM
   181  c505 8501               	STA PROCPORT	; All ROM (where to copy from)
   182  c507 a000               	LDY #<BASIC	; ROM start
   183  c509 8422               	STY CPTR
   184  c50b a9a0               	LDA #>BASIC
   185  c50d 8523               	STA CPTR+1	; BASIC ROM start
   186  c50f a220               	LDX #>($2000)	; BASIC ROM length in pages
   187  c511 b122               CPYROM	LDA (CPTR),Y	; Read from ROM
   188  c513 9122               	STA (CPTR),Y	; Write to RAM
   189  c515 c8                 	INY
   190  c516 d0f9               	BNE CPYROM
   191  c518 e623               	INC CPTR+1	; Next page
   192  c51a ca                 	DEX		; Page counter
   193  c51b d0f4               	BNE CPYROM
   194  c51d a501               	LDA PROCPORT	; Switch to RAM
   195  c51f 29fe               	AND #%11111110	; "BASIC off" mask
   196  c521 8501               	STA PROCPORT
   197  c523 a933               	LDA #<COLLECT	; Write "JMP COLLECT"
   198  c525 8d27b5             	STA GARBCOL+1	; patch code.
   199  c528 a9c5               	LDA #>COLLECT
   200  c52a 8d28b5             	STA GARBCOL+2
   201  c52d a94c               	LDA #$4C	; The "JMP" opcode
   202  c52f 8d26b5             	STA GARBCOL
   203  c532 60                 	RTS
   204                          } else {
   205                          
   206                          	; IRQ hook
   207                          
   208                          	SEI
   209                          	LDA V_IRQ	; Current IRQ routine
   210                          	LDX V_IRQ+1
   211                          	CMP #<(IRQ)	; Already hooked in?
   212                          	BNE HOOK
   213                          	CPX #>(IRQ)
   214                          	BEQ INSTEXIT	; Vektor bereits installiert
   215                          HOOK
   216                          	STA ORIGIRQ	; Keep old vector
   217                          	STX ORIGIRQ+1
   218                          	LDA #<(IRQ)	; New GC routine ...
   219                          	LDX #>(IRQ)
   220                          	STA V_IRQ	; hooked in.
   221                          	STX V_IRQ+1
   222                          INSTEXIT
   223                          	CLI
   224                          	RTS
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
   283                          	SEI
   284                          
   285                          	; IRQ stack frame:
   286                          	; $104,X status register
   287                          	; $105,X low byte PC
   288                          	; $106,X high byte PC
   289                          	; possibly the calling routine:
   290                          	; $107,X low byte of the caller (return-PC)-1
   291                          	; $108,X high byte of the caller (return-PC)-1
   292                          
   293                          	LDA $108,X	; Caller's PC high
   294                          	TAY
   295                          	LDA $107,X	; Caller's PC low
   296                          	; callers return-PC-1 in A/Y
   297                          			; Are we in "Open Space"?
   298                          	CMP #<(CALLER_OPSP)
   299                          	BNE CHK_SNS
   300                          	CPY #>(CALLER_OPSP)
   301                          	BNE CHK_SNS
   302                          IN_OPSP
   303                          	; Go back with RTI to the interrupted
   304                          	; copy-routine to ensure that the string
   305                          	; is completely moved. The return address
   306                          	; used by the following RTS passes control
   307                          	; to the correction routine (for the
   308                          	; descriptor) to finally start with the
   309                          	; new GC.
   310                          	LDA #<(CORR_STR-1)
   311                          	STA $107,X	; RTS expects the address-1 on stack
   312                          	LDA #>(CORR_STR-1)
   313                          	STA $108,X	; Always >0
   314                          	BNE CONT	; Always go back by RTI
   315                          
   316                          CORR_STR
   317                          	LDY $55		; Descriptor offset
   318                          	INY		; String length
   319                          	LDA $58		; Put string address low
   320                          	STA ($4E),Y	; into the descriptor
   321                          	INY
   322                          	LDA $59		; Put string address high
   323                          	STA ($4E),Y	; into the descriptor
   324                          	BNE START_COLLECT
   325                          			; Branch always, because always >0
   326                          
   327                          	
   328                          CHK_SNS			; We are in "Search for Next String"?
   329                          	CPY #>(CALLER_SNS1)
   330                          			; High byte is the same for all three addresses!
   331                          	!if (  >(CALLER_SNS1) != >(CALLER_SNS2) | >(CALLER_SNS2) != >(CALLER_SNS3) | >(CALLER_SNS1) != >(CALLER_SNS3)) {
   332                          	  !error "High-Byte of CALLER_SNS* are different. They must be all the same!"
   333                          	}
   334                          	BNE CHK_PC	; Check only the low byte of these addresses ...
   335                          	CMP #<(CALLER_SNS1)
   336                          	BEQ IN_SUB
   337                          	CMP #<(CALLER_SNS2)
   338                          	BEQ IN_SUB
   339                          	CMP #<(CALLER_SNS3)
   340                          	BNE CHK_PC
   341                          
   342                          IN_SUB
   343                          	LDA #<(SKIPSUB) ; Redirect by changing the interruption PC
   344                          	STA $105,X	; Low byte
   345                          	LDA #>(SKIPSUB)
   346                          	STA $106,X	; High byte
   347                          			; The following RTI branches to SKIPSUB
   348                          			; where the caller's address is taken from stack.
   349                          
   350                          CONT	JMP (ORIGIRQ)	; Pass control to the pre-hooked code (chain).
   351                          
   352                          CHK_PC
   353                          	LDA $106,X	; Check interruption PC
   354                          	TAY		; High byte
   355                          	LDA $105,X	; Low byte
   356                          	CPY #>(GC_START)
   357                          	BCC CONT	; Below GC routine
   358                          	BNE +		; Past GC beginning
   359                          	CMP #<(GC_START)
   360                          	BCC CONT	; Below GC routine
   361                          +	CPY #>(GC_END+1)
   362                          	BCC ++		; In GC routine!
   363                          	BNE CONT	; Above GC routine
   364                          	CMP #<(GC_END+1)
   365                          	BCS CONT	; Above GC routine
   366                          ++
   367                          	; The old GC routine has been interrupted!
   368                          	; Are there any special ranges where further action is required?
   369                          
   370                          	; In PHP/PLP section?
   371                          	!if >(GC_PHP_START) != >(GC_PHP_END+1) {
   372                          	  !error "High byte of GC_PHP_START and GC_PHP_END differs!"
   373                          	}
   374                          	CPY #>(GC_PHP_START)
   375                          	BNE +		; Not in right page, to next range test
   376                          	CMP #<(GC_PHP_START)
   377                          	BCC +		; Below, no stack correction
   378                          	CMP #<(GC_PHP_END+1)
   379                          	BCS +		; Above, no stack correction
   380                          	; Stack-Korrektur:
   381                          	PLA		; Remove SR from stack, N and Z changed, but not C
   382                          	BCC TO_COLLECT	; C always 0, RTI passes control to COLLECT
   383                          +
   384                          	; In critical section?
   385                          	!if >(GC_CRIT_START) != >(GC_CRIT_END+1) {
   386                          	  !error "High byte of GC_CRIT_START and GC_CRIT_END differs!"
   387                          	}
   388                          	CPY #>(GC_CRIT_START)
   389                          	BNE +		; Not in right page, to next range test
   390                          	CMP #<(GC_CRIT_START)
   391                          	BCC +		; Below, no stack correction
   392                          	CMP #<(GC_CRIT_END+1)
   393                          	BCS +		; Above, no stack correction
   394                          
   395                          	; Descriptor correction: set the descriptor's high byte with the
   396                          	; string address, the low byte has been already set.
   397                          	; Otherwise the address in the descriptor would be incosistent!
   398                          	LDY $55		; Descriptor offset
   399                          	INY		; String length
   400                          	INY		; String address low (is already set!)
   401                          	LDA $59
   402                          	STA ($4E),Y	; Just set string address high
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
   414                          	LDA #<(START_COLLECT)
   415                          			; Change the return-address on stack for RTI
   416                          	STA $0105,X	; Low byte
   417                          	LDA #>(START_COLLECT)
   418                          	STA $0106,X	; High byte
   419                          	BNE CONT	; IRQ continued, RTI starts the new GC
   420                          
   421                          SKIPSUB			; Open-Space or Search-for-Next-String routine aborted:
   422                          	PLA		; Called by RTI, remove the caller PC (for RTS)
   423                          	PLA		; leading back into the GC, transfer execution directly
   424                          			; to the new GC (COLLECT).
   425                          START_COLLECT
   426                          	LDA #3
   427                          	STA $53		; Step size to the next descriptor set to
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
   438                          
   439                          	; save zero-page
   440  c533 a207               	LDX #ZPLEN	; Counter and index
   441  c535 b54b               SAVLOOP	LDA ZPSTART-1,X	; Index runs from count to 1
   442  c537 9d10c7             	STA SAVE-1,X	; Save area
   443  c53a ca                 	DEX
   444  c53b d0f8               	BNE SAVLOOP
   445                          
   446                          !ifndef no_indicator {
   447                          			; X is zero from before!
   448  c53d 8622               	STX CPTR	; Pointer low byte = 0
   449  c53f ae8802             	LDX VIDPAGE	; Startpage of video RAM
   450                          	!if (>MARKOFF) >= 1 {
   451  c542 e8                 	INX
   452                          	!if (>MARKOFF) >= 2 {
   453  c543 e8                 	INX
   454                          	!if (>MARKOFF) >= 3 {
   455  c544 e8                 	INX
   456                          	} } }
   457                          	; X contains now the page plus the offset's high byte
   458  c545 8623               	STX CPTR+1
   459  c547 a0e7               	LDY #<(MARKOFF)
   460  c549 b122               	LDA (CPTR),Y	; Activity indicator on screen:
   461  c54b 8d0fc7             	STA ORIGVID	; Save current character
   462  c54e a92a               	LDA #MARKCHAR
   463  c550 9122               	STA (CPTR),Y	; Set mark character
   464  c552 ade7db             	LDA MARKCPOS	; Same for the color information
   465  c555 8d10c7             	STA ORIGCOL	; Save current color
   466  c558 a909               	LDA #MARKCOL
   467  c55a 8de7db             	STA MARKCPOS	; Set mark color
   468                          }
   469                          
   470  c55d a537               	LDA MEMEND	; Set string pointer
   471  c55f a638               	LDX MEMEND+1	; and region start
   472  c561 8533               	STA STRPTR	; to memory end.
   473  c563 8634               	STX STRPTR+1
   474  c565 854c               	STA RNGBEG
   475  c567 864d               	STX RNGBEG+1
   476                          
   477                          ; *** The region where the strings are searched
   478                          
   479                          ;                        STRADR
   480                          ;       +-------------------------------------+
   481                          ;       |                                     |
   482                          ;       |                                     V
   483                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   484                          ;   |L|PTR|      |        not yet        | searched | free |   treated  |
   485                          ;   | |   |      |   handled strings     | strings  |      |   strings  |
   486                          ;   +-+-+-+      +-----------------------+----------+------+------------+
   487                          ;    ^            ^                       ^          ^      ^            ^
   488                          ;    |            |                       |          |      |            |
   489                          ;    STRDP        STREND                  RNGBEG     RNGEND STRPTR       MEMSIZ
   490                          ;                                                           =FRETOP
   491                          ;   SDS,VAR,ARY  |<-------------------- string heap -------------------->|
   492                          ;
   493                          ; The region RNGBEG to RNGEND (searched strings) has to be reduced by 256 bytes 
   494                          ; from the buffer size, because a string might start on the end of the region
   495                          ; and could exceed the region by maximal 254 bytes. This "overflow"
   496                          ; needs to fit into the buffer and so a page is reserved for this!
   497                          
   498                          NEXTBLOCK
   499  c569 a533               	LDA STRPTR	; NEWPTR pulled along
   500  c56b 854e               	STA NEWPTR	; with BUFPTR in parallel.
   501  c56d a534               	LDA STRPTR+1
   502  c56f 854f               	STA NEWPTR+1
   503  c571 a64c               	LDX RNGBEG	; Region already at end
   504  c573 a54d               	LDA RNGBEG+1	; of string heap?
   505  c575 e431               	CPX STREND
   506  c577 d004               	BNE +
   507  c579 c532               	CMP STREND+1
   508  c57b f01b               	BEQ EXIT	; Yes -> finished
   509                          +
   510  c57d 865d               	STX RNGEND	; Move by buffer length - 256
   511  c57f 855e               	STA RNGEND+1	; down to lower addresses.
   512                          	!if <BUFSIZE > 0 {
   513                          	  !error "BUFSIZE is not a multiple of 256 ($100)!"
   514                          	}
   515  c581 38                 	SEC		
   516  c582 e91f               	SBC #(>BUFSIZE-1)
   517                          			; Region length in pages,
   518                          			; could be exceeded by max. 254 bytes!
   519  c584 9008               	BCC LASTRANGE	; < 0 = underflow (for sure <STREND)
   520  c586 854d               	STA RNGBEG+1
   521  c588 e431               	CPX STREND	; End of string heap reached?
   522  c58a e532               	SBC STREND+1
   523  c58c b02c               	BCS STRINRANGE	; Start of region >= bottom of string heap
   524                          LASTRANGE
   525  c58e a531               	LDA STREND	; Start of region = start of free
   526  c590 a632               	LDX STREND+1	; Memory area (bottom of string heap)
   527  c592 854c               	STA RNGBEG	; 
   528  c594 864d               	STX RNGBEG+1	; 
   529  c596 d022               	BNE STRINRANGE	; Always, because high byte >0
   530                          
   531                          
   532                          ; *** Garbage collection finished
   533                          
   534                          EXIT
   535                          	; Zero-page registers
   536  c598 a207               	LDX #ZPLEN	; Count and index
   537  c59a bd10c7             RESLOOP	LDA SAVE-1,X	; Index runs from count to 1
   538  c59d 954b               	STA ZPSTART-1,X	; Restore from save area
   539  c59f ca                 	DEX
   540  c5a0 d0f8               	BNE RESLOOP
   541                          
   542                          !ifndef no_indicator {
   543                          			; X is zero from before!
   544  c5a2 8622               	STX CPTR	; Pointer low byte = 0
   545  c5a4 ae8802             	LDX VIDPAGE	; Startpage of video RAM
   546                          	!if (>MARKOFF) >= 1 {
   547  c5a7 e8                 	INX
   548                          	!if (>MARKOFF) >= 2 {
   549  c5a8 e8                 	INX
   550                          	!if (>MARKOFF) >= 3 {
   551  c5a9 e8                 	INX
   552                          	} } }
   553                          	; X contains now the page plus the offset's high byte
   554  c5aa 8623               	STX CPTR+1
   555  c5ac a0e7               	LDY #<(MARKOFF)
   556  c5ae ad0fc7             	LDA ORIGVID	; Clear activation indicator:
   557  c5b1 9122               	STA (CPTR),Y	; restore character on screen
   558  c5b3 ad10c7             	LDA ORIGCOL	; and its color.
   559  c5b6 8de7db             	STA MARKCPOS
   560                          }
   561                          !ifdef debug {
   562                          	JSR gra_off
   563                          }
   564  c5b9 60                 	RTS
   565                          
   566                          
   567                          ; *** Find all strings within the region
   568                          
   569                          STRINRANGE
   570                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   571                          	LDA #>(BUF+BUFSIZE)
   572                          	STA BUFPTR+1
   573                          	LDA #<(BUF+BUFSIZE)
   574                          	STA BUFPTR
   575                          } else {
   576                          			; Special case: buffer end $FFFF
   577  c5ba a900               	LDA #0		; Set buffer pointer start value 
   578  c5bc 855f               	STA BUFPTR	; to $10000 (65536) = 0
   579  c5be 8560               	STA BUFPTR+1
   580                          }
   581  c5c0 38                 	SEC		; Initialize search
   582  c5c1 24                 	!byte $24	; BIT ZP, skip next instruction
   583                          NEXTSTR	
   584  c5c2 18                 	CLC		; Continue search
   585                          NEXTSTR1
   586  c5c3 2031c6             	JSR GETSA	; Fetch next string address
   587  c5c6 f03b               	BEQ COPYBACK	; No String found anymore
   588                          			; Address in X/Y
   589                          
   590  c5c8 98                 	TYA		; High byte
   591  c5c9 e45d               	CPX RNGEND	; X/A >= RNGEND:
   592  c5cb e55e               	SBC RNGEND+1	; Above region, try
   593  c5cd b0f3               	BCS NEXTSTR	; next string!
   594                          
   595  c5cf 98                 	TYA		; High Byte
   596  c5d0 e44c               	CPX RNGBEG	; X/A < RNGBEG:
   597  c5d2 e54d               	SBC RNGBEG+1	; Below the region, so
   598  c5d4 90ed               	BCC NEXTSTR1	; next string!
   599                          			; Within the region:
   600  c5d6 a55f               	LDA BUFPTR	; Carry flag always set
   601  c5d8 e552               	SBC LEN		; Buffer pointer moved
   602  c5da 855f               	STA BUFPTR	; down by string length.
   603  c5dc b002               	BCS +		; Check high byte overflow
   604  c5de c660               	DEC BUFPTR+1
   605                          
   606  c5e0 8459               +	STY STRADR+1	; Save as string address
   607  c5e2 8658               	STX STRADR	; for copy action.
   608                          
   609  c5e4 a452               	LDY LEN		; String length (always > 0)
   610  c5e6 d004               	BNE NBENTRY	; Always start with decrement
   611  c5e8 b158               NEXTBYT	LDA (STRADR),Y	; Copy string to buffer,
   612  c5ea 915f               	STA (BUFPTR),Y	; write through to RAM below ROM.
   613  c5ec 88                 NBENTRY	DEY		; Index and counter
   614  c5ed d0f9               	BNE NEXTBYT
   615  c5ef b158               	LDA (STRADR),Y	; Also the 0th byte,
   616  c5f1 915f               	STA (BUFPTR),Y	; extra handled
   617                          
   618  c5f3 38                 	SEC		; New string address:
   619  c5f4 a54e               	LDA NEWPTR	; Simply pull along
   620  c5f6 e552               	SBC LEN		; the pointer, by
   621  c5f8 854e               	STA NEWPTR	; subtract the length.
   622  c5fa b002               	BCS +		; 
   623  c5fc c64f               	DEC NEWPTR+1	; High byte overflow
   624                          +
   625  c5fe 2000c7             	JSR CORR	; Fix the string address in the
   626                          			; descriptor, Z=0 on leave.
   627  c601 d0bf               	BNE NEXTSTR	; Always, continue with next string
   628                          
   629                          
   630                          ; *** Transfer buffer back to the string heap
   631                          
   632                          ; 0 ------------------------------------------- FFFF	
   633                          ;      destination                  source
   634                          ;          +--------------------------+
   635                          ;          |                          |
   636                          ;          V                         /^\
   637                          ;     |||||||||||                |||||||||||
   638                          ;     ^          ^               ^          ^ 
   639                          ;     NEWPTR     STRPTR          BUFPTR     (BUF+BUFSIZE)
   640                          
   641                          COPYBACK
   642                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   643                          	LDA BUFPTR	; Buffer is empty ...
   644                          	CMP #<(BUF+BUFSIZE)
   645                          	BNE +
   646                          	LDA BUFPTR+1	; if pointer is still on end
   647                          	CMP #>(BUF+BUFSIZE)
   648                          	BEQ NOCOPY	; Skip copy if buffer is empty,
   649                          +			; to NEXTBLOCK, far branch needed.
   650                          } else {
   651                          			; Special case: buffer end at $FFFF
   652  c603 a55f               	LDA BUFPTR	; Buffer empty
   653  c605 0560               	ORA BUFPTR+1	; if pointer is 0 (at end).
   654  c607 f025               	BEQ NOCOPY	; Skip copy if buffer is empty,
   655                          			; to NEXTBLOCK, far branch needed.
   656                          }
   657                          
   658                          !ifdef orig_movblock {
   659  c609 a533               	LDA STRPTR	; Original MOVBLOCK needs
   660  c60b a634               	LDX STRPTR+1	; destination block end +1
   661                          } else {
   662                          	LDA NEWPTR	; Optimized MOVBLOCK needs
   663                          	LDX NEWPTR+1	; destination block start
   664                          }
   665  c60d 8558               	STA $58		; = STRADR,
   666  c60f 8659               	STX $59		; Depending on MOVBLOCK variant
   667                          			; end+1 or begin of destination block.
   668                          
   669                          !ifdef orig_movblock {
   670  c611 a54e               	LDA NEWPTR	; For original MOVBLOCK only,
   671  c613 a64f               	LDX NEWPTR+1	; otherwise already in A/X.
   672                          }
   673  c615 8533               	STA STRPTR	; New FRETOP so far
   674  c617 8634               	STX STRPTR+1
   675                          
   676                          !if ((BUF+BUFSIZE) and $FFFF) != 0  {
   677                          	LDA #<(BUF+BUFSIZE)
   678                          	STA $5A		; Source block end+1
   679                          	LDA #>(BUF+BUFSIZE)
   680                          	STA $5B
   681                          } else {
   682                          			; Special case buffer end at $FFFF
   683  c619 a900               	LDA #$00	; Source block end+1
   684  c61b 855a               	STA $5A
   685  c61d 855b               	STA $5B
   686                          }
   687                          			; Source block begin = BUFPTR
   688                          
   689  c61f 78                 	SEI		; Don't allow interrupts while
   690  c620 a501               	LDA PROCPORT	; KERNAL ROM is switched off
   691                          			; to gain access to the RAM under ROM.
   692  c622 48                 	PHA		; Save previous memory configuration
   693  c623 a935               	LDA #MEMRAM	; With KERNAL also BASIC ROM is off!
   694                          			; Both buffer $F000 as well as $A000
   695                          			; will have both ROMs switched off.
   696  c625 8501               	STA PROCPORT
   697                          
   698  c627 20bfa3             	JSR MOVBLOCK	; BASIC's routine to move a block,
   699                          			; with IRQ hook a own routine must be used
   700                          			; because the BASIC ROM isn't switched in!
   701                          			; Otherwise we have the ROM copy in RAM
   702                          			; Z=1
   703  c62a 68                 	PLA		; Restore previous memory configuration
   704  c62b 8501               	STA PROCPORT	; KERNAL ROM should be active again
   705  c62d 58                 	CLI
   706                          NOCOPY
   707  c62e 4c69c5             	JMP NEXTBLOCK	; next region
   708                          
   709                          
   710                          ;
   711                          ; *** Get String - fetch next string with length > 0
   712                          ;
   713                          ; ( C-flag, STAT, STRDP -> STRDP, LEN, STAT, X, Y, Z-flag )
   714                          ;
   715                          ; If C=1 start from the beginning at SDS, otherwise
   716                          ; continue with position STRDP and string status STAT.
   717                          ; If the Z-Flag is set no string is available,
   718                          ; otherwise X/Y contains the address and LEN
   719                          ; the length of the string.
   720                          
   721  c631 905a               GETSA	BCC CHECKTYPE	; C=0 -> continue with string according to STAT
   722                          			; otherwise start with at SDS ...
   723                          
   724                          ; *** Look up String Descriptor Stack (SDS): TOSS to TSSP
   725                          ;
   726                          ;    +-------------+
   727                          ;    |             V
   728                          ;    |    belegt->|<-frei
   729                          ;   +-+     +-----+-----+-----+
   730                          ;   | |     |S|L|H|S|L|H|S|L|H|
   731                          ;   +-+     +-----+-----+-----+
   732                          ;    ^       ^     ^     ^     ^
   733                          ;    $16     $19   $1C   $1F   $22
   734                          ;    TSSP    TOSS
   735                          
   736                          DESCSTACK
   737  c633 a000               	LDY #0
   738  c635 8423               	STY STRDP+1	; Zero descriptor pointer high
   739  c637 a905               	LDA #STAT_SDS	; Set status to SDS
   740  c639 8557               	STA STAT
   741  c63b a219               	LDX #TOSS	; Start of SDS
   742  c63d d005               	BNE ISDSTEND	; branch always
   743  c63f a622               DSTACK	LDX STRDP
   744  c641 e8                 NEXTDST	INX		; next descriptor
   745  c642 e8                 	INX
   746  c643 e8                 	INX
   747                          ISDSTEND
   748  c644 e416               	CPX TSSP	; SDS finished?
   749  c646 f010               	BEQ VARS
   750  c648 b500               	LDA 0,X		; Check string length
   751  c64a f0f5               	BEQ NEXTDST
   752  c64c 8552               	STA LEN		; Return variables:
   753  c64e 8622               	STX STRDP	; length, descriptor address
   754  c650 b502               	LDA 2,X		; String address high
   755  c652 a8                 	TAY
   756  c653 b501               	LDA 1,X		; String address low
   757  c655 aa                 	TAX
   758  c656 98                 	TYA		; Always not zero, Z=0
   759  c657 60                 	RTS		; Returns address in X/Y
   760                          
   761                          ; *** Look up simple variables: VARTAB to ARYTAB
   762                          
   763  c658 a52d               VARS	LDA VARTAB	; Begin of variables
   764  c65a a62e               	LDX VARTAB+1
   765  c65c 8522               	STA STRDP
   766  c65e 8623               	STX STRDP+1
   767  c660 a003               	LDY #STAT_VAR	; Set status to variables
   768  c662 8457               	STY STAT
   769  c664 d00b               	BNE ISVAREND	; Branch always
   770                          VAR
   771  c666 18                 NEXTVAR	CLC		; Next variable
   772  c667 a522               	LDA STRDP
   773  c669 6907               	ADC #7		; Advance to next variable
   774  c66b 8522               	STA STRDP
   775  c66d 9002               	BCC ISVAREND
   776  c66f e623               	INC STRDP+1	; Overflow high byte
   777                          ISVAREND
   778  c671 c52f               	CMP ARYTAB
   779  c673 d006               	BNE CHECKVAR
   780  c675 a623               	LDX STRDP+1	; Variable end (=array start)?
   781  c677 e430               	CPX ARYTAB+1
   782  c679 f01d               	BEQ ARRAYS	; Variable end reached, proceed with arrays
   783                          CHECKVAR
   784  c67b a000               	LDY #0		; Variable name
   785  c67d b122               	LDA (STRDP),Y	; 1st character, type in bit 7 
   786  c67f 30e5               	BMI NEXTVAR	; No string, to next variable
   787  c681 c8                 	INY
   788  c682 b122               	LDA (STRDP),Y	; 2nd character, type in bit 7
   789  c684 10e0               	BPL NEXTVAR	; No string, to next variable
   790  c686 c8                 	INY
   791  c687 b122               	LDA (STRDP),Y	; String length
   792  c689 f0db               	BEQ NEXTVAR	; = 0, to next variable
   793  c68b d063               	BNE RETGETSA
   794                          
   795                          CHECKTYPE
   796  c68d a557               	LDA STAT	; GETSA intro with C=0
   797  c68f c903               	CMP #STAT_VAR	; String status?
   798  c691 9042               	BCC ARRAY	; =1 -> arrays
   799  c693 f0d1               	BEQ VAR		; =3 -> variables
   800  c695 4c3fc6             	JMP DSTACK	; =5 -> SDS
   801                          
   802                          ; *** Look up arrays: ARYTAB to STREND
   803                          
   804  c698 8550               ARRAYS	STA PTR		; A/X set from simple variable processing,
   805  c69a 8651               	STX PTR+1	; pointing the start of arrays.
   806  c69c a001               	LDY #STAT_ARY
   807  c69e 8457               	STY STAT	; Set status to arrays
   808                          ISARREND
   809  c6a0 a550               	LDA PTR
   810  c6a2 a651               	LDX PTR+1
   811  c6a4 e432               CHKAEND	CPX STREND+1	; End of array area?
   812  c6a6 d004                       BNE NEXTARR
   813  c6a8 c531               	CMP STREND	; High byte matches, low byte is
   814                          			; less or equal.
   815  c6aa f04f               	BEQ NOSTRING	; Arrays finished -> no string
   816                          NEXTARR
   817                          			; Carry always cleared because of CPX/CMP
   818  c6ac 8522               	STA STRDP	; Start of an array
   819  c6ae 8623               	STX STRDP+1
   820  c6b0 a000               	LDY #0
   821  c6b2 b122               	LDA (STRDP),Y	; Array name
   822  c6b4 aa                 	TAX		; Array type, keep it for later
   823  c6b5 c8                 	INY
   824  c6b6 b122               	LDA (STRDP),Y
   825  c6b8 08                 	PHP		; Array type 2nd part, keep also
   826  c6b9 c8                 	INY
   827  c6ba b122               	LDA (STRDP),Y	; Offset to next array
   828  c6bc 6550               	ADC PTR		; C-flag is cleared (because of CMP/CPX above)
   829  c6be 8550               	STA PTR		; Save start of following array
   830  c6c0 c8                 	INY
   831  c6c1 b122               	LDA (STRDP),Y
   832  c6c3 6551               	ADC PTR+1
   833  c6c5 8551               	STA PTR+1
   834  c6c7 28                 	PLP		; Fetch array type
   835  c6c8 10d6               	BPL ISARREND	; Not a string array
   836  c6ca 8a                 	TXA		; Fetch array type 2nd part
   837  c6cb 30d3               	BMI ISARREND	; Not string array
   838  c6cd c8                 	INY
   839  c6ce b122               	LDA (STRDP),Y	; Number of dimensions
   840  c6d0 0a                 	ASL		; *2
   841  c6d1 6905               	ADC #5		; Offset = dimensions*2+5
   842                          			; C=0 as long as dim.. <= 125
   843  c6d3 d003               	BNE ADVDESC	; Branch always
   844                          ARRAY			; Entry on continuation
   845                          NEXTASTR
   846  c6d5 18                 	CLC
   847  c6d6 a903               	LDA #3		; String descriptor length
   848  c6d8 6522               ADVDESC	ADC STRDP	; Advance to next string
   849  c6da 8522               	STA STRDP
   850  c6dc 9002               	BCC +
   851  c6de e623               	INC STRDP+1	; Overflow high byte
   852  c6e0 c550               +	CMP PTR		; All array elements processed?
   853  c6e2 d006               	BNE IS0ASTR
   854  c6e4 a623               	LDX STRDP+1
   855  c6e6 e451               	CPX PTR+1
   856  c6e8 f0ba               	BEQ CHKAEND	; A/X = PTR, check for end of  array area
   857                          IS0ASTR
   858  c6ea a000               	LDY #0
   859  c6ec b122               	LDA (STRDP),Y	; String length
   860  c6ee f0e5               	BEQ NEXTASTR	; Next array element
   861                          RETGETSA
   862  c6f0 8552               	STA LEN		; Return value: length
   863  c6f2 c8                 	INY
   864  c6f3 b122               	LDA (STRDP),Y	; String address low
   865  c6f5 aa                 	TAX
   866  c6f6 c8                 	INY
   867  c6f7 b122               	LDA (STRDP),Y	; String address high
   868  c6f9 a8                 	TAY		; Always not zero, Z=0
   869  c6fa 60                 	RTS		; Return address in X/Y
   870                          NOSTRING
   871  c6fb a900               	LDA #0		; Length 0 
   872  c6fd 8552               	STA LEN		; No string found
   873  c6ff 60                 	RTS		; Z=1
   874                          
   875                          ;
   876                          ; *** Correct string Address in descriptor
   877                          ;
   878                          ; ( NEWPTR, STRDP, STAT -> )
   879                          ;
   880  c700 a557               CORR	LDA STAT	; String status
   881  c702 2903               	AND #%011	; Just 2 bits, giving the
   882  c704 a8                 	TAY		; offset to the descriptor ...
   883  c705 a54e               	LDA NEWPTR	;
   884  c707 9122               	STA (STRDP),Y	; which differs for SDS
   885  c709 c8                 	INY		; and array elements!
   886  c70a a54f               	LDA NEWPTR+1
   887  c70c 9122               	STA (STRDP),Y
   888  c70e 60                 	RTS
   889                          
   890                          
   891                          ;
   892                          ; ***  MOVBLOCK routines (needed for IRQ hook method)
   893                          ;
   894                          
   895                          !ifndef basic_patch {
   896                          
   897                            !ifndef orig_movblock {
   898                          	; The optimized implementation:
   899                          	;
   900                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   901                          	; isn't available while switching off the KERNAL ROM, which
   902                          	; switches the BASIC ROM off also. In this case we have to redefine
   903                          	; this task with a separate routine which is shorter and slightly
   904                          	; faster than the original from the ROM.
   905                          
   906                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   907                          	; Overlapping works as long as the destination address is below the
   908                          	; source block.
   909                          	; Input: $5F/$60 source start address
   910                          	;	 $5A/$5B source end address+1
   911                          	;	 $58/$59 destination start address
   912                          	; Destroyed: A, X, Y
   913                          	; Output: $58/$59 has the value destination end address +1
   914                          	;          X = 0
   915                          	;	   Y = 0 (if the block length is greater 0)
   916                          	;	   Z-flag = 1
   917                          MOVBLOCK
   918                                  SEC
   919                                  LDA $5A       ; Source end address low
   920                                  SBC $5F       ; Minus source begin address low
   921                                  TAY           ; Block length low
   922                                  LDA $5B       ; Source end address high
   923                                  SBC $60       ; Minus source begin address high
   924                                  TAX           ; Block length high, usage as DEC-DEC counter
   925                                  TYA           ; Length low
   926                                  BEQ copy      ; Nothing more to do if 0
   927                                  CLC           ; Length in A
   928                                  ADC $5F       ; Source begin address corrected by 
   929                                  STA $5F       ; low-byte offset: -(-length) -> +length
   930                                  BCS +         ; Because this is like a subtraction
   931                                  DEC $60       ; C=0 means to correct begin address high.
   932                          +
   933                                  TYA           ; Length low
   934                                  CLC
   935                                  ADC $58       ; Destination begin address corrected by
   936                                  STA $58       ; low-byte offset: -(-length) -> +length
   937                                  BCS +         ; Because this is like a subtraction
   938                                  DEC $59       ; C=0 means to correct begin address high.
   939                          +
   940                          	INX           ; Page counter (all full and one partial)
   941                          	TYA           ; Length low
   942                          	EOR #$FF      ; Negate (two's complement):
   943                          	TAY           ; NOT(X)+1
   944                          	INY
   945                          copy    LDA ($5F),Y   ; Copy source 
   946                                  STA ($58),Y   ; to destination, with increasing addresses
   947                                  INY
   948                                  BNE copy
   949                                  INC $60       ; Source high
   950                                  INC $59       ; Destination high
   951                                  DEX           ; Block counter
   952                                  BNE copy
   953                                  RTS
   954                          	; Takes 6 bytes less compared to the original routine.
   955                          
   956                            } else {
   957                          
   958                          	; The original routine taken from the ROM:
   959                          	;
   960                          	; The "Open Space" routine from the BASIC ROM, $A3BF
   961                          	; isn't available while switching off the KERNAL ROM, which
   962                          	; switches the BASIC ROM off also. In this case we have to redefine
   963                          	; this task with a separate routine which is shorter and slightly
   964                          	; faster than the original from the ROM.
   965                          
   966                          	; Copy memory range($5F/$60) to excl. ($5A/$5B) to ($58/$59)
   967                          	; Overlapping works as long as the destination's end address is 
   968                          	; above the source block.
   969                          	; Input: $5F/$60 source start address
   970                          	;	 $5A/$5B source end address+1
   971                          	;	 $58/$59 destination end address+1
   972                          	; Destroyed: A, X, Y
   973                          	; Output: $58/$59 has the value destination end address +1
   974                          	;          X = 0
   975                          	;	   Y = 0 (if the block length is greater 0)
   976                          	;	   Z-flag = 1
   977                          
   978                          MOVBLOCK
   979                                  SEC
   980                                  LDA $5A       ; End address low
   981                                  SBC $5F       ; Minus begin address low
   982                                  STA $22       ; Length low
   983                                  TAY
   984                                  LDA $5B       ; End address high
   985                                  SBC $60       ; Minus begin address high
   986                                  TAX           ; Length high
   987                                  INX           ; Length as DEC-DEC counter
   988                                  TYA           ; Length low
   989                                  BEQ +         ; If not zero, then correct
   990                                  LDA $5A       ; the end address by low-byte offset
   991                                  SEC           ; from length.
   992                                  SBC $22       ; Length low
   993                                  STA $5A       ;
   994                                  BCS ++
   995                                  DEC $5B       ; Overflow high byte 
   996                                  SEC
   997                          ++      LDA $58       ; Correct the destination end address
   998                                  SBC $22       ; by low-byte offset (length low).
   999                                  STA $58
  1000                                  BCS +++	      ; Overflow high byte
  1001                                  DEC $59       ; 
  1002                                  BCC +++
  1003                          -       LDA ($5A),Y   ; Copy source to destination
  1004                                  STA ($58),Y   ; from higher to lower addresses.
  1005                          +++     DEY
  1006                                  BNE -
  1007                                  LDA ($5A),Y   ; Source
  1008                                  STA ($58),Y   ; Destination
  1009                          +       DEC $5B       ; Source high
  1010                                  DEC $59       ; Destination high
  1011                                  DEX           ; Block counter
  1012                                  BNE --
  1013                                  RTS
  1014                            }
  1015                          }
  1016                          
  1017                          
  1018                          !ifdef debug {
  1019                          !source "debug.asm"
  1020                          }
  1021                          
  1022                          !ifndef no_indicator {
  1023  c70f 00                 ORIGVID !byte 0		; Original character of marker position
  1024  c710 00                 ORIGCOL !byte 0		; Original color of marker position
  1025                          }
  1026                          !ifndef basic_patch {
  1027                          ORIGIRQ !byte 0,0	; Original IRQ vector
  1028                          }
  1029  c711 00                 SAVE	!byte 0		; Saved zero-page variables
  1030                          *=*+ZPLEN-1
  1031                          
  1032                          
