;<s-gc-8-dok.prg> ==1FD8==
.LI 1,4 ,0
; *************************
; *  GARBAGE  COLLECTION  *
; *   VON JOHANN KLASEK   *
; * 1985-12-27 VERS. 1.1  *
; *************************
;
; AUFRUF: SYS ...
; RAEUMT STRINGSPEICHER AUF
;
; ES WERDEN NUR JENE SPEICHERSTELLEN
; BENUETZT, DIE AUCH DIE NORMALE
; GC VERWENDET; ALLE ANDEREN
; WERDEN WIEDER RESTAURIERT.
;
.EQ STRPTR = $33  ; STRINGPOINTER
.EQ BUFPTR = $5F  ; BUFFERPOINTER
.EQ STRADR = $58  ; STRINGADRESSE
.EQ STRDP  = $22  ; STRINGDESCRIPTORADRESSE
.EQ MEMBEG = $31  ; SPEICHER-ANFANG
.EQ MEMEND = $37  ; SPEICHER-ENDE
.EQ BERANF = $FB  ; BEREICHSANFANG U. -ENDE
.EQ BEREND = $FD
.EQ FLAG   = $53  ; ENDE-FLAG
.EQ LEN    = $4E  ; STRINGLAENGE
.EQ STAT   = $4F  ; STRING-TYPE
.EQ PTR    = $50  ; ARRAY-POINTER
.BA $C500
         SEI               ; BETRIEBSSYS.-ROM
         LDA 1             ; WEGBLENDEN
         PHA               ; DAMIT RAM ZUGAENGLICH
         LDA #$35          ; WIRD
         STA 1             
         LDA MEMEND        ; STRINGPOINTER
         STA STRPTR        ; UND BEREICHANFANG
         LDX MEMEND+1      ; AUF SPEICHERENDE
         STX STRPTR+1      ; SETZEN
         STA BERANF        
         STX BERANF+1      
         LDA PTR           ; PTR-REGISTER RETTEN
         LDX PTR+1         
         STA SPTR          
         STX SPTR+1        
         LDY #3            
L10      LDA $FB,Y         ; ZERO-PAGE-REG.
         STA SPTR+2,Y      ; RETTEN
         DEY               
         BPL L10           
         INY               
         STY FLAG          ; FLAG=0
BEGIN1   LDX BERANF        ; *** HAUPTSCHLEIFE ***
         LDA BERANF+1      ; BEREICH
         STX BEREND        ; GENAU UM 8K
         STA BEREND+1      ; NACH UNTEN VERLEGEN
         SEC               
         SBC #$20          
         BCC L2            
         STA BERANF+1      
         CPX MEMBEG        ; SPEICHERANFANG
         SBC MEMBEG+1      ; ERREICHT?
         BCS L1            
L2       INC FLAG          ; JA, ENDEFLAG SETZEN
         LDA MEMBEG        ; BEREICHANFANG =
         LDX MEMBEG+1      ; SPEICHERANFANG
         STA BERANF        ; BEREICHANFANG = BE-
         STX BERANF+1      ; REICHENDE (SONDERFALL)
         CMP BEREND        ; => BEREICH IST 0 BYTE
         BNE L1            ; LANG => FERTIG
         CPX BEREND+1      
         BEQ ENDE          
L1       LDA #0            ; BUFFERPOINTER AUF
         STA BUFPTR        ; $10000 OD. DEZ.
         STA BUFPTR+1      ; 65536 SETZEN
         SEC               ; C=1: GETSA-SUBR INIT
BEGIN2   JSR GETSA         ; STRING-ADRESSE HOLEN
         LDA LEN           ; STRINGLAENGE = 0 ?
         BEQ WEITER        ; KEINEN STRING MEHR
         TYA               ; GEFUNDEN
         CPX BERANF        
         SBC BERANF+1      
         BCC L3            ; LIEGT STRINGADR.
         TYA               ; IM BEREICH ?
         CPX BEREND        
         SBC BEREND+1      
         BCS L3            ; NEIN, NAECHSTER STR.
         SEC               ; JA, BUFFERPOINTER UM
         LDA BUFPTR        ; STRINGLAENGE
         SBC LEN           ; VERSCHIEBEN
         STA BUFPTR        
         BCS L4            
         DEC BUFPTR+1      
L4       STY STRADR+1      ; STRINGADR. ABSPEICHER
         STX STRADR        
         LDX LEN           ; STRING IN DEN
         LDY #0            ; BUFFERBEREICH
LOOP1    LDA (STRADR),Y    ; UEBERTRAGEN
         STA (BUFPTR),Y    
         INY               
         DEX               
         BNE LOOP1         
         CLC               
         LDA BUFPTR        ; NEUE STRINGADRESSE
         ADC STRPTR        ; BERECHNEN:
         STA STRADR        ; STRADR=BUFPTR-(65536-
         LDA BUFPTR+1      ; STRPTR)
         ADC STRPTR+1      ; DA 16-BIT BERECHNUNG
         STA STRADR+1      ; => 65536 = 0 => SA=BP+SP
         JSR CORR          ; STRINGDESCRIPTOR
L3       CLC               ; KORRIGIEREN
         JMP BEGIN2        ; NAECHSTEN STRING
WEITER   LDA BUFPTR        ; BUFFER LEER ?
         ORA BUFPTR+1      
         BEQ L6            ; JA, DANN BRAUCHT
         CLC               ; NICHTS VERSCHOBEN
         LDA STRPTR        ; WERDEN
         TAX               
         ADC BUFPTR        ; VERSCHIEBE PARAMETER
         STA STRPTR        ; ERMITTELN
         LDA STRPTR+1      
         TAY               
         ADC BUFPTR+1      ; BUFPTR=QUELLBLOCKANFANG
         STA STRPTR+1      
         LDA #$00          ; QUELLBLOCKENDE+1
         STA $5A           ; (=65536)
         STA $5B           
         STX $58           ; ZIELBLOCKENDE+1
         STY $59           ; BASIC-INTERPRETER-
         JSR $A3BF         ; ROUTINE:BLOCKVERSCHIEBEN
L6       LDA FLAG          ; ENDEFLAG GESETZT?
         BNE ENDE          
         JMP BEGIN1        ; NAECHSTEN BEREICH BE-
ENDE     PLA               ; ARBEITEN
         STA 1             ; AUF BETRIEBSSYS.-ROM
         CLI               ; SCHALTEN
         LDA SPTR          ; PTR-REG. IN ZERO-PAGE
         LDX SPTR+1        ; WIEDERHERSTELLEN
         STA PTR           
         STX PTR+1         
         LDY #3            
L11      LDA SPTR+2,Y      ; ZERO-PAGE-REG. ZU-
         STA $FB,Y         ; RUECKSETZEN
         DEY               
         BPL L11           
         RTS               ; GC-ROUTINE ENDE
;
;
GETSA    BCC G1            ; C=0 => NAECHSTE STRADR
         LDA #0            ; HOLEN(ZUM NORMALEN
         STA STRDP+1       ; EINSPRUNG)
         LDX #$16          ; C=1:STATUS U. STR.-
         STX STRDP         ; DESCRIPTORPOINTER INIT
         LDA #3            ;
         STA STAT          ; STATUS=3:DESCR.-STACK
DSTCK    LDA STRDP         ; STRINGDESCRIPTOR VOM
         CLC               ; STRING DESCRIPTOR-
         ADC #3            ; STACK HOLEN
         STA STRDP         
         LDY #0            
         LDA (STRDP),Y     
         STA LEN           
         INY               
         LDA (STRDP),Y     
         TAX               
         INY               
         LDA (STRDP),Y     
         TAY               
         LDA $16           
         CMP STRDP         
         BEQ N1            ; STRINGDESCRIPORSTACK
         LDA LEN           ; LEER? => VARIABLEN
         BEQ DSTCK         
         RTS               
N1       LDA $2D           ; VAR-ANFANG
         LDX $2E           
         STA STRDP         
         STX STRDP+1       
         LDA STRDP         
         LDY #2            ; STATUS=2 => VARIABLE
         STY STAT          
         BNE V1            
VAR      CLC               
         LDA STRDP         
         ADC #7            ; VARIABLEN-LAENGE
         STA STRDP         
         BCC V1            
         INC STRDP+1       
V1       LDX STRDP+1       
         CPX $30           
         BNE G2            ; VAR-ENDE ERREICHT
         CMP $2F           ; VAR. ENDE ? JA, ARRAYS
         BEQ N2            ; ABSUCHEN
G2       LDY #0            
         LDA (STRDP),Y     ; VAR-NAME UNTERSUCHEN
         BMI VAR           ; KEINE STRINGVAR
         INY               
         LDA (STRDP),Y     
         BPL VAR           ; KEINE STRINGVARIABLE
         INY               
         LDA (STRDP),Y     ; STRINGLAENGE = 0 ?
         BEQ VAR           ; NAECHSTE VARIABLE
         STA LEN           
         INY               
         LDA (STRDP),Y     ; STRINGADR. HOLEN
         TAX               
         INY               
         LDA (STRDP),Y     
         TAY               
         RTS               
G1       LDA STAT          ; STATUS PRUEFEN
         CMP #2            ; UND ZUR STRING-TYP
         BCC ARRAY         ; VERZWEIGEN
         BEQ VAR           
         JMP DSTCK         
N2       STA PTR           ; POINTER AUF ARRAY INIT
         STX PTR+1         
         LDY #1            ; STATUS=1: ARRAY
         STY STAT          
NEXTARR  LDA PTR           ; ARRAY-ENDE ERREICHT ?
         LDX PTR+1         
         CPX $32           
         BNE A1            
         CMP $31           
         BEQ N3            
A1       STA STRDP         
         STX STRDP+1       
         LDY #0            
         LDA (STRDP),Y     ; ARRAY-NAME
         TAX               
         INY               
         LDA (STRDP),Y     
         PHP               
         INY               
         LDA (STRDP),Y     ; PTR AUF NAECHSTES
         ADC PTR           ; ARRAY LINKEN
         STA PTR           
         INY               
         LDA (STRDP),Y     
         ADC PTR+1         
         STA PTR+1         
         PLP               ; ARRAY-TYP TESTEN
         BPL NEXTARR       ; KEIN STRINGARRAY
         TXA               
         BMI NEXTARR       
         INY               
         LDA (STRDP),Y     ; ANZAHL DER DIMENSIONEN
         LDY #0            
         ASL               ; DIMENSIONENWERTE
         ADC #2            ; UEBERLESEN
         ADC STRDP         
         STA STRDP         
         BCC ARRAY         
         INC STRDP+1       
ARRAY    CLC               
         LDA STRDP         
         ADC #3            ; STRINGDESCRIPTORLAENGE
         STA STRDP         
         BCC A5            
         INC STRDP+1       
A5       LDX STRDP+1       
         CPX PTR+1         ; ARRAY FERTIG DURCH-
         BNE A6            ; SUCHT?
         CMP PTR           
         BEQ NEXTARR       
A6       LDY #0            
         LDA (STRDP),Y     
         BEQ ARRAY         ; LAENGE=0 => NACHSTES
         STA LEN           ; ARRAYELEMENT
         INY               
         LDA (STRDP),Y     ; STRINGADRESSE UEBER-
         TAX               ; GEBEN
         INY               
         LDA (STRDP),Y     
         TAY               
         RTS               
N3       LDA #0            ; KEINEN STRING MEHR
         STA LEN           ; GEFUNDEN
         RTS               
;
;
CORR     LDA STAT          ; STRINGADR. KORRIGIEREN
         ASL               ; C=0,MAL 2
         ADC #$FF          ; MINUS 1
         AND #3            ; NUR 2 BITS ALS INDEX
         TAY               ; (POSITIONSBERECHNUNG,
         LDA STRADR        ; DA LAGE D. STRINGADR.
         STA (STRDP),Y     ; BEI DSTACK, VAR U.
         INY               ; ARRAY VERSCHIEDEN
         LDA STRADR+1      ; IST)
         STA (STRDP),Y     
         RTS               
SPTR     .BY 0,0,0,0,0     ; 5-BYTES ZERO-PAGE
