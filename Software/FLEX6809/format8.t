 TTL 6809 FORMAT8 PROGRAM
 PAG
* DNEWDISK
*
* DISK FORMATTING PROGRAM FOR 6809 FLEX
* 8" format S/D side S/D density
*
* Copyright (c) 1984 by
* Technical Systems Consultants, Inc.
*
*
* THE NEWDISK PROGRAM INITIALIZES A NEW DISKETTE AND
* THEN PROCEEDS TO VERIFY ALL SECTORS AND INITIALIZE
* TABLES.  THIS VERSION IS SETUP FOR AN 8 INCH DISK.
*
        lib     flpinfo.h

* extern from monitor
cable equ $F7EE  PC or Straight cable

**************************************************
* DISK SIZE PARAMETERS
* **** **** **********
**************************************************
MAXTRK EQU 77 NUMBER OF TRACKS
* SINGLE DENSITY:
SMAXS0 EQU 15 SD MAX SIDE 0 SECTORS
SMAXS1 EQU 30 SD MAX SIDE 1 SECTORS
* DOUBLE DENSITY:
DMAXS0 EQU 26 DD MAX SIDE 0 SECTORS
DMAXS1 EQU 52 DD MAX SIDE 1 SECTORS
**************************************************
* MORE DISK SIZE DEPENDENT PARAMETERS
* **** **** **** ********* **********
* THE FOLLOWING VALUES ARE ALSO DEPENDENT ON THE
* SIZE OF DISK BEING FORMATTED.  EACH VALUE SHOWN
* IS FOR 8 INCH
**************************************************
* SIZE OF SINGLE DENSITY WORK BUFFER FOR ONE TRACK
TKSZ EQU 6250 
* TRACK START VALUE
TST EQU 40 
* SECTOR START VALUE
SST EQU 73 
* SECTOR GAP VALUE
GAP EQU 27 
**************************************************
* WORK SPACE WHERE ONE TRACK OF DATA IS SETUP
WORK EQU $1000 WORK SPACE
SWKEND EQU TKSZ+WORK SINGLE DENSITY
DWKEND EQU TKSZ*2+WORK DOUBLE DENSITY
* GENERAL EQUATES
FIRST EQU $0101 FIRST USER SECTOR
FCS EQU 30 FCB CURRENT SECTOR
FSB EQU 64 FCB SECTOR BUFFER
IRS EQU 16 INFO RECORD START
AVLP EQU FSB+IRS+13
DIRSEC EQU 5 FIRST DIR. SECTOR
RDSS EQU 9 READ SS FMS CODE
WTSS EQU 10 WRITE SS FMS CODE
DATE EQU $CC0E DOS DATE
* FLEX ROUTINES EQUATES
PSTRNG EQU $CD1E
PUTCHR EQU $CD18
OUTDEC EQU $CD39
GETHEX EQU $CD42
GETCHR EQU $CD15
PCRLF EQU $CD24
INBUF EQU $CD1B
GETFIL EQU $CD2D
INDEC EQU $CD48
FMS EQU $D406
FMSCLS EQU $D403
OUT2HS EQU $CD3C
WARMS EQU $CD03
* DISK DRIVER ROUTINES
DWRITE EQU $DE03 WRITE A SINGLE SECTOR
REST EQU $DE09 RESTORE HEAD
DSEEK EQU $DE1B SEEK TO TRACK
DCKRDY EQU $DE0F
* TEMPORARY STORAGE
 ORG $0020
TRACK RMB 1
SECTOR RMB 1
BADCNT RMB 1 BAD SECTOR COUNT
DRN RMB 1
SIDE RMB 1
DBSDF RMB 1
DENSE RMB 1
DNSITY RMB 1
INDEX RMB 2
SECCNT RMB 2 SECTOR COUNTER
FSTAVL RMB 2 FIRST AVAILABLE
LSTAVL RMB 2 LAST AVAILABLE
MAXS0 RMB 1  MAX SIDE 0 SECTOR
MAXS1 RMB 1 MAX SIDE 1 SECTOR
MAX RMB 1 MAX SECTOR
FKFCB RMB 4
VOLNAM RMB 11
VOLNUM RMB 2
 PAG
 ORG $0100
********************************************
* MAIN PROGRAM STARTS HERE
********************************************
NEWDISK BRA FORM0
VN FCB 8 VERSION NUMBER
FORM0
 bra FORM1
OUTIN JSR PSTRNG OUTPUT STRING
OUTIN2 JSR GETCHR GET RESPONSE
 ANDA #$5F MAKE IT UPPER CASE
 CMPA #'Y SEE IF "YES"
 RTS
FORM1 LDA #SMAXS0 INITIALIZE SECTOR MAX
 STA MAXS0
 STA MAX
 LDA #SMAXS1
 STA MAXS1
 JSR GETHEX GET DRIVE NUMBER
 LBCS EXIT
 TFR X,D
 lda cable
 cmpa #1 straight cable?
 beq tst3
 CMPB #1 ENSURE 0 TO 1 PC cable
 LBHI EXIT
 bra tstdone
tst3 cmpb #3 ENSURE 0 TO 3 Straight cable
 lbhi EXIT
tstdone LDX #WORK
 STB 3,X
 STB DRN
 LDX #SURES ASK IF HE'S SURE
 BSR OUTIN PRINT & GET RESPONSE
 LBNE EXIT EXIT IF "NO"
 LDX #SCRDS CHECK SCRATCH DRIVE NO.
 JSR PSTRNG OUTPUT IT
 LDX #WORK+2
 CLR 0,X
 CLRB
 JSR OUTDEC
 LDA #'? PRINT QUESTION MARK
 JSR PUTCHR
 LDA #$20
 JSR PUTCHR
 BSR OUTIN2 GET RESPONSE
 BNE EXIT EXIT IF "NO"
*
 CLR DBSDF CLEAR FLAG
 CLR SIDE
*** PLACE A "BRA FORM25" HERE IF CONTROLLER
*** IS ONLY SINGLE SIDED.
 LDX #DBST ASK IF DOUBLE SIDED
 BSR OUTIN PRINT & GET RESPONSE
 BNE FORM25 SKIP IF "NO"
 INC DBSDF SET FLAG
 LDA #SMAXS1 SET MAX SECTOR
 STA MAX
*
FORM25 CLR DENSE INITIALIZE SINGLE DENSITY
 CLR DNSITY
 CLR DENSE
*** PLACE A "BRA FORM26" HERE IF CONTROLLER
*** IS ONLY SINGLE DENSITY.
 LDX #DDSTR ask if double density
 bsr OUTIN
 bne FORM26
 inc DENSE
*
FORM26 LDX #NMSTR ASK FOR VOLUME NAME
 JSR PSTRNG PRINT IT
 JSR INBUF GET LINE
 LDX #FKFCB POINT TO FAKE
 JSR GETFIL
FORM27 LDX #NUMSTR OUTPUT STRING
 JSR PSTRNG
 JSR INBUF GET LINE
 JSR INDEC GET NUMBER
 BCS FORM27 ERROR?
 STX VOLNUM SAVE NUMBER
 JSR PCRLF PRINT CR & LF
*
 LDX #WORK
 jsr DCKRDY
 bne EXIT
*
 LDX #WORK
 JSR REST RESTORE DISK
 bitb #%00000100  track zero
 BNE FORMAT SKIP IF NO ERROR
*
 LDX #WPST
 BITB #$40 WRITE PROTECT ERROR?
 BNE EXIT2 SKIP IF SO
* EXIT ROUTINES
EXIT LDX #ABORTS REPORT ABORTING
EXIT2 JSR PSTRNG OUTPUT IT
EXIT3 JSR FMSCLS
 JMP WARMS RETURN
 PAG
**************************************************************
*
* ACTUAL FORMAT ROUTINE
*
* THIS CODE PERFORMS THE ACTUAL DISK FORMATTING BY PUTTING
* ON ALL GAPS, HEADER INFORMATION, DATA AREAS, SECTOR LINKING,
* ETC.  THIS SECTION DOES NOT WORRY ABOUT SETTING UP THE
* SYSTEM INFORMATION RECORD, BOOT SECTOR, OR DIRECTORY.
* IT ALSO DOES NOT NEED BE CONCERNED WITH TESTING THE DISK FOR
* ERRORS AND THE REMOVAL OF DEFECTIVE SECTORS ASSOCIATED WITH
* SUCH TESTING.  THESE OPERATIONS ARE CARRIED OUT BY THE
* REMAINDER OF THE CODE IN "NEWDISK".
* IF USING A WD1771 OR WD1791 CONTROLLER CHIP, THIS CODE SHOULD
* NOT NEED CHANGING (SO LONG AS THE WRITE TRACK ROUTINE AS
* FOUND LATER IS PROVIDED).  IF USING A DIFFERENT TYPE OF
* CONTROLLER, THIS CODE MUST BE REPLACED AND THE WRITE TRACK
* ROUTINE (FOUND LATER) MAY BE REMOVED AS IT WILL HAVE TO BE
* A PART OF THE CODE THAT REPLACES THIS FORMATTING CODE.
* WHEN THIS ROUTINE IS COMPLETED, IT SHOULD JUMP TO 'SETUP'.
*
**************************************************************
* MAIN FORMATTING LOOP
FORMAT CLR TRACK
FORM3 CLR SIDE SET SIDE 0
 CLR SECTOR
 BSR TRKHD SETUP TRACK HEADER
FORM32 LDX #WORK+SST POINT TO SECTOR START
 TST DNSITY DOUBLE DENSITY?
 BEQ FORM4 SKIP IF NOT
 LDX #SST*2+WORK DD SECTOR START
FORM4 LBSR DOSEC PROCESS RAM WITH INFO
 INC SECTOR ADVANCE TO NEXT
 LDA SECTOR CHECK VALUE
 TST SIDE CHECK SIDE
 BNE FORM45
 CMPA MAXS0
 BRA FORM46
FORM45 CMPA MAXS1
FORM46 BNE FORM4 REPEAT?
FORM47 lda TRACK GET TRACK NUMBER
       ldb SECTOR
       JSR DSEEK SEEK TRACK AND SIDE
 JSR WRTTRK WRITE TRACK
FORM5 TST DBSDF ONE SIDE?
 BEQ FORM6
 TST SIDE
 BNE FORM6
 COM SIDE SET SIDE 1
 BRA FORM32
FORM6 INC TRACK BUMP TRACK
 JSR SWITCH SWITCH TO DD IF NCSSRY
FORM7 LDA TRACK CHECK VALUE
 CMPA #MAXTRK
 BNE FORM3
 LBRA SETUP DONE...GO FINISH UP
* SETUP TRACK HEADER INFORMATION
TRKHD LDX #WORK POINT TO BUFFER
 TST DNSITY DOUBLE DENSITY?
 BNE TRHDD SKIP IF SO
 LDB #$FF
TRHDS1 STB 0,X+ INITIALIZE TO $FF
 CMPX #SWKEND
 BNE TRHDS1
 LDX #WORK+TST
 CLRA SET IN ZEROS
 LDB #6
 BRA TRHDD2
TRHDD LDB #$4E
TRHDD1 STB 0,X+ INITIALIZE TO $4E
 CMPX #DWKEND
 BNE TRHDD1
 LDX #TST*2+WORK
 CLRA SET IN ZEROS
 LDB #12
 BSR SET
 LDA #$F6 SET IN $F6'S
 LDB #3
TRHDD2 BSR SET
 LDA #$FC SET INDEX MARK
 STA 0,X
 RTS
* SET (B) BYTES OF MEMORY TO (A) STARTING AT (X)
SET STA 0,X+
 DECB
 BNE SET
 RTS
* PROCESS SECTOR IN RAM
DOSEC CLRA
 TST DNSITY DOUBLE DENSITY?
 BNE DOSEC1 SKIP IF SO
 LDB #6 CLEAR 6 BYTES
 BRA DOSEC2
DOSEC1 LDB #12 CLEAR 12 BYTES
 BSR SET
 LDA #$F5 SET IN 3 $F5'S
 LDB #3
DOSEC2 BSR SET
 LDA #$FE ID ADDRESS MARK
 STA 0,X+
 LDA TRACK GET TRACK NO.
 STA 0,X+
 LDB DNSITY DOUBLE DENSITY?
 BEQ DOSEC3 SKIP IF NOT
 LDB SIDE GET SIDE INDICATOR
 ANDB #$01 MAKE IT 0 OR 1
DOSEC3 STB 0,X+
 LDB SECTOR GET SECTOR NO.
 LDY #SSCMAP POINT TO CORRECT MAP
 TST DNSITY
 BEQ DOSEC4
 LDY #DSCMAP
DOSEC4 LDB B,Y GET ACTUAL SECTOR
 STB 0,X+
 CMPB MAX END OF TRACK?
DOSEC6 BNE DOSEC7 SKIP IF NOT
 INCA BUMP TRACK NO.
 CLRB RESET SECTOR NO.
 CMPA #MAXTRK END OF DISK?
 BNE DOSEC7 SKIP IF NOT
 CLRA SET ZERO FORWARK LINK
 LDB #-1
DOSEC7 INCB BUMP SECTOR NO.
 PSHS D SAVE FORWARD LINK
 LDA #1 SECTOR LENGTH = 256
 STA 0,X+
 LDA #$F7 SET CRC CODE
 STA 0,X+
 TST DNSITY DOUBLE DENSITY?
 BNE DOSEC8 SKIP IF SO
 LEAX 11,X LEAVE $FF'S
 CLRA PUT IN 6 ZEROS
 LDB #6
 BRA DOSEC9
DOSEC8 LEAX 22,X LEAVE $4E'S
 CLRA PUT IN 12 ZEROS
 LDB #12
 BSR SET
 LDA #$F5 PUT IN 3 $F5'S
 LDB #3
DOSEC9 BSR SET
 LDA #$FB DATA ADDRESS MARK
 STA 0,X+
 PULS D RESTORE FORWARD LINK
 STD 0,X++ PUT IN SECTOR BUFFER
 CLRA CLEAR SECTOR BUFFER
 LDB #254
 BSR SET
 LDA #$F7 SET CRC CODE
 STA 0,X+
 LEAX GAP,X LEAVE GAP
 TST DNSITY DOUBLE DENSITY?
 BEQ DOSECA SKIP IF NOT
 LEAX GAP,X DD NEEDS MORE GAP
DOSECA RTS
 PAG
**************************************************************
*
* DISK TESTING AND TABLE SETUP
*
* THE FOLLOWING CODE TESTS EVERY SECTOR AND REMOVES ANY
* DEFECTIVE SECTORS FROM THE FREE CHAIN.  NEXT THE SYSTEM
* INFORMATION RECORD IS SETUP, THE DIRECTORY IS INITIALIZED,
* AND THE BOOT IS SAVED ON TRACK ZERO.  ALL THIS CODE SHOULD
* WORK AS IS FOR ANY FLOPPY DISK SYSTEM.  ONE CHANGE THAT
* MIGHT BE REQUIRED WOULD BE IN THE SAVING OF THE BOOTSTRAP
* LOADER.  SPECIAL BOOT LOADERS MIGHT REQUIRE CHANGES IN THE
* WAY THE BOOT SAVE IS PERFORMED.  FOR EXAMPLE, IT MAY BE
* NECESSARY TO SAVE TWO SECTORS IF THE BOOT LOADER DOES NOT
* FIT IN ONE.  ALSO IT MAY BE NECESSARY, BY SOME MEANS, TO
* INFORM THE BOOT LOADER WHETHER THE DISK IS SINGLE OR
* DOUBLE DENSITY SO THAT IT MAY SELECT THE PROPER DENSITY
* FOR LOADING FLEX.
*
**************************************************************
* READ ALL SECTORS FOR ERRORS
SETUP LDB MAX GET MAX SECTORS
 LDA #MAXTRK-1
 STD LSTAVL SET LAST AVAIL.
 MUL FIND TOTAL SECTORS
 STD SECCNT SAVE IT
 LDX #FIRST SET FIRST AVAIL
 STX FSTAVL
 LDA DRN
 STA WORK+3
 CLRA  CLEAR COUNTER
 STA BADCNT
 STA TRACK SET TRACK
 STA DNSITY SNGL DNST FOR TRK 0
 INCA
 STA SECTOR SET SECTOR
 LDA #SMAXS0 RESET MAXIMUM
 STA MAXS0 SECTOR COUNTS
 LDA #SMAXS1
 STA MAXS1
 TST DBSDF DOUBLE SIDED?
 BNE SETUP1 SKIP IF SO
 LDA #SMAXS0
SETUP1 STA MAX SET MAXIMUM SECTORS
SETUP2 BSR CHKSEC GO CHECK SECTOR
 BNE REMSEC ERROR?
 CLR BADCNT CLEAR COUNTER
SETUP4 LDD TRACK GET TRACK & SECTOR
 BSR FIXSEC GET TO NEXT ADR
 LBEQ DOTRK SKIP IF FINISHED
 STD TRACK SET TRACK & SECTOR
 BRA SETUP2 REPEAT
* CHECK IF SECTOR GOOD
CHKSEC LDX #WORK POINT TO FCB
 LDD TRACK GET TRACK & SECTOR
 STD FCS,X SET CURRENT TRK & SCT
 JMP READSS GO DO READ
* SWITCH TO DOUBLE DENSITY IF NECESSARY
SWITCH LDB DENSE DOUBLE DENSITY DISK?
 BEQ SWTCH2 SKIP IF NOT
 STB DNSITY SET FLAG
 LDB #DMAXS0 RESET SECTOR COUNTS
 STB MAXS0
 LDB #DMAXS1
 STB MAXS1
 TST DBSDF DOUBLE SIDED?
 BNE SWTCH1 SKIP IF SO
 LDB #DMAXS0
SWTCH1 STB MAX SET MAX SECTOR
SWTCH2 RTS
* SET TRK & SEC TO NEXT
FIXSEC CMPB MAX END OF TRACK?
 BNE FIXSE4 SKIP IF NOT
 INCA  BUMP TRACK
 BSR SWITCH SWITCH TO DD IF NCSSRY
 CLRB RESET SECTOR NO.
FIXSE4 INCB BUMP SECTOR NO.
 CMPA #MAXTRK END OF DISK?
 RTS
* REMOVE BAD SECTOR FROM FREE SECTOR CHAIN
REMSEC INC BADCNT UPDATE COUNTER
 BEQ REMSE1 COUNT OVERFLOW?
 LDA TRACK GET TRACK
 BNE REMSE2 TRACK 0?
 LDB SECTOR GET SECTOR
 CMPB #DIRSEC PAST DIRECTORY?
 BHI REMSE2
REMSE1 LDX #FATERS REPORT FATAL ERROR
 JMP EXIT2 REPORT IT
REMSE2 LDX #WORK POINT TO FCB
 LDD FSTAVL GET 1ST TRACK & SECTOR
 CMPD TRACK CHECK TRACK & SECTOR
 BNE REMSE3
 BSR FIXSEC SET TO NEXT
 STD FSTAVL SET NEW ADR
 BRA REMSE8 GO DO NEXT
REMSE3 LDD TRACK GET TRACK & SECTOR
 SUBB BADCNT
 BEQ REMS35 UNDERFLOW?
 BPL REMSE4
REMS35 DECA  DEC TRACK
 LDB MAX RESET SECTOR
REMSE4 STD FCS,X SET CURRENT ADR
 BSR READSS GO DO READ
 BNE REMSE1 ERROR?
 LDD FSB,X GET LINK ADR
 BSR FIXSEC POINT TO NEXT
 BNE REMSE6 OVERFLOW?
 LDD FCS,X GET CURRENT ADR
 STD LSTAVL SET NEW LAST AVAIL
 CLRA  SET END LINK
 CLRB
REMSE6 STD FSB,X SET NEW LINK
 BSR WRITSS GO DO WRITE
 BNE REMSE1 ERROR?
REMSE8 LDX SECCNT GET SEC COUNT
 LEAX -1,X DEC IT ONCE
 STX SECCNT SAVE NEW COUNT
 LDX #BADSS REPORT BAD SECTOR
 JSR PSTRNG OUTPUT IT
 LDX #TRACK POINT TO ADDRESS
 JSR OUT2HS OUTPUT IT
 LDA #$20
 JSR PUTCHR
 LEAX 1,X BUMP TO NEXT
 JSR OUT2HS
 JMP SETUP4 CONTINUE
* READ A SECTOR
READSS LDX #WORK POINT TO FCB
 LDA #RDSS SET UP COMMAND
 STA 0,X
 JMP FMS GO DO IT
* WRITE A SECTOR
WRITSS LDX #WORK POINT TO FCB
 LDA #WTSS SETUP COMMAND
 STA 0,X
 JSR FMS GO DO IT
 BEQ READSS ERRORS?
 RTS  ERROR RETURN
* SETUP SYSTEM INFORMATION RECORD
DOTRK CLR DNSITY BACK TO SINGLE DENSITY
 LDX #WORK POINT TO SPACE
 CLR FCS,X SET TO DIS
 LDA #3 SECTOR 3
 STA FCS+1,X
 BSR READSS READ IN SIR SECTOR
 BNE DOTRK4 ERROR?
 LDX #WORK FIX POINTER
 CLR FSB,X CLEAR FORWARD LINK
 CLR FSB+1,X
 LDD FSTAVL ADDR. OF 1ST FREE SCTR.
 STD AVLP,X SET IN SIR
 LDD LSTAVL ADDR. OF LAST FREE SCTR.
 STD AVLP+2,X PUT IN SIR
 LDD SECCNT GET TOTAL SECTOR COUNT
 STD AVLP+4,X PUT IN SIR
 LDA #MAXTRK-1 GET MAX TRACK NO.
 LDB MAXS0 SET MAX SECTORS/TRACK
 TST DBSDF DOUBLE SIDE?
 BEQ DOTRK2
 LDB MAXS1 CHANGE FOR DOUBLE SIDED
DOTRK2 STD AVLP+9,X SAVE MAX TRACK & SECTOR
 LDD DATE SET DATE INTO SIR
 STD AVLP+6,X
 LDA DATE+2
 STA AVLP+8,X
 LDB #13
 LDY #VOLNAM POINT TO VOLUME NAME
 LDX #WORK
DOTR33 LDA 0,Y+ COPY NAME TO SIR
 STA FSB+IRS,X
 LEAX 1,X
 DECB DEC THE COUNT
 BNE DOTR33
 BSR WRITSS WRITE SIR BACK OUT
 BEQ DIRINT SKIP IF NO ERROR
DOTRK4 JMP REMSE1 GO REPORT ERROR
* INITIALIZE DIRECTORY
DIRINT LDX #WORK SET POINTER
 LDA #SMAXS0 GET MAX FOR TRK 0
 TST DBSDF SINGLE SIDE?
 BEQ DIRIN1 SKIP IF SO
 LDA #SMAXS1 SET MAX FOR DS
DIRIN1 STA FCS+1,X SET UP
 JSR READSS READ IN SECTOR
 BNE DOTRK4 ERROR?
 LDX #WORK RESTORE POINTER
 CLR FSB,X CLEAR LINK
 CLR FSB+1,X
 JSR WRITSS WRITE BACK OUT
 BNE DOTRK4 ERRORS?
* SAVE BOOT ON TRACK 0 SECTOR 1
* (MAY REQUIRE CHANGES - SEE TEXT ABOVE)
DOBOOT lda #%01000001  drive select 0
 TST DENSE TEST DENSITY
 BNE DBOOT1 SKIP IF SINGLE
 ora #%00100000  set single density
DBOOT1 ldb cable get cable type
 cmpb #1
 beq DOIT auto boot loader
 cmpb #5
 beq DOIT auto boot loader
 ora #%00000101  drive select 1
DOIT sta densf  SET DENSITY IN BOOT
 lda #SMAXS0
 sta sectrk       set boot
 lda MAXS0
 sta sectdd
 LDX #BOOT POINT TO LOADER CODE
 CLRA TRACK #0
 LDB #1 SECTOR #1
 JSR DWRITE WRITE THE SECTOR
 BNE DOTRK4 SKIP IF AN ERROR
 LDX #BOOT+$100 POINT TO second sector
 CLRA TRACK #0
 LDB #2 SECTOR #2
 JSR DWRITE WRITE THE SECTOR
 BNE DOTRK4 SKIP IF AN ERROR

* REPORT TOTAL SECTORS AND EXIT
 LDX #WORK SETUP AN FCB
 LDA #16 OPEN SIR FUNCTION
 STA 0,X
 JSR FMS OPEN THE SIR
 BNE DOTRK4
 LDA #7 GET INFO RECORD FUNCTION
 STA 0,X
 JSR FMS GET 1ST INFO RECORD
 LBNE DOTRK4
 LDX #CMPLTE REPORT FORMATTING COMPLETE
 JSR PSTRNG
 LDX #SECST PRINT TOTAL SECTORS STRING
 JSR PSTRNG
 LDX #WORK+21 TOTAL IS IN INFO RECORD
 CLRB
 JSR OUTDEC PRINT NUMBER
 JMP EXIT3 ALL FINISHED!
**************************************************
* SECTOR MAPS
* ****** ****
* THE MAPS SHOWN BELOW CONTAIN THE CORRECT
* INTERLEAVING FOR AN 8 INCH DISK. 
**************************************************
SSCMAP
 FCB 1,6,11,3,8,13,5,10
 FCB 25,2,7,12,4,9,14
 FCB 16,21,26,18,23,28,20,25
 FCB 30,17,22,27,19,24,29

DSCMAP
 FCB 1,14,3,16,5,18,7
 FCB 20,9,22,11,24,13
 FCB 26,2,15,4,17,6,19
 FCB 8,21,10,23,12,25
 FCB 27,40,29,42,31,44,33
 FCB 46,35,48,37,50,39
 FCB 52,28,41,30,43,32,45
 FCB 34,47,36,49,38,51

* STRINGS
SURES FCC 'ARE YOU SURE? '
 FCB 4
WPST FCC 'DISK IS PROTECTED!'
 FCB 4
SCRDS FCC '8" SCRATCH DISK IN DRIVE '
 FCB 4
FATERS FCC 'FATAL ERROR --- '
ABORTS FCC 'FORMATTING ABORTED'
 FCB 4
BADSS FCC 'BAD SECTOR AT '
 FCB 4
CMPLTE FCC 'FORMATTING COMPLETE'
 FCB 4
SECST FCC 'TOTAL SECTORS = '
 FCB 4
DBST FCC 'DOUBLE SIDED DISK? '
 FCB 4
DDSTR FCC 'DOUBLE DENSITY DISK? '
 FCB 4
NMSTR FCC 'VOLUME NAME? '
 FCB 4
NUMSTR FCC 'VOLUME NUMBER? '
 FCB 4
 PAG
***************************************************************
* WRITE TRACK ROUTINE                                         *
***************************************************************
WD2791  set     0
***************************************************************
WRTTRK  pshs    dp,d,x,u
        lda     #fdbasp
        setdp   fdbasp
        tfr     a,dp
        lda     #%00000000      set 8"
        tst     DNSITY          actual density
        bne     10f
        ora     #%00100000      set single
10      tst     SIDE
        beq     11f
        ora     #%00010000      side 1

11      ldb     cable
        cmpb    #1           straight cable?
        bls     TAB4
        ldx     #DRBTPC      No
        bra     TABPC
TAB4    ldx     #DRBTAB      Yes
TABPC   ldb     DRN          drive number
        abx
        ora     0,x
        sta     <fo4lat
*
        ldx     #0
12      ldb     fo2cmd
        bpl     14f
        leax    -1,x
        bne     12b
        ldb     #$80    NR
        bra     16f
*
14      ldu     #WORK
        lda     #FD_WTR
        if      (WD2791=1)
        coma
        endif
        sta     <fo2cmd
*
01      orcc    #$50
        bra     03f
*
02      lda     0,u+
        if      (WD2791=1)
        coma
        endif
        sta     <fo2dat
*
03      lda     <fo4sta
        bmi     02b
        beq     03b
*
        ldb     <fo2cmd
        if      (WD2791=1)
        comb
        endif
16      stb     2,s     set B
*
        puls    dp,d,x,u,pc


 rts

DRBTPC  fcb     %00000101,%00001010
DRBTAB  fcb     %00000001,%00000010
        fcb     %00000100,%00001000
**************************************************
 PAG
**********************************************************
*
* BOOTSTRAP FLEX LOADER
*
*
**********************************************************
BOOT    equ     *
*
* bootsector code for FLEX on a CPUXXCMI system
*
*

*
* floppy hardware definitions
*
fdccmd  equ     $f100
fdctrk  equ     $f101
fdcsec  equ     $f102
fdcdat  equ     $f103
fdcsel  equ     $f104
fdcsta  equ     $f108
*
*bstack  equ     $c440
* sbug routines
pdata   equ    $f812
monitr  equ    $f800


*       org    $c100

boots   lda    #fdccmd/256
        setdp  fdccmd/256
        bra    boot2
* X,5/8,DD/SD,S1/S0,ds,ds,ds,ds
densf   fcb    %00100001        drive0 select 8"/SD/S0
oslink  fdb    0                FLEX.SYS link
        fcb    0
* 5 inch settings
sectrk  fcb    15               8" SD
sectdd  fcb    26               8" DD
retry   fcb    12
bufptr  fdb    0
*
boot2   tfr    a,dp
        leas   bstack,pcr
        lda    <densf,pcr
* nops1   ora    cable
        bita   #%00100000       dens bit
        bne    boot3
        lda    <sectdd,pcr
        sta    <sectrk,pcr
*
boot3   lda    #2               read 2nd sector
        sta    <fdcsec
        leax   boots+$100,pcr   where to put
        lbsr   readsc
        bne    fatal1
*
* here the 2nd bootsector has been loaded
*
        ldd    <oslink,pcr      go load OS
        lbeq   ntlink
*
        leau   bufend,pcr       init bufptr test
        stu    <bufptr,pcr      set end of buffer address
************************************************************
*
* load OS file, process segment headers
*
************************************************************
loadf   lbsr   rfilbt
        cmpa   #2               binary segment
        beq    rdbseg
        cmpa   #$16             tranfer segemnt
        bne    loadf
*
        lbsr   rfilbt
        sta    ,--s             build transfer address
        lbsr   rfilbt
        sta    1,s
        clra
        tfr    a,dp             reset DP
        rts

************************************************************
*
* read in binary segment
*
************************************************************
rdbseg  lbsr   rfilbt
        sta    ,--s             build load address
        lbsr   rfilbt
        sta    1,s
        puls   y                memory pointer
        lbsr   rfilbt           byte count
        beq    loadf
        tfr    a,b
* read in bytes into memory
rdbsg1  pshs   b
        lbsr   rfilbt
        sta    ,y+
        puls   b
        decb
        bne    rdbsg1
        bra    loadf

************************************************************
fatal1  leax   <s2err,pcr
fatal2  jsr    [pdata]
        jmp    [monitr]
s2err   fcc    "- Can't read sec.2",$0a,$0a,4

************************************************************
*
* read one sector into buffer
*
************************************************************
readsc  lda    #%10001100
        sta    <fdccmd
        leax   buffr1,pcr       set buffer pointer
        bra    rddata
*
rdbyt   lda    <fdcdat
        sta    0,x+
rddata  lda    <fdcsta
        bmi    rdbyt
        beq    rddata
        ldb    <fdccmd
        bitb   #%10011100
        rts

************************************************************
*
* seek, find track/secor/side
*
************************************************************
seek    stb   <fdcsec
        cmpb  sectrk,pcr
        pshs  cc
        ldb   densf,pcr
* nops2   orb   cable
        puls  cc
        bls   sksd0
        orb   #%00010000        side 1
sksd0   stb   <fdcsel           set latch
*
        cmpa  <fdctrk
        beq   tstbs1
        sta   <fdcdat
        lda   #%00011011        seek
        sta   <fdccmd
*
tstbsy  ldb   <fdcsta
        beq   tstbsy
tstbs1  ldb   <fdccmd
        bitb   #%10011000
        rts

************************************************************
*
*
*
************************************************************
rdlink  leau  buffr1,pcr
        ldd   ,u
        std   oslink,pcr
*
        leau  4,u            skip over segment header

************************************************************
*
*
*
************************************************************
rfilbt  cmpu  bufptr,pcr
        beq   rdnext
        lda   ,u+
        rts

************************************************************
*
*
*
************************************************************
rdnext  ldd   oslink,pcr     track/sector
        beq   badnuc
        bsr   seek
        lbsr  readsc
        beq   rdlink

************************************************************
*
*
*
************************************************************
        lda   #%00001011      restore
        sta   <fdccmd
        bsr   tstbsy
        dec   retry,pcr
        bne   rdnext

************************************************************
*
        leax  <ncerr,pcr
        lbra  fatal2
*
badnuc  leax  <bnerr,pcr
        lbra  fatal2
*
ntlink  leax  <nlerr,pcr
        lbra  fatal2
*
************************************************************
nlerr   fcc   "- Not Linked",$0a,$0a,4
ncerr   fcc   "- Nucleus I/O Error",$0a,$0a,4
bnerr   fcc   "- Bad Nucleus",$0a,$0a,4
************************************************************

        rzb   (boots+512)-*
buffr1  rmb     256
bufend  equ     *
bstack  equ     *+64

 END NEWDISK
                                                                                                                                                                                                                                                         