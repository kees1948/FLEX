
 pag
 sttl console driver
*
* FLEX console drivers
*
* ROM montor entry points
*
m_inch        equ     $F804     no echo
m_inche       equ     $F806     with ech0
m_outch       equ     $F80A
m_tstat       equ     $F808
m_start       equ     $F800
cable         equ     $F7EE

        abs

        org     $d370
*
* area for console drivers
*
*
d_inch   jmp     [m_inch]
d_ince   jmp     [m_inche]
do_outch jmp     [m_outch]
do_tstat jmp     [m_tstat]
do_warm  jmp     [m_start]
do_tinit jmp     do_rts
*
do_rti   rti
do_rts   rts
dumvec   fdb     0

*
* console driver entry points here
*
        org     $d3e5
*
        fdb     d_inch         input char no echo
        fdb     do_rti          dummy rti handler
        fdb     dumvec          dummy vector
        fdb     dumvec          dummy vector
        fdb     do_rts          dummy routine
        fdb     do_rts          dummy routine
        fdb     do_rts          dummy routine
        fdb     do_warm         monitor re-entry
        fdb     do_tinit        terminal init
        fdb     do_tstat        terminal status
        fdb     do_outch        output character
        fdb     d_ince         input char with echo
*

 pag
   sttl diskdriver cmi_io4beta
*  Disk Driver Package
*
*  Copyright (C) 1981,84 by
*  Technical Systems Consultants, Inc.
*  111 Providence Rd, Chapel Hill, NC
*
* adapted for FLEX by C. Schoenmakers
* CPU09XXX systeem
*
* Modified for auto cable select by C.A. De Jong
* needs cmi_sbug version 1.3
* works with PC-cable and Straigth-cable.
*
* Added step rate and double step for each drive.
*
        lib     flpinfo.h

sngsid  equ     %00000000
dblsid  equ     %00000001
sngdns  equ     %00000000
dbldns  equ     %00000001
higdns  equ     %10000000
size5   equ     %01000000
MXSTPM  equ     %00000011
DBDSTP  equ     %00010000
HLTDEL  equ     %00001000

* FMS equates
FDN    EQU 3        CURRENT DRIVE
FCS    EQU 30 - 31  CURRENT SECTOR
FSP    EQU 60 - 63  SPARE BYTES
FSB    EQU 64 - 319 SECTOR BUFFER
FMX    EQU FSP  MAX SECTOR NUMBER
*

FLEXENTRY equ   $cd00     flex starting entry

*
prcnt   equ     $cc34     spooler active
*
*****************************************************
*                                                   *
*  I/O Vector Table                                 *
*                                                   *
*****************************************************
 org $DE00

 JMP READ
 JMP WRITE
 JMP Verify
 JMP RST
 JMP DRV
 JMP CHKRDY     check ready
 JMP CHKRDY     quick check
 JMP Init
 JMP ret
 JMP SEEK
 FDB 0          driver table

Version equ *  CMISTEP test driver version
        fdb    drvstp
*****************************************************
*                                                   *
*  Variables Used By Disk Drivers                   *
*                                                   *
*****************************************************
retry   rzb     1
ltcopy  rzb     1
lside   rzb     1               actual side flag
ldens   rzb     1               actual dens flag
* drive in operation
curdrv  fcb     0               current drive
cursid  fcb     0
curdns  fcb     0
dskidx  fcb     0               disk type identifier
dskstp  fcb     0               disk step
hltdel  fcb     0               HLT delay
double  fcb     0               double step
* next 3 items have fixed place
drvtrk  fcb     0,0,0,0         track for each drive
drvsec  fcb     0,0,0,0         disk type identifier
drvstp  equ *                   CMISTEP table pointer
        fcb     0,0,0,0         step rate each drive
*
* wait for FC INT to arrive, after command
*
wait    tst     prcnt           are we spooling
        beq     wait1
        swi3    switch tasks
        nop
wait1   ldb     fo4sta
        bitb    #ST_INT
        beq     wait1
        ldb     fo2cmd          read status
        andb    #%10011001
        rts

*****************************************************
*                                                   *
*  SEEK - Seek to Track/Sector                      *
*                                                   *
*  Entry - (A) = Track Number                       *
*          (B) = Sector Number                      *
*          (X) = FCB
*                                                   *
*   Exit - (B) = Error Condition                    *
*          (Z) = 1 if no error                      *
*                0 if an error                      *
*                                                   *
*****************************************************
SEEK    pshs    d             save calling regs

        jsr     setdsk

        ldd     0,s
        stb     fo2sec
        cmpa    fo2trk
        beq     l03f
        ldb     double
        bitb    #DBDSTP    double step
        beq     skip1
        ldb     fo2trk     logical track#
        aslb               *2
        stb     fo2trk     physical track#
        asla        
skip1   sta     fo2dat
        lda     #FD_SEK
        ora     dskstp       add step rate
        sta     fo2cmd
        bsr     wait
        ldb     double
        bitb    #DBDSTP    double step
        beq     skip2
        ldb     fo2trk     physical track#
        asrb               /2
        stb     fo2trk     logical track#
skip2   ldb     fo2cmd
        andb    #%10011001
l03f    puls    d,pc

 pag
*****************************************************
*                                                   *
*  READ - Read a Sector                             *
*                                                   *
*  Entry - (A) = Track Number                       *
*          (B) = Sector Number                      *
*          (X) = Where to Place Data                *
*                                                   *
*   Exit - (Z) = 1 if no error                      *
*                0 if an error                      *
*          (B) = Error Condition                    *
*                                                   *
*****************************************************
READ    pshs    d,x,u
        lda     #3
        sta     retry
        jsr     CHKRDY
        bne     l04f

l3b     ldd     0,s             restore track/sector/data loc
        bsr     SEEK            seek to right track
        bne     l04f

        jsr     setlatch

        ldb     ltcopy
        stb     fo4lat

        ldu     2,s             buffer address
        lda     #FD_SRD
        ora     hltdel
        jsr     frdblk          do the read
        bne     l04f
        puls    d,x,u

ok      sez                     indicate no error
ret     rts

l04f    bitb    #$10            RNF?
        beq     l11f
* device
        jsr     trymode
        bne     l3b
*
l11f    stb     1,s
        puls    d,x,u

not_ok  clz
        rts
*****************************************************
*                                                   *
*  WRITE - Read a Sector                            *
*                                                   *
*  Entry - (A) = Track Number                       *
*          (B) = Sector Number                      *
*          (X) = Where to Get Data                  *
*                                                   *
*   Exit - (Z) = 1 if no error                      *
*                0 if an error                      *
*          (B) = Error Condition                    *
*                                                   *
*****************************************************
WRITE   pshs    d,x,u
        lda     #3
        sta     retry
l13b    ldd     0,s
        jsr     CHKRDY
        bne     w04f

        jsr     SEEK    seek to right track
        bne     w11f    if seek error

        jsr     setlatch

        ldb     ltcopy
        stb     fo4lat

        ldu     2,s             buffer address
        lda     #FD_SWR
        ora     hltdel
        jsr     fwrblk          do the write
        bne     w04f
        puls    d,x,u
        bra ok

w04f    jsr     trymode
        bne     l13b
*
w11f    stb     1,s
        puls    d,x,u
        bra     not_ok

 pag
*****************************************************
*                                                   *
*  Verify - Check For CRC Errors                    *
*                                                   *
*   Exit - (Z) = 1 if no error                      *
*                0 if an error                      *
*          (B) = Error Condition                    *
*                                                   *
*****************************************************
Verify jmp ok indicate no errors

*****************************************************
*                                                   *
*  RST - Seek to Track 00                           *
*                                                   *
*  Entry - (X) = FCB Address                        *
*                                                   *
*   Exit - (Z) = 1 if no error                      *
*                0 if an error                      *
*          (B) = Error Condition                    *
*                                                   *
*****************************************************
RST     pshs    x
        bsr     DRV             select the drive
        lda     #FD_RST
        ora     dskstp          add step rate
        sta     fo2cmd
        jsr     wait
        ldb     fo2cmd
        bitb    #FS_RWOK
        puls    x,pc
 pag
*****************************************************
*                                                   *
*  DRV - Select a Drive                             *
*                                                   *
*  Entry - (X) = FCB Address                        *
*                                                   *
*   Exit - (B) = $0F if non-existant drive          *
*              = Error Condition Otherwise          *
*          (Z) = 1 if no error                      *
*                0 is an error                      *
*          (C) = 0 if no error                      *
*                1 is an error                      *
*                                                   *
*****************************************************
DRV     pshs    x               save FCB
        lda     cable           check boot cable
        cmpa    #5              PC cable?
        bne     drv04
        lda     3,x             get drive number
        cmpa    #1              max 2 drives PC cable
        bls     drv02
        bra     drv0F
drv04   lda     3,x             get drive number
        cmpa    #3              max 4 drives
        bls     drv02
* illegal drive
drv0F   ldb     #$0F            set error
        sec
        puls    x,pc
*
drv02   bsr     fndtrk          get current data in >X
        sta     curdrv          save new drive
        ldb     fo2trk          current track
        stb     0,x             save it
        ldb     dskidx
        stb     4,x

        bsr     fndtrk          get data for new drive  X=>drvtrk
        lda     8,x             get more data new drive
        anda    #DBDSTP         double step bit set
        sta     double          set double step status
        lda     8,x
        anda    #MXSTPM         lower 2 bits step rate
        sta     dskstp          set step rate
        lda     8,x
        anda    #HLTDEL         HLT delay bit
        sta     hltdel          set HLT status

        ldb     0,x             set controller track
        stb     fo2trk
        ldb     4,x
        stb     dskidx

        jsr     setlatch        set latch
        clra
        puls    x,pc

*****************************************************
*                                                   *
*  CHKRDY - Check for Drive Ready                   *
*                                                   *
*  Entry - (X) = FCB Address                        *
*                                                   *
*          (Z) = 1 if drive ready                   *
*                0 if not ready                     *
*          (C) = 0 if drive ready                   *
*                1 if not ready                     *
*                                                   *
*****************************************************
CHKRDY  pshs    d,y
        clr     0,-s
        ldy     #0
        clc
*
chkr02  lda     fo2cmd
        bita    #%10000000
        bpl     chkr03
        leay    -1,y
        bne     chkr02
        inc     0,s
        lda     0,s
        cmpa    #3
        bne     chkr02
        leas    1,s               reset stack
        sec
        bra     chkr01
*
chkr03  puls    b
*
chkr01  puls    d,y,pc

fndtrk  ldx     #drvtrk         point to vars
        ldb     curdrv
        abx
        rts
 pag

*
*
MX5TRK0 equ     10
MX8TRK0 equ     15
MX5SDS0 equ     10
MX5DDS0 equ     18
MX8SDS0 equ     15
MX8DDS0 equ     26

dsktyp  equ     *
        fcb     10,10,0,$40
DTYPSZ  equ     *-dsktyp
        fcb     10,18,1,$40
        fcb     15,15,0,$00
        fcb     15,26,1,$00

MXDTYP  equ     (*-dsktyp)/DTYPSZ

***********************************************************
*
* trymode, change disk settings
*
***********************************************************
trymode pshs    b
        ldb     retry
        beq     trym00    set zero
*
        decb
        stb     retry
*
        ldb     dskidx
        incb
        cmpb    #MXDTYP
        blo     trym01
*
        clrb
trym01  stb     dskidx
*
        ldb     #$ff      clear zero
*
trym00  puls    b,pc

***********************************************************
*
* record not found, figure out side/dens for disk
*
***********************************************************
setdsk  pshs    d,x         track/sector info
        ldb     dskidx      size is checked elsewhere
        ldx     #dsktyp
        aslb
        aslb    *4
        abx
*
        ldb     2,x
        stb     curdns    density
*
        ldb     3,x
        stb     cursid    5/8 inch
*
*  track zero test
*
        tst     0,s       track zero?
        beq     tst5s1
*
* 5"/8"
*
        lda     curdns
        anda    #%00000001
        sta     ldens
*
       ldb      1,s       get sector
       cmpb     1,x       non track zero max sec/track
tst5s5 bls      tst5s3    toggle dens
*
* 5"/8" SD side 1
*
        lda     #%00000001 Side 1
        bra     tst5s2
*
* 5"/8" side 0
*
tst5s3  clra
tst5s2  sta     lside
        bra     tstd01
*
* track == zero
*
tst5s1  clr    ldens
        ldb     1,s
        cmpb    0,x       track zero max sec/track
* select proper side
        bra     tst5s5
*
tstd01  puls    d,x,pc


***********************************************************
*
***********************************************************
setlatch pshs   d
        lda     cursid
        anda    #%01000000      retain 5/8 info
        tst     ldens
        bne     stl02
        ora     #%00100000      set single dens
stl02   tst     lside
        beq     sts02
        oraa    #%00010000      set side 1
sts02   ldb     cable           check boot cable
        cmpb    #5              PC cable?
        bne     stl00
        ldb     curdrv          set lower 4 bits drive PC cable
        beq     st203
        ora     #%00001010      set drive 1 on PC cable
        bra     stl05
st203   ora     #%00000101      set drive 0 on PC cable
        bra     stl05
stl00   ldb     curdrv          set lower 4 bits for drive select!
        ldx     #drvtab         Straight cable drive table
        abx                     entry
        ora     0,x             get drive bits
stl05   sta     fo4lat          select the drive
        sta     ltcopy
        puls    d,pc

Init    ldx     #curdrv         clear variables
warm    rts

drvtab  fcb     %00000001,%00000010
        fcb     %00000100,%00001000

        org     $e100
        pag
***********************************************************
*
***********************************************************
*
* code routine, to read one dataset from FDC
* U = buffer address,
* A = command
*
* can read sector or track
* drive select, density and such alreay set up
* time out from INT fdc
*
frdblk  equ     *
        pshs    dp,x,u
        ldb     #fdbasp
        setdp   fdbasp
        tfr     b,dp
        ldb     #31  very long   create timeout
        ldx     #0              65536*2*25/4 cycles
*
        if      (WD2791=1)
        coma
        endif
        sta     <fo2cmd
*
01      orcc    #$50            disable ints
        bra     03f
* loop here
02      lda     <fo2dat         get data
        if      (WD2791=1)
        coma
        endif
        sta     0,u+             transfer
* poll fdc for DRQ
03      lda     <fo4sta
        bmi     02b             DRQ
        bne     97f             INT
        leax    1,x             count up
        bne     03b
        decb                    at zero dec B
        bne     03b             if zero abort
        bra     98f
*
97      ldb     <fo2cmd        read status
        if      (WD2791=1)
        comb
        endif
*
        bitb    #%10011111
        puls    dp,x,u,pc

98      lda     #FD_FI0+8      force immediate interrupt
        if      (WD2791=1)
        coma
        endif
        sta     <fo2cmd
* wait for INT
90      lda     <fo4sta
        beq     90b
        lda     #FD_FI0        force immediate interrupt
        if      (WD2791=1)
        coma
        endif
        sta     <fo2cmd
        bra     97b

*
* code routine, to write one dataset to the FDC
* U = buffer address
* A = command
*
* drive select, density and such alreay set up
* time out from INT fdc
*
fwrblk  equ     *
        pshs    dp,x,u
        ldb     #fdbasp
        tfr     b,dp
*
        if      (WD2791=1)
        coma
        endif
        sta     <fo2cmd
*
01      orcc    #$50            disable ints
        bra     03f

02      lda     0,u+
        if      (WD2791=1)
        coma
        endif
        sta     <fo2dat         put data
*
03      lda     <fo4sta
        bmi     02b
        beq     03b
*
        ldb     <fo2cmd         read status
        if      (WD2791=1)
        comb
        endif
*
99      bitb    #%11011111
        puls    dp,x,u,pc
*
        setdp   0               reset dp

*
        end     FLEXENTRY
