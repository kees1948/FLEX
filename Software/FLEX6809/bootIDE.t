
*              FLEX IDE BOOT SKELETAL
*
*   This is the second parts of the FLEX boot process. It is the
*   FLEX locator and loader that resides on sectors 1 and 2 of 
*   the IDE drive and is loaded int memory at $C000 by the ROM
*   with the use of the 'W' command.
*

 lib iospace.h

fdccmd  equ     io_space+$100   command register
fdctrk  equ     fdccmd+1   drive register
fdcsec  equ     fdccmd+2   sector register
fdcdat  equ     fdccmd+3   data register
fdcsel  equ     fdccmd+4   drive select
fdcsta  equ     fdccmd+8   drive status
IOdp    equ     fdccmd/256
*
D5INCH  equ     %01000000
DSINGLE equ     %00100000
*
stack   equ     io_space+$07C0        2       top of internal stack / user vector
trap    equ     io_space+$07C2        2
swi3    equ     io_space+$07C4        2       software interrupt vector #3
swi2    equ     io_space+$07C6        2       software interrupt vector #2
firq    equ     io_space+$07C8        2       fast interrupt vector
irq     equ     io_space+$07CA        2       interrupt vector
swi     equ     io_space+$07CC        2       software interrupt vector
svcvo   equ     io_space+$07CE        2       supervisor call vector orgin
svcvl   equ     io_space+$07D0        2       supervisor call vector limit
cport   equ     io_space+$07D2        2       re-vectorable control port
echo    equ     io_space+$07D4        1       echo flag
flpspd  equ     io_space+$07D5        1       floppy data speed
bptbl   equ     io_space+$07D6        24      breakpoint table base addr
cable   equ     io_space+$07EE        1       cable type
IOpnt   equ     $FFE0                         IO space pointer

*----------------------------------------------------------
* Load FLEX from IDE
*
*   Since this part must be less than or equal to 512 bytes so 
*   it can fit in the first two sectors of the disk, , it needs 
*   to be short and sweet. In order to use the ROM IDE read 

*   sector code, we have provided a jump instruction to it in
*   the ROM at the last three bytes just before the hardware
*   vectors ($FFED = JMP READ).
*
IDEREAD EQU     $FFED 
DESCRIP equ     $FFE2
*   This needs to be written to the disk not as STX records, 
*   but pure binary and is ORGed at the address he ROM part loads 
*   it to namely loader
*
*   cable type location depending on IOdp

        ORG     $C100          Address where SECTOR 1 and 2 LOADED
        
* FLEX loader
LOADER  lda     #IOdp
        setdp   IOdp
        bra     LDFLEX
        fcb     bcsize/256      number sectoren
LTRKNO  fdb     0               cylinder to load flex from
LSECNO  fcb     0               sector to load flex from
XFER    fdb     0               transfer address
LDPTR   fdb     0               load pointr address
PNTR    fdb     0               buffer next read pointer

DRVDES  EQU     $DE20           descriptor pointer

* these are vectors and must be used indirectly

monitor    EQU      $F800 
nextcmd    EQU      $F802 
inch       EQU      $F804 input char (raw) 
inche      EQU      $F806 input and echo 
inchek     EQU      $F808 input check (if char is available) 
outch      EQU      $F80A output char (raw) 
pdata      EQU      $F80C 
pcrlf      EQU      $F80E 
pstrng     EQU      $F810 
lra        EQU      $F812 

*-------------------------------------------------------------
* FLEX LOADER   
*
* Load FLEX from the LINK in provided in sector 1

* DRIVE DESCRIPTOR LOADED HERE
* If always boot on drive H0, offset $0000
* use DESCRIPTOR in MONITOR.
* 
* Remove table below, set DESWIN equ DESCRIP
*DESWIN  FCB     1               winchester
*        FCB     0               hardware drive number
*        FCB     0               
*        FCB     0               
*        FCB     0               
*        FCB     0               
*        FDB     0               track offset
DESWIN equ DESCRIP

*              $$$$$ BOOTCODE START $$$$            *

*
*   first figure out what type of cable is being used
*
LDFLEX  tfr     a,dp
        clr     <fdcsel
        lda     #%00000001  select drive 0
        sta     <fdcsel
        bsr     Delay
        ldb     <fdccmd     ready?
        bmi     set5        no - try PC cable type
        bra     set1        yes - set straight thru type

Delay   ldb     #5
        ldx     #0
loop    leax    1,x
        cmpx    #0
        bne     loop
        tst     <fdcsta  keep active
        decb
        bne     loop
        rts
       
set5    lda     #%00000101  drive 0 not found on straight cable
set1    ldx     LOADER+1
        leax    cable-(IOdp*256+$20),x
        sta     0,x          Set PC cable type
                
* drive select on cable, 1 = FLEX 5 = PC compatible
        clra
        sta     <fdcsel      deselect drive
        tfr     a,dp
        setdp 0
* test monitor
        ldd     DESWIN
        cmpd    #$0100         Check descriptor table
        bne     FAIL
* INITIALIZE TEMP STORAGE
        ldx     #PNTR+1
        ldab    #PNTR+1-XFER
init    clr     b,x
        decb
        bne     init
        LDX     #DESWIN
        STX     DRVDES
        bsr     REDSEC          read first sector
LOAD    bsr     READAT          get data
        CMPA    #$02            test for load record
        BEQ     BINREC
        LDX     #0
        CMPX    LBUF
        BNE     LOAD

* PROCESS TRANSFER RECORD

        CMPA    #$16            test for transfer record
        BNE     LOAD
        BSR     READAT
        STA     XFER
        BSR     READAT
        STA     XFER+1
        LDX     XFER
        JMP     0,X

FAIL    LDX     #FAILMSG
        JSR     [pdata]
        JMP     [$F800]         restart ptmon

REDSEC  LDD     LTRKNO          GET STARTING TRACK AND SECTOR
RESEC1  LDX     #LBUF
        JSR     IDEREAD         CALL the ROM READ routione
        bcs     FAIL            yes : restart
        LDX     #LDATA          point x at beginning of data
        STX     PNTR            save pointer
        RTS

* GET DATA BYTE AND ADVANCE POINTER

READAT  LDX     PNTR            get pointer
        CPX     #DATEND         end of data?
        BEQ     REDAT1          yes: read next sector
        LDA     0,X+            no: get next byte
        STX     PNTR            save pointer
        RTS

* READ NEXT SECTOR

REDAT1  LDX     #LBUF           get the pointer to the linka bytes
        LDD     0,X             get the next sector to read in
        bsr     RESEC1          read next sector
        BRA     READAT          continue getting the next byte

* BINARY FILE LOADER

BINREC  BSR     READAT          get msb of address
        STA     LDPTR
        BSR     READAT          get lsb of address
        STA     LDPTR+1
        BSR     READAT          get character count
        TFR     A,B
        TSTB
        BEQ     LOAD
        LDY     LDPTR           get load address
        PSHS    B               save byte count
        BSR     READAT
        PULS    B               restore byte count
        BRA     BLOAD0
BLOAD   LDA     0,X+
BLOAD0  STA     0,Y+
        CMPX    #DATEND
        BEQ     BLOAD2
BLOAD1  DECB
        BNE     BLOAD
        STX     PNTR
        lbra    LOAD

BLOAD2  PSHS    B,Y
        LDX     #LBUF
        LDD     0,X
        bsr     RESEC1
        LDX     #LDATA
        PULS    B,Y
        BRA     BLOAD1

FAILMSG FCC     /LOAD ERROR/
 FCB 4

*              $$$$$ BOOTCODE END $$$$              *
end equ *
 IF end<$C200 1 sector
    rzb 256-(end<<8/256) fill up
loadend equ * BOOT size = 1 sector
 ELSE 2 sector size
    rzb 256-(end<<8/256) fill up
loadend equ * BOOT size = 2 sectoren
 ENDIF
bcsize  equ loadend-LOADER-1

LBUF    EQU     *
LDATA   EQU     LBUF+4
DATEND  EQU     LBUF+256
