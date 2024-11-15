 OPT PAGE
 NAM IDE & FLP DISK DRIVER
 PAG

* Last modified 11-11-2024 CAJ
 lib iospace.h

* extern pointers
FLEX  equ $CD00 FLEX cold start
prcnt equ $CC34 spooler
cable equ io_space+$07EE cable type
PDATA equ $FC08 cmisbug print string
STACK1 equ $C6FF start address of stack

* The CPUXXCMI system has the IDE driver located at $E200

HREAD equ dr_shift+$0200 -- READ JMP READ1
HWRITE equ dr_shift+$0203 -- WRITE JMP WRITE1
HVERIF equ dr_shift+$0206 -- VERIFY JMP VER1
HREST equ dr_shift+$0209 -- RESTOR JMP REST1
HSEL equ dr_shift+$020C -- DRVSEL JMP DRVSL1
HRDY equ dr_shift+$020F -- CDRRDY JMP CDRR1
QHRDY equ dr_shift+$0212 -- QDRRDY JMP CDRR1
HCOLD equ dr_shift+$0215 -- DCOLDS JMP DCOLD

*
* fdc hardware
*
fdcbas equ io_space+$0100 FDC controller
fdbasp equ fdcbas/256 for direct addressing
* subject to EOR4FDC
fo2cmd equ fdcbas offset to command register
fo2trk equ fdcbas+1 offset to track register
fo2sec equ fdcbas+2 offset to sector register
fo2dat equ fdcbas+3 offset to data register
* not subject to EOR4FDC
fo4lat equ fdcbas+4 drive,side,density latch
fo4sta equ fdcbas+8 fdc status buffer

* HW debug select
fo2trg equ fdcbas+12 scope trigger @ this address

*************************************************
* 
* I/O Vector Table 
* 
*************************************************

 ORG $DE00

LDE00 JMP >FREAD 
LDE03 JMP >FWRITE 
FVERIFY JMP >Verify 
FRESTOR JMP >RST 
SELECT JMP >DRV 
CHECK JMP >CHKRDY 
QCHECK JMP >CHKRDY 
DRINIT JMP >DRVINIT 
WINITD JMP >ret 
SEEKIT JMP >SEEK 
 FDB $0000 
DRVPTR FDB DRVBEG INIT TO FIRST DRIVE 

* Define a Default drive descriptor so these drivers work
* if a descriptor table is not appended to FLEX

DRVBEG equ * use extern map to append!
* FCB 1,0,0,0,0,0,0,0 Drive 0 - HD Partition 0 
* FCB 1,0,0,0,0,0,0,1 Drive 1 - HD Partition 1 
* FCB 0,0,0,0,0,0,0,0 Drive 2 - Floppy Drive 0 
* FCB 0,1,0,0,0,0,0,0 Drive 3 - Floppy Drive 1 

* gobal fopr all drives
 org $DE42
retry fcb $00
ltcopy fcb $00
lside fcb $00
ldens fcb $00

* just for drive in operation

curdrv fcb $00
cursid fcb $00
curdns fcb $00
dskidx fcb $00
dskstp fcb $0 disk step
hltdel fcb $0 HLT delay
double fcb $0 double step

* next 3 items have fixed place

drvtrk fcb 0,0,0,0
drvsec fcb 0,0,0,0
drvstp fcb 0,0,0,0 step rate each drive

* WINCHESTER CHECK SET-UP
* see if the drive number has been assigned to an IDE partition

WINSET pshs x 
 ldx DRVPTR 
 TST DISK,x 
 puls x 
 RTS 
*
* SUBROUTINE: wait 
*
* ENTRY: 
* EXIT: 
*
wait TST >prcnt
 BEQ wait1 
 SWI3 
 NOP 
wait1 LDAB >fo4sta 
 BITB #$40 
 BEQ wait1 
 LDAB >fo2cmd 
 ANDB #$99 
 RTS 

*************************************************
* 
* SEEK - Seek to Track/Sector 
* 
* Entry - (A) = Track Number
*         (B) = Sector Number 
*         (X) = FCB
*
* Exit - (B) = Error Condition 
*        (Z) = 1 if no error 
*              0 if an error 
* 
*************************************************
SEEK PSHS A,B save calling regs
 JSR >setdsk 
 LDD ,S 
 STAB >fo2sec 
 CMPA >fo2trk offset to track register 
 BEQ l03f 
 ldb double
 bitb #%00010000 double step
 beq skip1 no - 
 
 ldb fo2trk yes - logical track#
 aslb *2
 stb fo2trk physical track#
 asla 
 
skip1 STAA >fo2dat 
 LDAA #$18 
 ora dskstp add step rate
 STAA >fo2cmd 
 BSR wait 
 
 ldb double
 bitb #%00010000 double step
 beq skip2
 ldb fo2trk physical track#
 asrb /2
 stb fo2trk logical track#
 
skip2 ldb fo2cmd
 andb #%10011001
 
l03f PULS A,B,PC 

*************************************************
* 
* READ - Read a Sector 
* 
* Entry - (A) = Track Number 
*         (B) = Sector Number 
*         (X) = Where to Place Data 
* 
* Exit - (Z) = 1 if no error 
*              0 if an error 
*        (B) = Error Condition 
* 
*************************************************
FREAD PSHS A,B,X,U 
 bsr WINSET
 BEQ l13f floppy

* do IDE stuff here

 PULS A,B,X,U
 JMP HREAD
 
l13f LDAA #$03 
 STAA >retry 
 JSR >CHKRDY 
 BNE l04f 
l3b LDD ,S restore track/sector/data loc 
 BSR SEEK seek to right track 
 BNE l04f 
 JSR >setlatch 
 LDAB >ltcopy 
 STAB >fo4lat 
 LDU 2,S buffer address 
 LDAA #$88 
 JSR >frdblk do the read 
 BNE l04f 
 
 PULS A,B,X,U 
ok ORCC #$04 
ret RTS 

l04f BITB #$10 
 BEQ l11f 
 JSR >trymode 
 BNE l3b 
l11f STAB 1,S 
 PULS A,B,X,U 
 
not_ok ANDCC #$FB 
 RTS 

FWRITE PSHS A,B,X,U 
 JSR WINSET 
 BEQ l13a floppy

* do IDE stuff here

 PULS A,B,X,U
 JMP HWRITE
 
l13a LDAA #$03 
 STAA >retry 

l13b LDD ,S 
 JSR >CHKRDY 
 BNE w04f 
 JSR >SEEK seek to right track 
 BNE w11f if seek error 
 
 JSR >setlatch 
 LDAB >ltcopy 
 STAB >fo4lat 
 LDU 2,S buffer address 
 LDAA #$A8 
 JSR >fwrblk do the write 
 BNE w04f 
 
 PULS A,B,X,U 
 BRA ok 

w04f JSR >trymode 
 BNE l13b 
 
w11f STAB 1,S 
 PULS A,B,X,U 
 BRA not_ok 

*************************************************
* 
* Verify - Check For CRC Errors 
* 
* Exit - (Z) = 1 if no error 
*              0 if an error 
*        (B) = Error Condition 
* 
*************************************************

Verify JMP >ok 

*************************************************
* 
* RST - Seek to Track 00 
* 
* Entry - (X) = FCB Address 
* 
* Exit - (Z) = 1 if no error 
*              0 if an error 
*        (B) = Error Condition 
* 
*************************************************

RST PSHS X save the FCB
 BSR DRV select the drive 
 LDAA #$08 floppy reset command
 STAA >fo2cmd reset the floppy
 JSR >wait wait before checking for finished
 LDAB >fo2cmd get status
 BITB #$D8 check for errors
 PULS X,PC return
 
*************************************************
* 
* DRV - Select a Drive 
* 
* Entry - (X) = FCB Address 
* 
* Exit - (B) = $0F if non-existant drive 
*            = Error Condition Otherwise 
*        (Z) = 1 if no error 
*              0 is an error 
*        (C) = 0 if no error 
*              1 is an error 
* 
*************************************************
DRV PSHS X save FCB
 JSR REST1 this will set DRVPTR properly
 JSR WINSET
 BEQ drv01

* do the IDE stuff here - nothing to do - it was done in REST1

 PULS X,PC 
 
drv01 LDAA >cable check boot cable
 CMPA #$05 PC cable?
 BNE drv04 
 LDAA 3,X get drive number
 
* now we need to get the physical drive number

 CMPA #$01 max 2 drives PC cable
 BLS drv02 valid drive number
 BRA drv0F 

drv04 LDAA 3,X get drive number
 JSR GETPHYS get the physical drive number in B
 CMPB #$03 max 4 drives
 BLS drv02 

* illegal drive
 
drv0F ldab #$0F set error
 orcc #$01 
 puls X,PC 

* save drive information for previously selected floppy drive.

drv02 bsr fndtrk get current data in >X
 staa >curdrv save new drive (logical)
 ldab >fo2trk current track
 stab ,X save it
 ldab >dskidx 
 stab 4,X 

 bsr fndtrk get data for new drive X=>drvtrk

 lda 8,x get more data new drive
 anda #%00010000 double step bit set
 sta double set double step status
 lda 8,x 
 anda #%00000011 lower 2 bits step rate
 sta dskstp set step rate
 lda 8,x 
 anda #%00001000 HLT delay bit
 sta hltdel set HLT status
 
 ldab ,X get more data new drive
 stab >fo2trk double step bit set
 ldab 4,X set double step status
 stab >dskidx 
 
 jsr >setlatch lower 2 bits step rate
 clra 
 puls X,PC 

*************************************************
* 
* CHKRDY - Check for Drive Ready 
* 
* Entry - (X) = FCB Address 
* 
*         (Z) = 1 if drive ready 
*               0 if not ready 
*         (C) = 0 if drive ready 
*               1 if not ready 
* 
*************************************************

CHKRDY PSHS A,B,Y 
 JSR WINSET
 BEQ chk01

* do IDE stuff here

 PULS A,B,Y
 JMP HRDY
 
chk01 CLR ,-S clear outer loop count
 LDY #$0000 
 ANDCC #$FE clc
chk02 LDAA >fo2cmd get fdc status
 BITA #$80 is the NOT READY bit set? 
 BPL chk03 no - controller is ready
*
 LEAY -1,Y decrement inside counter 
 BNE chk02 no 0 yet - loop inside
*
 INC ,S increment outer loop counter 
 LDAA ,S get outer loop counter
 CMPA #$03 have we tried 3 times yet?
 BNE chk02 no - keep trying
*
 LEAS 1,S fix the stack
 ORCC #$01 set the carry bit in the CCR
 BRA chkr01 return

chk03 PULS B 
chkr01 PULS A,B,Y,PC 

fndtrk LDX #drvtrk 
 LDAB >curdrv 
 ABX 
 RTS 

 IF dr_shift>$E000
   ORG dr_shift
 ENDIF


dsktyp FCB $0A,$0A,$00,$40,$0A,$12,$01,$40
 FCB $0F,$0F,$00,$00,$0F,$1A,$01,$00

*******************************************************
*
* trymode, change disk settings
*
*******************************************************

trymode PSHS B 
 LDAB >retry 
 BEQ trym00 set zero 
 
 DECB 
 STAB >retry 
 LDAB >dskidx 
 INCB 
 CMPB #$04 
 BCS trym01 
 CLRB 
trym01 STAB >dskidx 
 LDAB #$FF 
trym00 PULS B,PC 

*******************************************************
*
* record not found, figure out side/dens for disk
*
*******************************************************

setdsk PSHS A,B,X track/sector info 
 LDAB >dskidx size is checked elsewhere 
 LDX #dsktyp 
 ASLB 
 ASLB *4 
 ABX 
 LDAB 2,X 
 STAB >curdns density 
 LDAB 3,X 
 STAB >cursid 5/8 inch 
 
* track zero test
 
 TST ,S track zero? 
 BEQ tst5s1 
 
* 5"/8"
 
 LDAA >curdns 
 ANDA #%00000001 #$01
 STAA >ldens 
 LDAB 1,S get sector 
 CMPB 1,X non track zero max sec/track 
tst5s5 BLS tst5s3 toggle dens 

* 5"/8" SD side 1

 LDAA #%00000001 Side 1 
 BRA tst5s2 

* 5"/8" side 0

tst5s3 CLRA 
tst5s2 STAA >lside 
 BRA tstd01 

* track == zero

tst5s1 CLR >ldens 
 LDAB 1,S 
 CMPB ,X 
* select proper side
 BRA tst5s5 

tstd01 PULS A,B,X,PC 

*******************************************************
* setlatch
*******************************************************
setlatch PSHS A,B 
 LDAA >cursid 
 ANDA #%01000000 retain 5/8 info $40
 TST >ldens 
 BNE stl02 
 
 ORAA #%00100000 set single dens $20
stl02 TST >lside 
 BEQ sts02 
 
 ORAA #%00010000 set side 1 $10
sts02 LDAB >cable check boot cable
 CMPB #$05 PC cable?
 BNE stl00 no - do straight thru cable
 
 LDAB >curdrv set lower 4 bits drive PC cable
 BEQ st203 
 
 ORAA #%00001010 set drive 1 on PC cable $0A
 BRA stl05 

st203 ORAA #%00000101 set drive 0 on PC cable $05
 BRA stl05 

stl00 LDAB >curdrv set lower 4 bits for drive select!

 PSHS A save A - has bits for latch
 TBA put logical drive number in A
 JSR GETPHYS B will now have physical number
 PULS A restor A - bits for latch
 
 LDX #drvtab Straight cable drive table
 ABX entry
 ORAA ,X get drive bits
 
stl05 STAA >fo4lat select the drive
 STAA >ltcopy 
 PULS A,B,PC 
 
DRVINIT LDX #curdrv clear variables
 RTS 

drvtab FCB $01,$02,$04,$08

 ORG dr_shift+$0100

*******************************************************
* read
*
* code routine, to read one dataset from FDC
*
* U = buffer address,
* A = command
*
* can read sector or track
* drive select, density and such alreay set up
* time out from INT fdc
*
*******************************************************

frdblk PSHS DP,X,U save the direct page register
 ldb #fdbasp set it to point to the floppy controller
 TFR B,DP direct page register now points to the floppy controller
 setdp fdbasp
 LDAB #31 set delay outside counter
 LDX #0 set delay inside counter
 STAA <fo2cmd set the command in the floppy command register
 ORCC #$50 set the interrupt flag = no interrupts
 BRA frb03 

frb02 LDAA <fo2dat get byte from floppy data register
 STAA ,U+ 
frb03 LDAA <fo4sta get floppy status
 BMI frb02 
 BNE frb97 
 LEAX 1,X 
 BNE frb03 
 DECB 
 BNE frb03 
frb98 LDAA #$D8 
 STAA <fo2cmd 
frb90 LDAA <fo4sta get floppy status
 BEQ frb90 
 LDAA #$D0 
 STAA <fo2cmd 
frb97 LDAB <fo2cmd 
 BITB #$9F 
 PULS DP,X,U,PC 


*******************************************************
* write
*
* code routine, to write one dataset to the FDC
*
* U = buffer address
* A = command
*
* drive select, density and such alreay set up
* time out from INT fdc
*
*******************************************************

fwrblk PSHS DP,X,U 
 ldb #fdbasp
 TFR B,DP 
 STAA <fo2cmd 
 ORCC #$50 
 BRA fwb03 

fwb02 LDAA ,U+ 
 STAA <fo2dat 
fwb03 LDAA <fo4sta get floppy status
 BMI fwb02 
 BEQ fwb03 
 LDAB <fo2cmd 
 BITB #$DF 
 PULS DP,X,U,PC 
 setdp 0 reset dp 

* FOR SWTPC/6809 ROM ADDRESS $E800
* WINCHESTER DISK DRIVER FOR WD1002-HD0 CONTROLLER
*
* COPYRIGHT 1984 PERIPHERAL TECHNOLOGY
* ALL RIGHTS RESERVED
*
* LAST CHANGE 5/25/85 for the WD1002 Origional Code
* LAST CHANGE 10/22/21 14:32
* LAST CHANGE 5/29/2024 modified for use with CPUXXCMI
* LAST CHANGE 7/11/2024 rewrite the READ & WRITE (CAJ)
* LAST CHANGE 9/10/2024 rewrite the TSKSET (CAJ)

* This version will only work with the CPU09IDE baord
* since it uses 16 bit access to the controller
*
* for purpose of this driver, the IDE device is put in LBA mode.
*
* Drive arangemnet for this rewrite is for 16MB partitions
* You may format a smaller size, but the driver works in 16MB blocks
* IDE/SD devices use 512 byte sector size and 16 bit transfer per read.
*
* Offset is the number of 16MB offsets for the partition being accessed.
*
* DRIVE DESCCRIPTORS - WINCHESTER
*
DISK equ $0 0=FLOPPY <>0 - WINCHESTER
DRVNO equ DISK+1 HARDWARE DRIVE NUMBER - Must be 0 or 1
STEP equ DRVNO+1 Not used was STEP RATE
INITED equ STEP+1 0=NOT INITIALIZED
SHIFT equ INITED+1 Not used was <9 NUMBER OF SHIFTS >8 DIV
HMASK equ SHIFT+1 Not used was HEAD MASK
OFFSET equ HMASK+1 Partition OFFSET - number of 16MB offsets
DRVSIZ equ OFFSET+2 EQU * DESCRIPTOR SIZE 

* FLOPPY DRIVER EQUATES

version EQU $DE20 DESCRIPTOR POINTER

* PORT DEFINITION FOR IDE

BASADR EQU io_space+$0180 CPU09IDE board in CPUXXCMI system
basdp EQU BASADR/256

DATA EQU BASADR DATA REGISTER
ERROR EQU BASADR+2 ERROR REGISTER
WPC EQU BASADR+2 WRITE PRECOMP REGISTER
SECNT EQU BASADR+4 SECTOR COUNT
SECNO EQU BASADR+6 SECTOR NUMBER
CYLLO EQU BASADR+8 CYLINDER NUMBER (LSB)
CYLHI EQU BASADR+10 CYLINDER NUMBER (MSB)
SDH EQU BASADR+12 SIZE/DRIVE/HEAD REGISTER
STATUS EQU BASADR+14 STATUS REGISTER
COMREG EQU BASADR+14 COMMAND REGISTER
idestat equ BASADR+24 alternative status

* COMMAND DEFINITIONS FOR IDE

READCM EQU $20 READ COMMAND
WRTCMD EQU $30 WRITE COMMAND

 PAG
 ORG dr_shift+$0200 FOR CPUXXCMI

* DISK JUMP TABLE
*
* This is here to make the IDE driver portable - do not remove
* If dr_split = $E400 then read $E6.. for the following entry's

READ JMP READ1 $E200 <- this variable (READ) is not referenced anywhere
WRITE JMP WRITE1 $E203
VERIFY JMP VER1 $E206
RESTOR JMP REST1 $E209
DRVSEL JMP DRVSL1 $E20C
CDRRDY JMP CDRR1 $E20F
QDRRDY JMP CDRR1 $E212
DCOLDS JMP DCOLD $E215
DWARMS JMP RTS $E218

*******************************************************
* READ SECTOR COMMAND IDE
*
* Entry
*     X - Address to place sector - 256 bytes
*     A - Sector Number
*     B - Track Number
* Exit
*     X,A May be destroyed
*     B - Error Code
*     Z - 1 if no error
*     Z - 0 if an error
*
*******************************************************

READ1 equ *
 pshs dp,b 
 ldb #basdp
 tfr b,dp Speedup -490 cpu cycles
 puls b
 setdp basdp
rdsect bsr TSKSET
 ldd #READCM
 std <COMREG
 clr 0,-s init bytecount

rslop ldd <STATUS READ STATUS (actual status will be in B)
 aslb check for DRQ
 bcs rslop if not set keep going

 ldd <DATA get word
 stb 0,x+ use lower part
 dec 0,s update counter
 bne rslop

 puls a empty stack
 ldd <STATUS
 bitb #1 
 beq rddone
 ldb #$10 set error
 sec
 puls dp,pc
rddone clrb clear carry
 puls dp,pc
 setdp 0

*******************************************************
* WRITE COMMAND IDE
*
*
*     X - Address of 256 bytes to be written
*     D - Logical sector number
* EXIT
*     X,A may be destroyed
*     B - Error Code - 0=None
*     Z - Z=1 if no error
*     Z - Z=0 if error
*
* all controller access MUST be 16 bit
*
*******************************************************

WRITE1 equ *
 pshs dp,b
 ldb #basdp
 tfr b,dp Speedup -490 cpu cycles
 puls b
 setdp basdp
wrsect bsr TSKSET
 ldd #WRTCMD GET WRITE COMMAND
 std <COMREG
 clr 0,-s init bytecount
*
wrlop ldd <STATUS READ STATUS (actual status will be in B)
 aslb check for DRQ
 bcs wrlop if not set keep going
 ldb 0,x+ get lower part
 std <DATA save word

 dec 0,s update counter
 bne wrlop
*
 puls a empty stack
wrstat ldd <STATUS
 bitb #$80
 bne wrstat
 bitb #1 
 beq wrdone
 clrb clear carry
 puls dp,pc

wrdone ldb <ERROR set error
 sec
 puls dp,pc
 setdp 0

*******************************************************
* VERIFY DISK DRIVER 
*
*
* The sector just written is to be verified.
* This routine only called immediately after a write
* 
* Entry
*      No parameters are given
* Exit
*     X,A may be destroyed
*     B - Error code - 0=none
*******************************************************

* warning warning Will Robinson 
* ---- IT PROBABLY DOESN'T WORK ----
* UNLESS TO READ COMMAND CAN BE CANCELED THIS ROUTINE
* PROBABLY LEAVES THE DRIVE IN A BUSY STATE UNTIL
* 256 READS CAN BE PERFORMED ON THE DATA REGISTER

VER1 CLRB --- EXIT - ROUTINE NOT VIABLE
RTS RTS

*******************************************************
* TASK SET-UP ROUTINE 
* Entry
*     X - Address to place sector - 256 bytes
*     A - Sector Number
*     B - Track Number
*******************************************************
*
* INPUT D = 16 BIT LOGICAL SECTOR NUMBER WITHIN THE PARTITION
* 
* Driver currently supports only 256 partitions
*
* OUTPUT IDE registers set
*
TSKSET pshs Y  Speedup -17 cpu cycles
 pshs A save
 CLRA
 STD SECNO Set LSB of LSN
 puls B get A in B
 STD CYLLO Set MSB of LSN

* the logical sector number is set - now add in the partition offset

 LDY DRVPTR point to descriptor
 LDAD OFFSET,Y get parttion number
 STD CYLHI

* and the drive select bits

 CLRA
 LDAB DRVNO,Y Get drive Number
 LSLB Put in corrent bit position
 LSLB
 LSLB
 LSLB
 ORB #$E0 Drive number + LBA Mode
 STD SDH Select Drive and LBA and upper 4 bits of LBA

 LDD #1 SET FOR ONE SECTOR READ/WRITE
 STD SECNT SET IDE REGISTER
 puls Y,PC
 
*******************************************************
* GETPHYS ROUTINE
*
* Entry
*     A - contains logical drive number
* Exit
*     B - physical drive number for the controller
*******************************************************

GETPHYS PSHS A,X
 LDB #DRVSIZ GET DESCRIPTOR SIZE
 MUL CALCULATE OFFSET A times B
 LDX #$DE22 DRVBEG
 ABX
 LDAB DRVNO,X get physical drive number
 PULS A,X
 RTS 
 
*******************************************************
* RESTORE COMMAND 
*
* Drive in the FCB address (3,X contains drive number) should be selected
* before the RESTORE is performed.
*
* Entry
*     X - FCB address - 3,X contains drive number
* Exit
*     X,A - may be destroyed
*     B - Error code - 0=no error
*******************************************************

* While there is nothing to do for a modern drive, FLEX
* expects DRIVE SELECT to be called by this routine. It is therefore
* necessary to set the DRVPTR pointer with this routine.

REST1 PSHS X
 LDA 3,X GET DRIVE NUMBER
 LDB #DRVSIZ GET DESCRIPTOR SIZE
 MUL CALCULATE OFFSET
 LDX #$DE22 DRVBEG
 ABX
 STX DRVPTR
 PULS X
 CLRB No error
 RTS 

*******************************************************
*
* DRIVE READY TEST 
*
* Drive number found in the FCB should be checked for ready
*
* Entry
*     X - FCB address - 3,X contains drive number
* Exit
*     X,A - may be destroyed
*       B - Error code - 0=no error

*******************************************************

CDRR1 LDD STATUS
 BITB #$40 Drive ready bit
 BEQ CDRR2 branch for no ready

 CLRB clear error flag
 RTS

CDRR2 LDB #$80 set not ready error
 SEC
 RTS

*******************************************************
* COLD START - INIT
*
* Do any driver initialization here when the system is first booted.
*
* Entry
*     No parameters are given
* Exit
*     A,B,X,Y,U may be destroyed
* No exit code for this routine. Simply issue RTS
*
*******************************************************

DCOLD LDD STATUS Get status
 BITB #$80 Test Busy bit
 BNE DCOLD loop until ready

* This would be a good place to insert time out code
* and print a message if no drive plugged in

 RTS

*******************************************************
* DRIVE SELECT ROUTINE 
*
* Drive specified is selected
*
* Entry
*     X - FCB address - (3,X contains drive number)
* Exit
*     X,A may be destroyed
*     B - $0F error code for non-existant drive
*     Z - 1=error 0=no error
*     C - 0=error 1=error
*
*******************************************************

* For this driver, it means the DRVPTR must be set so
* READ/WRITE works on the correct drive.
* DRVPTR IS TO BE SET BY "DISK.TXT" DRIVER THAT
* IS LOCATED IN SPACE RESERVER FOR FLOPPY DRIVERS
* IN FLEX.
*
* The drive is selected during the Task setup routine
* There is nothing to do to the drive here

* THIS ENTRY POINT IS MAINTAINED SO THAT
* IF AT SOME POINT IN THE FUTURE SOMETHING NEEDS
* TO BE DONE, THE ENTRY POINT ALREADY EXISTS.

DRVSL1 bsr REST1
 RTS

FAILMSG FCC 'LOAD FAILED'
 FCB 4

FAIL LDX #FAILMSG
 JSR [PDATA]
 JMP [$F800] RESTART PTMON

 END FLEX cold start
