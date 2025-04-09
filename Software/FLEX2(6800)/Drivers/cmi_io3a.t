 nam flex 6800 SSSD driver
*
* Use FLEX9 [CRASMB + I6800.BIN]
*  Watch the Mnemonic structure:
*  instruction-space-register ,X (upper case)
*
 CRO I6800

* FLEX COR Versie 2 or 3 !
Versie EQU 3 <<<<

COLDS EQU $ad00
PFLG EQU $AC34
 pag

 lib flp2inf.h

 org $BE00
************************************
fsetup LDX #curdrv
fsu01 CLR 0,X
 INX
 CPX #iniend
 BNE fsu01
 RTS

 org $BE20
************************************
* is cleared on init
curdrv fcb $00
xsave3 fdb $0000
trktab fcb $00,$00,$00,$00
iniend EQU *
stpbyt fcb $00 steprate, 00,01,10,11
drvset fcb %01100000 5", SD

************************************
* 
* Disk I/O Vector Table 
*
************************************
 org $BE80
fread JMP readsc
fwrit JMP writsc
fverf JMP verfsc
frest JMP restor
fdriv JMP seldrv
fcrdy JMP tready
fqick JMP tready
finit JMP fsetup

************************************
*
* read sector
*
* A=track, B=sector, X=buffer
*
************************************
readsc BSR flseek
 JSR setlat
 JSR tready
 BNE sbee4
 LDA A #$8c
 TST PFLG
 BEQ sbebd
 SWI
sbebd NOP
 SEI
 STA A fo2cmd
 BRA read01

read02 LDA A fo2dat
 STA A 0,X
 INX
read01 LDA A fo4sta
 BMI read02
 BEQ read01
sbee4 BSR gtfdst
sbee0 BIT B #%10011100 NRDY, RNF, CRC, LOST
 NOP
 CLI
 RTS

gtfdst TST PFLG
 BEQ sbeeb
 SWI
sbeeb LDA B fo2cmd
 RTS

************************************
*
* seek
*
************************************
flseek CMP A fo2trk
 BEQ sbf0a
 STA A fo2dat
 LDA A #$18
 ORA A stpbyt set steprate
 STA A fo2cmd
 PSH B
flsk01 LDA B fo4sta
 BEQ flsk01
 BSR gtfdst
 PUL B
sbf0a STA B fo2sec
 RTS

************************************
*
* write sector
*
************************************
writsc BSR flseek
 JSR setlat
 JSR tready
 BNE sbf38
 LDA A #$ac
 TST PFLG
 BEQ sbf1a
 SWI
sbf1a NOP
 SEI
 STA A fo2cmd
 BRA wrsc01

wrsc02 LDA A 0,X
 STA A fo2dat
 INX
wrsc01 LDA A fo4sta
 BMI wrsc02
 BEQ wrsc01
sbf38 BSR gtfdst
sbf3d BIT B #$5c
 NOP
 CLI
 RTS

************************************
*
* verify sector
*
************************************
verfsc LDA A #$8c
 TST PFLG
 BEQ sbf4a
 SWI
sbf4a NOP
 SEI
 STA A fo2cmd
 BRA verf01
verf02 LDA A fo2dat
verf01 LDA A fo4sta
 BMI verf02
 BEQ verf01
 BSR gtfdst
 NOP
 CLI
 BIT B #$18
 RTS

************************************
*
* restore drive
*
************************************
restor STX xsave3
 BSR seldrv
 LDA A #$08
 ORA A stpbyt set steprate
 STA A fo2cmd
rstr01 LDA A fo4sta
 BEQ rstr01
 JSR gtfdst
 LDX xsave3
 BIT B #$40
 BNE sbf71
 CLC
 RTS

sbf71 LDA B #$0b
sbf73 CLC
 RTS

************************************
*
* select drive, X=FCB
*
************************************
seldrv STX xsave3
 LDA A 3,X
 CMP A #3
 BLS sbf7c
 CLR A *!!
sbf7c BSR fndtrk
 STA A curdrv
 LDA B fo2trk
 STA B 0,X
 BSR fndtrk
 LDA A 0,X
 STA A fo2trk
 BSR setlat
 LDX xsave3
 CLC
 RTS

fndtrk LDX #trktab
 LDA B curdrv
 BEQ sbf9f
sbf9b INX
 DEC B
 BNE sbf9b
sbf9f RTS

************************************
*
* chk ready, X=fcb
*
************************************
tready STX xsave3
 LDA A #4
trlp03 PSH A
 LDX #0
trlp01 LDA A fo2cmd chck controller
 BIT A #%10000000 notready
 BEQ trlp02
 DEX
 BNE trlp01
 PUL A
 DECA
 BNE trlp03
 LDA A #$80
 SEC
 BRA trlp04

trlp02 PUL A
 CLR A
trlp04 LDX xsave3
 TST A refresh result
 RTS
************************************
*
* set drive select latch
*
************************************
setlat PSH A
 PSH B
 LDA A drvset
 LDA B curdrv
 BEQ setl01
 ORA A #LASEL1
 BRA setl02
setl01 ORA A #%0000101
setl02 STA A fo4lat
 PUL B
 PUL A
 RTS

************************************
*
* Configuration table for environment
*
************************************
* FLEX2 COR
 if Versie=2
 org $B3E5
 fdb $f806 [] INCHE vector
 fdb $f80a [] OUTCH vector
 fdb $f004 ACIA device address
 fdb $0000 timer address
 fdb $f7c6 irq vector
 fdb $f7c4 swi vector
 fdb $f802 [] NXTCMD vector
 fdb $f7af+5 stack+pch offset
 endif

* FLEX3 COR
 if Versie=3
 org $A700 Remove printer spooler
 fcb $39,$39,$39,$39,$39,$39
 fcb $39,$39,$39,$39,$39,$39
 fcb $39,$39,$39,$39,$39,$39
 org $A71B
 fcb 0

 org $B3E5
 fdb $F804 $FD94 inch
 fdb $AE04 $B3E7 ihndlr
 fdb $F7C4 $B3E9 swivec
 fdb $F7C6 $B3EB irqvec
 fdb $AE04 $B3ED tmoff
 fdb $AE04 $b3ef tmon
 fdb $AE04 tmint
 fdb $F800 $F814 monitr
 fdb $AE04 $B3F5 tinit
 fdb $F808 $FDA6 incheck
 fdb $F80A $FDBA outch
 fdb $F806 $FD8E inche
 endif

 end COLDS
