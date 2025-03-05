
* Select one IO area
io_space set $F000 IO start 2K rom
*io_space set $E000 IO start 4K rom

fdcbas equ io_space+$100
fdcDP set fdcbas/256
Cable set io_space+$07EE
IOpnt equ $FFE0
TSCvec equ $DE00
DRVPTR equ $DE20

SetDP clrb test monitor IO space
 lda #$E0
 cmpd IOpnt
 beq fndDP Set correct DP=$E1
 lda #$F0
 cmpd IOpnt
 beq fndDP Set correct DP=$F1
 lda io_space/256 ignore monitor
fndDP inca
 sta ldp DP to use

setcable
 ldd DRVPTR get driver version
 cmpa #$DE
 beq getcable old drivers no cable type
 cmpa #5 PCcable set in driver
 beq savcable
 lda #1
savcable sta Cable
getcable lda Cable
 sta lcable local Cable

idedrv
 lda Logical Drive number
 sta Physical drive number
 ldd DRVPTR
 cmpa #$DE
 bne noIDE
 cmpb #$34
 beq noIDE
 cmpb #$35
 beq noIDE
 ldx #DRVPTR+2
 lda Logical Drive number
 ldb #8 descriptor size
 mul
 abx
 lda 0,x
 cmpa #1 IDE drive, exit
 lbeq noFLP
 lda 1,x get the drive number
 sta Physical Drive number
 inc PhySet

noIDE equ * Virtual?
 ldd TSCvec
 cmpa #$7E  jmp or bsr
 beq continue Virtual not loaded 
 cmpb #$DF  Flex or Virtual
 bls continue Virtual not loaded 
 decb Virtual start point
 tfr b,a
 ldb #8 skip first FCB's
 tfr d,x
 adda #2 set vecgim Hi byte

diofnd
 cmpa 0,x+
 bne diofnd search entry
 leax -9,x  DIOVEC start
 stx DIOVEC
 ldb Logical drive nr
 lda #2 diovec entry size
 mul
 abx table entry
 ldd 0,x get Type and Physical number
 std typephy
 cmpa #0
 bne noFLP drive error
 tst PhySet
 bne continue skip if set by IDE
 stb Physical drive nr
 bra continue  

noFLP
 ldx #msgflp
 jmp EXIT2

continue
 ldb Logical restore drive number

+++