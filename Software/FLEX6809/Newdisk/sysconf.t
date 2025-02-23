* Select one IO area
io_space set $F000 IO start 2K rom
*io_space set $E000 IO start 4K rom

fdcbas equ io_space+$100
fdcDP set fdcbas/256
Cable set io_space+$07EE
IOpnt equ $FFE0
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

setdrv
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
 lbeq EXIT
 lda 1,x get the drive number
 sta Physical Drive number
noIDE
 ldb Logical restore drive number
