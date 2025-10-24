 nam flex 6800 Bootstrap

* 6800 Bootstrap Loader

* Dens is set by NEWDISK
* Drive type number is set by MONITOR.

* System Equates
fdccmd EQU $0 FDC COMMAND REG
fdctrk EQU $1 FDC TRACK REG
fdcsec EQU $2 FDC SECTOR REG
fdcdat EQU $3 FDC DATA REG
fdclat EQU $4 FDC DRIVE SELECT
fdcsta EQU $8 FDC STATUS DRQ-INTRQ

* 6802 VECTOR table
vectors equ $fff8

 ORG $A100
start bra init
 fdb $6802

* default dens values
* 5" SSSD $61     8" SSSD $21
* 5" DSSD $61     8" DSSD $21
* 5" SSDD $41     8" SSDD $01
* 5" DSDD $41     8" DSDD $01
*
densf  fcb    %01100001 drive select 5"/SD/S0

* link to FLEX.sys
slnkh  fcb    $00 track
slnkl  fcb    $00 sector

* 5 inch settings
sectrk    fcb    10        5" SD
sectdd    fcb    18        5" DD
* 8 inch settings
*sectrk    fcb    15        8" SD
*sectdd    fcb    26        8" DD

FCB fdb fdccmd+$F100
mempth fcb $00
memptl fcb $00
bufdpt fdb $0000
Xtmp fdb $0000

* drive has restored, start loading
strt01 LDA A slnkh
 LDA B slnkl
* seek
bsseek ldx FCB
 CMP A fdctrk,X
 beq bssl01
 STA A fdcdat,X
 LDA A #$1b
 STA A fdccmd,X
 PSH B
 bsr bsstat
 PUL B
bssl01 STA B fdcsec,X
 CMP B sectrk
 BHI bsDS
 LDA B densf
 BRA BSSET
bsDS LDA B densf
 ORA B #%00010000 side 1
BSSET STA B fdclat,X
 LDA A #$8c read sector(s)
 STA A fdccmd,X
* read sector in buffer
 clrb
 ldx #buffer
 stx Xtmp
 ldx FCB
 bra brs001

bsstat LDA B fdcsta,X
 BIT B #%01000000
 beq bsstat
 LDA A fdccmd,X
 rts

bsrdat LDA A fdcdat,X
 ldx Xtmp
 STA A 0,X
 inx
 stx Xtmp
 ldx FCB
brs001 LDA A fdcsta,X data loop
 bmi bsrdat
 beq brs001
 LDA A fdccmd,X
 BIT A #%00011100
 bne start1 try again
* set buffer start
 ldx #buffer+4
updbfp stx bufdpt
 rts

* restore drive
init LDA A densf
 BIT A #%00100000 dens bit
 bne start1
 LDA A sectdd
 STA A sectrk set DD
start1 lds #bstack
 ldx FCB
 LDA A #$0B
 STA A fdccmd,X
 bsr bsstat
 jsr strt01
nxtseg bsr gtbfch
 CMP A #2
 beq rdseg
 CMP A #$16
 bne nxtseg
* done, start it
 bsr gtbfch
 PSH A hibyte
 bsr gtbfch
 PUL B hibyte
 PSH A lobyte
 PSH B hibyte
 rts jump into it

gtbfch ldx bufdpt
 cpx #buffer+256
 beq getbf1
 LDA A 0,X
 inx
 bra updbfp

* buffer exhausted, read next sector
getbf1 ldx #buffer
 LDA A 0,X link
 LDA B 1,X
 jsr bsseek
 bra gtbfch

* read next segment
rdseg bsr gtbfch
 PSH A
 bsr gtbfch
 PUL B
 STA A memptl
 STA B mempth
 bsr gtbfch seg len
 tab
 beq nxtseg
wmemlp PSH B
 bsr gtbfch
 PUL B
 ldx mempth
 STA A 0,X
 inx
 stx mempth
 DEC B
 bne wmemlp
 bra nxtseg

 org start+256

buffer rmb 256
bstack equ *+128

 end
