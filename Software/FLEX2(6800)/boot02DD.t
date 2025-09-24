* 6800 Bootstrap Loader

* Dens is set by NEWDISK
* Drive type number is set by MONITOR.

* System Equates
fdccmd EQU $F100 FDC COMMAND REG
fdctrk EQU $F101 FDC TRACK REG
fdcsec EQU $F102 FDC SECTOR REG
fdcdat EQU $F103 FDC DATA REG
fdclat EQU $F104 FDC DRIVE SELECT
fdcsta EQU $F108 FDC STATUS DRQ-INTRQ

 ORG $A100
start bra start1
 fdb fdccmd

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

mempth fcb $00
memptl fcb $00
bufdpt fdb $0000

* drive has restored, start loading
strt01 ldaa slnkh
 ldab slnkl
* seek
bsseek cmpa fdctrk
 beq bssl01
 staa fdcdat
 ldaa #$1b
 staa fdccmd
 pshb
 bsr bsstat
 pulb
bssl01 stab fdcsec
 CMP B sectrk
 BHI bsDS
 LDA B densf
 BRA BSSET
bsDS LDA B densf
 ORA B #%00010000 side 1
BSSET STA B fdclat
 ldaa #$8c read sector(s)
 staa fdccmd
* read sector in buffer
 ldab #0
 ldx #buffer
 bra brs001

bsrdat ldaa fdcdat
 staa 0,x
 inx
brs001 ldaa fdcsta data loop
 bmi bsrdat
 beq brs001
 ldaa fdccmd
 bita #%00011100
 bne start try again
* set buffer start
 ldx #buffer+4
updbfp stx bufdpt
 rts

* restore drive
start1 lds #bstack
 ldaa densf
 bita #%00100000 dens bit
 bne keep
 ldaa sectdd
 staa sectrk set DD
keep ldaa #$0B
 staa fdccmd
 bsr bsstat
 jsr strt01
nxtseg bsr gtbfch
 cmpa #2
 beq rdseg
 cmpa #$16
 bne nxtseg
* done, start it
 bsr gtbfch
 psha hibyte
 bsr gtbfch
 pulb hibyte
 psha lobyte
 pshb hibyte
 rts jump into it

bsstat ldab fdcsta
 bitb #%01000000
 beq bsstat
 ldaa fdccmd
 rts

gtbfch ldx bufdpt
 cpx #buffer+256
 beq getbf1
 ldaa 0,x
 inx
 bra updbfp

* buffer exhausted, read next sector
getbf1 ldx #buffer
 ldaa 0,x link
 ldab 1,x
 jsr bsseek
 bra gtbfch

* read next segment
rdseg bsr gtbfch
 psha
 bsr gtbfch
 pulb
 staa memptl
 stab mempth
 bsr gtbfch seg len
 tab
 beq nxtseg
wmemlp pshb
 bsr gtbfch
 pulb
 ldx mempth
 staa 0,x
 inx
 stx mempth
 decb
 bne wmemlp
 bra nxtseg

 org start+256

buffer rmb 256
bstack equ *+128

 end
