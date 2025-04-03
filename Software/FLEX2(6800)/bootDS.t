* external label equates

*
fdccmd equ $f100
fdctrk equ $f101
fdcsec equ $f102
fdcdat equ $f103
fdclat equ $f104
fdcsta equ $f108

        org    $a100
* here start bootsector code
start jmp start1

* link to FLEX.sys
 fcb $00
densf  fcb    %01100001 drive select 5"/SD/S0
slnkh  fcb    $00 track
slnkl  fcb    $00 sector
* 5 inch settings
sectrk    fcb    10        5" SD
sectdd    fcb    18        5" DD
*
mempth fcb $00
memptl fcb $00

bufdpt fdb $0000

* drive has restored, start loading
strt01  ldaa     slnkh
        ldab     slnkl

skrdsc   bsr      bsseek   seek

        ldaa     #$8c      read sector(s)
        staa     fdccmd

* read sector in buffer
        ldab     #0
        ldx      #buffer
        bra      brs001

bsrdat  ldaa    fdcdat
        staa    0,x
        inx
brs001  ldaa    fdcsta    data loop
        bmi     bsrdat
        beq     brs001
*
        ldaa    fdccmd
        bita    #%00011100
        bne     start     try again
* set buffer start
        ldx     #buffer+4
updbfp  stx     bufdpt
        rts

* seek
bsseek  cmpa    fdctrk
        beq     bssl01
        staa    fdcdat
        ldaa    #$1b
        staa    fdccmd
        pshb
        bsr     bsstat
        pulb
bssl01  stab    fdcsec
 CMP B sectrk
 BHI bsDS
 LDA B densf
 BRA BSSET
bsDS LDA B densf
 ORA B #%00010000 side 1
BSSET STA B fdclat
        rts

bsstat  ldab    fdcsta
        bitb    #%01000000
        beq     bsstat
        ldaa    fdccmd
        rts

gtbfch  ldx     bufdpt
        cpx     #buffer+256
        beq     getbf1
        ldaa    0,x
        inx
        bra     updbfp

* buffer exhausted, read next sector
getbf1  ldx     #buffer
        ldaa    0,x      link
        ldab    1,x
        bsr     skrdsc
        bra     gtbfch

* restore drive
start1 lds #$F780
 ldaa #$0B
        staa    fdccmd
        bsr     bsstat
        jsr     strt01

nxtseg  bsr     gtbfch
        cmpa    #2
        beq     rdseg
        cmpa    #$16
        bne     nxtseg
* done, start it
        bsr     gtbfch
        psha              hibyte
        bsr     gtbfch
        pulb              hibyte
        psha              lobyte
        pshb              hibyte
        rts               jump into it

* read next segment
rdseg   bsr     gtbfch
        psha
        bsr     gtbfch
        pulb
        staa    memptl
        stab    mempth
        bsr     gtbfch  seg len
        tab
        beq     nxtseg
wmemlp  pshb
        bsr     gtbfch
        pulb
        ldx     mempth
        staa    0,x
        inx
        stx     mempth
        decb
        bne     wmemlp
        bra     nxtseg

        org     start+256

buffer equ *

 end
