
        pag

*
* This is the header file for  cmi_io.t
*
 lib iospace.h

* controller type
WD2791  set     0
*
* control latch out (fo4lat)
*
LA_0SEL equ     %00000001       drive 0 select
LA_1SEL equ     %00000010       drive 1 select
LA_2SEL equ     %00000100       drive 2 select
LA_MOT  equ     %00001000       motor on
LA_SID  equ     %00010000       side select
LA_SDN  equ     %00100000       dens select 0=DD/1=SD
LA_8_5  equ     %01000000       8"/5" data rate 0=8"/1=5"
LA_HLT  equ     %10000000       toggle head load timer

*
* status buffer (fo4sta)
*
ST_INT  equ     %01000000       interrupt from FDC
ST_DRQ  equ     %10000000       data request from FDC

FD_ERR  equ     %10000000       error flag in response

*
* FDC Commands  (fo2cmd)
*
FD_RST  equ     $08             restore   (fastest step rate)
FD_SEK  equ     $18             seek      (fastest step rate)
FD_SRD  equ     $88             read sector data
FD_SWR  equ     $A8             write sector data
FD_MRD  equ     $98             read multiple sectors
FD_MWR  equ     $B8             write multiple sectors
FD_RTR  equ     $E4             read track
FD_WTR  equ     $F4             write track
FD_FI0  equ     $D0             force interrupt 0

*
* FDC status bits (fo2cmd)
*
FS_BUSY equ     $01             busy bit           [*]
FS_DRQ  equ     $02             data request bit   [2]
FS_IDX  equ     $02             index pulse        [1]
FS_TRK0 equ     $04             track zero present [1]
FS_LOST equ     $04             lost data          [2]
FS_CRC  equ     $08             CRC error detected [2]
FS_SKER equ     $10             seek error         [1]
FS_RNF  equ     $10             record not found   [2]
FS_HLD  equ     $20             head loaded        [1]
FS_WRP  equ     $40             write protect      [1]+W
FS_NRDY equ     $80             drive not ready    [*]
FS_RWOK equ     $D8             read/write OK mask [2]


*
* fdc hardware
*
fdcbas  equ     io_space+$0100  FDC controller
fdbasp  equ     fdcbas/256      for direct addressing

* subject to EOR4FDC
fo2cmd  equ     fdcbas          offset to command register
fo2trk  equ     fdcbas+1        offset to track register
fo2sec  equ     fdcbas+2        offset to sector register
fo2dat  equ     fdcbas+3        offset to data register
* not subject to EOR4FDC
fo4lat  equ     fdcbas+4        drive,side,density latch
fo4sta  equ     fdcbas+8        fdc status buffer

* HW debug select
fo2trg  equ     fdcbas+12       scope trigger @ this address
*
