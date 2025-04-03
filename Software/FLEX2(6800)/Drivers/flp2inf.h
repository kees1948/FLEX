        pag

*
* This is the header file for gppflpdrvr.t
*

* controller type
WD2791  set     0
*
* control latch out (fo4lat)
*
LASEL0 equ     %00000001       drive 0 select
LASEL1 equ     %00000010       drive 1 select
LASEL2 equ     %00000100       drive 2 select
LAMOT  equ     %00001000       motor on
LASID  equ     %00010000       side select
LASDN  equ     %00100000       dens select 0=DD/1=SD
LA85   equ     %01000000       8"/5" data rate 0=8"/1=5"
LAHLT  equ     %10000000       toggle head load timer

*
* status buffer (fo4sta)
*
STINT  equ     %01000000       interrupt from FDC
STDRQ  equ     %10000000       data request from FDC

FDERR  equ     %10000000       error flag in response

*
* FDC Commands  (fo2cmd)
*
FDRST  equ     $08             restore   (fastest step rate)
FDSEK  equ     $18             seek      (fastest step rate)
FDSRD  equ     $80             read sector data
FDSWR  equ     $A0             write sector data
FDMRD  equ     $90             read multiple sectors
FDMWR  equ     $B0             write multiple sectors
FDRTR  equ     $E0             read track
FDWTR  equ     $F0             write track
FDFI0  equ     $D0             force interrupt 0

*
* FDC status bits (fo2cmd)
*
FSBUSY equ     $01             busy bit           [*]
FSDRQ  equ     $02             data request bit   [2]
FSIDX  equ     $02             index pulse        [1]
FSTRK0 equ     $04             track zero present [1]
FSLOST equ     $04             lost data          [2]
FSCRC  equ     $08             CRC error detected [2]
FSSKER equ     $10             seek error         [1]
FSRNF  equ     $10             record not found   [2]
FSHLD  equ     $20             head loaded        [1]
FSWRP  equ     $40             write protect      [1]+W
FSNRDY equ     $80             drive not ready    [*]
FSRWOK equ     %11011000       read/write OK mask [2]


*
* fdc hardware
*
fdcbas  equ     $f100           FDC controller
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
