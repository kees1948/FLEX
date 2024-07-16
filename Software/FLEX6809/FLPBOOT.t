* DRIVE DESCCRIPTORS
*
* DISK   rmb 1 0=FLOPPY <>0 - WINCHESTER
* DRVNO  rmb 1 HARDWARE DRIVE NUMBER - Must be 0 or 1
* STEP   rmb 1 Not used was STEP RATE
* INITED rmb 1 0=NOT INITIALIZED
* SHIFT  rmb 1 Not used was <9 NUMBER OF SHIFTS >8 DIV
* HMASK  rmb 1 Not used was HEAD MASK
* OFFSET rmb 2 Partition OFFSET - number of 16MB offsets
* DRVSIZ rmb * DESCRIPTOR SIZE 

 org $DE22
DRVBEG equ * 
 FCB 0,0,0,0,0,0,0,0 Drive 0 - Floppy Drive 0 
 FCB 0,1,0,0,0,0,0,0 Drive 1 - Floppy Drive 1 
 FCB 1,0,0,0,0,0,0,0 Drive 2 - HD Partition 0 
 FCB 1,0,0,0,0,0,0,1 Drive 3 - HD Partition 1 
 