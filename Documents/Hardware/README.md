[2022-11-08]

The wiring needed to get the FLEX system running is very minimal.

* wire 'C1A' to 'C1B' and 'C1C'  this runs #DIV5 (F100...F17F) over the bus
this serves for the IO select for te floppy interface


* wire 'C23A' to the same pin at the slot where the floppy interface will be,
this serves as the /RESET for the floppy interface


* wrie 'A30' to 'BCB' and 'B30' to 'CCB'
this runs the baudclock over the bus.


Adding an CPU09SR4: select a slot. Connect C23 at this slot to pin A28 
on the CPUXXCMI. This passes the DIV3# select, IO ports. (F008...F07F)


[note] 'C23A' is a Label on the backplane. C23A is a Pin on the bus connector
