
* Select one IO area
io_space set $F000 IO start 2K rom
*io_space set $E000 IO start 4K rom

 IF io_space=$F000
dr_shift set $E000 no need to skip IO area
 ELSE
dr_shift set $E400 shift part driver after IO
 ENDIF

