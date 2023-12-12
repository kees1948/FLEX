                READING OLD 5.25" AND 8" DISKETTES WITH OLD DRIVES.


                              INTRO
  The storage.
All the material spent the first years in a normal climate temperature 
between winter evening and summer day from -10 to 30 degrees Celsius normal humidity
The last 15 years the storage was in a tropical climate temperature 
between winter evening and summer day from 18 to 38 degrees Celsius and 
humidity 85% and sometimes above 90%.

  The material.
The 5.25" diskettes are between 40 and 45 years old
The 8" diskettes are between 43 and 45 years old
The 5.25" SSDD 40 tracks FD200 drive is 45 years old
The 8" DSDD 77 tracks F208 is 43 years old
Two 5.25" DSDD 80 tracks drives come from a PC and are 20 years old

  The Hardware.
CPU09CMI plus CPU09FLX mini setup see FLXMIN.PDF
FLEX with cmi_io4B driver
GOTEK boot drive
GOTEK backup drive
8" drive and one of the 5.25" drives
2 straight FD cables
power supplies
see Setup.pdf

  The software.
The software used is a modified TSC COPY.CMD
The program GCOPY.CMD copies all files to a blank diskette
in the event of a read error it skips that file and continues with the next file
At the end the program indicates the number of missing files

A second run of GCOPY skips the files that have already been copied 
and tries to copy the files with errors
Very often copying is possible with the second or third run of GCOPY

Via the serial communication line and the 'L' function of the SBUG version for
the CPUXXCMI card, one can download the gcopy.s1 file and save it

+++MON    brings you to sbug
L start hex loader, completes on the S9 record
^P CD03
G         this should bring you back into flex
+++SAVE.LOW GCOPY.CMD,C100,C55F,C100



             WARNING: NOW MAKE SURE YOUR ARCHIVE DISKETTES ARE WRITE PROTECTED

                      Reading the 5.25" diskettes
Jumper and/or change the drives to work with the 09FLP card,
see 'Documents\Disk Drives' for the changes

Clean the track-0 sensor with compressed air
Clean the read head(s), see Setup.pdf for my cleaning tool
Clean the two head shafts with sewing machine oil and leave an oil film on the shafts

Start the FLEX system
Use CMISTEP to set the step rate for the old disk 
(FD200 - 30 msec) (PC disk 3 msec or double step 6 msec)
Now select a 80track DDDS GOTEK backup file, insert an old diskette
Do a DIR 1 first, if that works (If not, try reinserting the diskette)
then run GCOPY 1,2 if any files are missing, run it again
If after three runs you are still missing files or the DIR fails, 
try to recover them later using FLEX Diagnostics
Go for the next diskette
I had to clean the heads of the drives every time after processing 25 diskettes

                      Reading the 8" diskettes
On my Olivetti F802 drive the DIR command gave crc errors on many diskettes
So this drive needed some extra maintenance,
see 'Documents\Disk Drives\Refurbish F802.pdf'

Start the FLEX system
Use CMISTEP to set the step rate for all drives to 3msec
Now select a 77track DDDS GOTEK backup file, insert an old diskette
Do a DIR 3 first, if that works (If not, try reinserting the diskette)
then run GCOPY 3,2 if any files are missing, run it again
If after three runs you are still missing files or the DIR fails, 
try to recover them later using FLEX Diagnostics
Go for the next diskette
The heads remained clean while processing the 90 diskettes


                    The result after using GCOPY

5.25" diskettes, 3 not readable
5.25" diskettes, 164 processed
Files copied 2918
Files not copied 4
Files missing 4

8" diskettes, 4 not readable
8" diskettes, 86 processed
Files copied 5258
Files not copied 6, but these are also on other disks
Files missing 0

Not bad after so many years


Remark:
The 5.25" DSxx 40-track diskettes were copied using an 80-track dual step drive
12 pieces of 5.25" MiniFLEX 35-track diskettes were copied using the miniFLEX software


CdJ


