


              MS-DOS driver program for FLEX
                     User Manual



Date of this version: September 2023



1 - What can this program do ?
------------------------------
MS-DOS  for  FLEX is a software product which will let you read and write DOS
floppy disks in your FLEX system, almost as easily as  FLEX-formatted  disks.
With  MS-DOS,  transferring  FLEX  files  between  a  Flex system and your PC
becomes as simple as moving a floppy.  MS-DOS  takes  care of  all  the  file
management  tasks  on  the  DOS floppy, and lets a program running under Flex
access the DOS files as easily as if they were on a  Flex  floppy.  Once  you
have  loaded  MS-DOS,  you  can use DIR, COPY, LIST, MAP, or any Flex command
with the files on the DOS disk.


2 - Acknowledgement
-------------------
This program was originally written by  Klaus Dickhoerner,  for a  Eurocom II
system.  Klaus has  generously  granted the  right to freely  distribute this
program to the Flex Users Group.


3 - How does it work ?
----------------------
MS-DOS has two modes of operation, binary mode or text mode,  which  must  be
chosen  manually  by  the  user, depending on the type of file to be handled.
MS-DOS must be on your system disk. You run it just like any Flex command:

+++ MSDOS (drive number) (mode) [return]

in which (drive number) is the drive in which there is a DOS disk, and (mode)
is either T for text mode or B for binary mode. Upon  execution,  MSDOS  will
automatically:

- Run in low memory and read the formatting information from the DOS disk;
- Display the formatting information (number of sectors, number of FATs,
  size, etc..) found on the disk;
- Redirect the FLEX entry points so that when the FMS is called, if the drive
  selected is the one which contains the DOS disk, then the DOS file manager
  will be called instead of the Flex file manager. Any FMS calls which relate
  to the other drive(s) in your system will be processed normally by Flex.
- Then, MSDOS will move itself to high memory, where it will use
  approximately 12K bytes, and will decrement MEMEND accordingly. The program
  exits to $CD03 and the Flex prompt is displayed.
- From that time, the drive and its disk are managed as a DOS drive.

At any time, you can switch MSDOS between  binary  mode  and  text  mode,  by
simply  typing  MSDOS  B  or  MSDOS T at the Flex prompt. You can also remove
MSDOS completely, and return your system to normal FLEX operation by  running
MSDOS  without  supplying  any parameters, which will remove MSDOS altogether
and restore MEMEND to its original value.

Be careful to configure MS-DOS to binary or text mode, as a function  of  the
files  you  are  handling,  and  don't forget to switch it between modes when
needed.  For  example,  copying  a  file  with  MS-DOS  set  to   the   wrong
configuration will hopelessly corrupt the file !


4 - Hardware requirements and disk formats
------------------------------------------

 Disk type  | Tracks | Sides | WD 179X |  WD279X  |  Minimum 6809 speed  |
            |        |       |CLK input| 5/8 pin  |   Read   |   Write   |
------------+--------+-------+---------+----------+----------+-----------+
5.25"  180K     40       1      1 MHz        0       950 kHz    1050 kHz
5.25"  360K     40       2      1 MHz        0       950 kHz    1050 kHz
3.5"   720K     80       2      1 MHz        0       950 kHz    1050 kHz
3.5"  1.44M     80       2      2 MHz        1      1900 kHz    2100 kHz

All the above values assume a disk spindle rate of 300 RPM; the 5.25", 1.2 MB
DOS disks use a spindle rate of 360 rpm, and a data rate of 2.4 MHz which  is
above  the  2 MHz limit of the Western Digital 179X and 279X chips. Also note
that writing requires slightly more speed than reading; this is  due  to  the
different  service  time  requirements  of  the WD chips. MS-DOS also handles
double-stepping configurations, i.e. reading a 40-track disk in  an  80-track
drive.  Of  course,  the  usual  compatibility  limitations  related  to this
configuration are applicable here as in any  other  system.  (Author's  note:
theoretically,  an  80-track,  5.25" drive spinning at 300 rpm instead of the
standard 360 rpm should be able to handle 1.2 MB disks, with the WD179X  chip
running at 2 MHz. I have not tried this).


5 - Adapting to your system.
----------------------------
MS-DOS also uses its own error file, MSDOSERR.SYS, which  is  a  random  file
with  the  same  structure  as  the  ERRORS.SYS file which is already on your
system. Since the error codes are identical to those used by Flex, this  file
can replace ERRORS.SYS without affecting operation under Flex.



6 - Other utilities
-------------------
MSDOS comes with four additional utilities:
- MSDOS-CD (Change directory) which enables you to change from the current
  directory to another one (displayed as <directory name>.DIR by the usual
  DIR command of Flex). The syntax is +++MSDOS-CD <directory name>; of
  course the directory name must be specified with the drive number, if this
  is not the work drive.
- MSDOS-RD (Reset Directory) which points back to the root directory; the
  syntax is +++MSDOS-RD <drive number>.
- MSDOS-MD to make a new subdirectory on the MSDOS disk; the syntax is
  +++MSDOS-MD <directory name>.
- MSDOS-KD (Kill directory) which removes a directory from the MSDOS disk.
  Syntax: Same as above.


7 - Status 
    11-09-2023 Adapted for CPU09FLX.
    18-09-2023 Add MONITOR cable test.
    24-09-2023 Add "Physical Drive 40 track y/n" 
               (40track floppy in 80track drive set double step)

Tested on PC-cable (2 drives) and Straight-cable (4drives),
  cmisbug3.bin and cmi_io4B driver.

3.5" tests done with modified SONY MPF-420-1 drive (READY)
     Tested on 3.5" 1.44Mb MSDOS floppy, works 100%
     Tested on 3.5"  720Kb MSDOS floppy, works 100%

3.5" tests done on GoTek image.
     Tested MSDOS-1.44M.DSDD80-3.IMG, works 100%
     Tested MSDOS-720K.DSDD80-3.IMG, works 100%    

5.25" tests done on GoTek image.
      Tested MSDOS-360K.DSDD40-5.IMG, works 100%
      Tested MSDOS-180K.SSDD40-5.IMG, works 100%

5.25" tests done with TEAC FD-55GFR 149-U drive
      Tested on 5.25" 360K MSDOS floppy in 80 track drive.
      MSDOS show double step is on:
           DIR functions works.
           reading the files NOT, need still some work!
      At the moment no 180K / 360K floppys or DS 40 track drive available.



Notes:
     Don't use de FLEX delete command.
     Directory and File names must have MSDOS short format eg. 8.3
     FLEX commands will show sector info twice it value (256/512 bytes/sector)
     The date format is incorrect.
     For GoTek MSDOS images update the IMG.CFG
     If floppy or MSDOS drive changes first unload MSDOS.


CdeJ
