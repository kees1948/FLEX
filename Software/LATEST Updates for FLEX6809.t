             Latest software versions for FLEX6809
                       2026-11-17

Updates:
     * BOOTON   - 7.1 
       COPY99   - 4.3:0
       CMILINK  - 1.0
     * CMISTEP  - 6.0 
     + DIR-COPY - 0.1
       DISKEDIT - 6.0
       DISKTEST - 1.1:2
       DPACH    - 1
       EXAMINE  - 6.0
       EXTENDIR - 3.0
     * FORMAT5  - 5.2
     * FORMAT8  - 8.2
     * FDRIVES  - 6.1 
     * IDEFMT   - 6.1 
       PDEL     - 2
     * PLIST    - 6.0 
       RBRDSK   - 1.0
       RBRDSKM  - 1.0
       SHOWDIR  - 2.0
       TEST     - 6.0
       VALIDATE - 6.0
     * VDISK    - 9.8  
     * VIRTUAL  - 9.8  Change disk format for diagnostic tools.

     * works on all drivers 
     + JCP script


Latest drivers:
       CMI_IO4B - FLP only
       CMI_IDE3 - IDE & FLP

       CMI_IO7B - On CMISBUG6 and up independent IO, FLP only
       CMI_IDE7 - On CMISBUG6 and up independent IO, IDE & FLP 


Latest monitors:
       CMISBUG5 - 63x09, 1.5 use the one after 2024-07-29
       CMISBUG6 - 63x09, 1.6 will boot & run software IO independent

       cmiHbug6 - 63x09, H2.6 will boot & run software IO independent
       cmiMbug6 - 68x09, M2.6 will boot & run software IO independent


Notes:
    TSC 6808 Diagnostic v6.0:
        'TEST.CMD' will work on all 5" & 8" (v)disk formats, Vdisk & SD.
        'EXAMINE.CMD' will work on all 5" & 8" (v)disk formats, Vdisk & SD.
        'VALIDATE.CMD' will work on all 5" & 8" (v)disk formats.
    
    DISKEDIT v6.0 will work on all 5" & 8" (v)disk formats and Vdisk.

    DISKTEST will work on all 5" & 8" (v)disk formats and Vdisk & SD card 
             accept the "Maximum Track/Sector Read Was:" message.

    All utils version 6.0 and version 6 drivers are assembled for IO $F000,
    so they still works on the older drivers/monitors.
    When cmisbug6 and up is present they are IO independent.



CdeJ



 

