                 Latest software versions
                       2024-11-23

Updates:
     * BOOTON   - 6.0 
       CMILINK  - 1.0
     * CMISTEP  - 6.0 
       DPACH    - 1
     * FDRIVES  - 6.0 
     * IDEFMT   - 6.0 
     * PLIST    - 6.0 
       RBRDSK   - 1.0
       RBRDSKM  - 1.0
     * VDISK    - 9.7  skip protection?
     * VIRTUAL  - 9.7  autorun VDISK

     * works on all drivers 


Latest drivers:
       CMI_IO4B - FLP only
       CMI_IDE3 - IDE & FLP

       CMI_IO6B - On CMISBUG6 independent IO, FLP only
       CMI_IDE6 - On CMISBUG6 independent IO, IDE & FLP 


Latest monitors:
       CMISBUG5 - 1.5 use the one after 2024-07-29

       CMISBUG6 - 1.6 will boot & run software IO independent


Notes:
    TSC 'DISKTEST.CMD' will work on the IDE formated disks
    if you use not more than 255 tracks.
    It will give a error message and ask to use 'FE/FF'.
    Just enter 'Y'.

    All utils version 6.0 and version 6 drivers are assembled for IO $F000,
    so they still works on the older drivers/monitors.
    When cmisbug6 is present they are IO independent.



CdeJ



 

