Atmel contains the specifications for the Atmel ATF22V10 & ATF16V8 chips.

Translated from PALASM to CUPL. 

Check the 'CPUxxCMI_GAL_Table' to make a selection.

After programming there is a question?
Does the fuse table do what it is programmed to do?
In the TL866II+, import the CMI_xxx.lgc files in the 'Logic IC Test' screen and Test the chip!

For the CMI_2 use CMI_2.lgc (this is a full test, no don't care inputs).

For the CMI_1 use both CMI_1-P1.lgc and CMI_1-P2.lgc (this tests use don't care inputs).

Works for GAL...., ATF.... and PALCE... chips.
You can even test with a 5V or 3.3V supply.


Comments:
   The TL866II+ vector table can only handle 512 lines.

   Chips with many inputs must therefore be tested in parts
   and with a 'don't care' on inputs that are not used in the function under test.

   The test items are indicated with -P1, -P2 etc.

   If there is no -Px after the GAL name, it is a full test.

   A complete test vector table for the CMI_1 contains more than 8000 lines.


CdeJ


