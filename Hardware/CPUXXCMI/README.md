The CPUXXCMI is an Eurocard sized board that provides a very basic computer environment, i.e. a FLEX system.

![CPUXXCMI](./20210629_084739a.jpg)

The board an accomodate either a MC68B02 or a MC68B09 CPU. (HD63X09)

It provides 60 KByte of contigeous RAM, 1 KByte of decoded IO space, 1 KByte of RAM, intended for monitor variables and IO buffers,  2 KByte of EEROM. Lastly, it has a serial port (ACIA), with jumper selectable baudrates, 300 baud up to 38400 baud.



The hardware supports the concept of a SUPERVISOR/USER mode. If this is used, no user program can access the IO and buffers directly. Access is with SWI type calls to te handler in the ROM.

The connections to the bus connector are fully buffered.
