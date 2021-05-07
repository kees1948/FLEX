The IO decoding on the CPUXXCMI is done in regions. The 09FLP floppy interface can be applied with a
minimalistic version of the CPU09FLX board. Without further decoding the floppy hardware will be
in the F180-F1FF area multiple times. If that is not desired, add the GAL logic to the CPU09FLX board,
this way you can narrow down the final addresses.
