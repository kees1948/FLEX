 name CMI_BUG  (18) cpuxxcmi monitor 1.1
 opt pag
 pag
* monitor program for the cpu09xxx system
* products cpuxxcmi board as commented by....
* allen clark            wallace watson
* 2502 regal oaks lane   4815 east 97th ave.
* lutz, fla. 33549       temple terrace, fla. 33617
* ph. 813-977-0347       ph. 813-985-1359
* modified to sbug09 ver 1.8 by:  randy jarrett
*                                 2561 nantucket dr apt. e
*                                 atlanta, ga  30345
*                                 ph. 404-320-1043
*
* again modified for CPUXXCMI and due process
* small bug fixes (2021-09-26)
*                  by :  kees schoenmakers
*                        de rijf 20
*                        5258jb, berlicum
*
* correction bootmodes (2021-11-06) CS
*
* boot for straight cable (2023-03-03) CAJ

*
*       *** commands ***
* control a   = alter the "a" accumulator
* control b   = alter the "b" accumulator
* control e   = alter the "e" accumulator
* control f   = alter the "f" accumulator
* control c   = alter the condition code register
* control d   = alter the direct page register
* control p   = alter the program counter
* control u   = alter user stack pointer
* control x   = alter "x" index register
* control y   = alter "y" index register
* b hhhh      = set breakpoint at location $hhhh
* u           = boot a 09flp 5/8 inch floppy Straight cable
* e ssss-eeee = examine memory from starting address ssss
*              -to ending address eeee.
* g           = continue execution from breakpoint or swi
* l           = load tape
* m hhhh      = examine and change memory location hhhh
* p ssss-eeee = punch tape, start ssss to end eeee addr.
* q ssss-eeee = test memory from ssss to eeee
* r           = display register contents
* s           = display stack from ssss to $7fc0
* x           = remove all breakpoints

romstk  equ     $f780
vectors equ     $fff0

        org     $f7c0

stack   rmb     2       top of internal stack / user vector
trap    rmb     2
swi3    rmb     2       software interrupt vector #3
swi2    rmb     2       software interrupt vector #2
firq    rmb     2       fast interrupt vector
irq     rmb     2       interrupt vector
swi     rmb     2       software interrupt vector
svcvo   rmb     2       supervisor call vector orgin
svcvl   rmb     2       supervisor call vector limit
cport   rmb     2       re-vectorable control port
echo    rmb     1       echo flag
flpspd  rmb     1       floppy data speed
bptbl   rmb     24      breakpoint table base addr
cable   rmb     1       cable type


acias   equ     $f004   control port
*
fdccmd  equ     $f100   command register
fdctrk  equ     $f101   drive register
fdcsec  equ     $f102   sector register
fdcdat  equ     $f103   data register
fdcsel  equ     $f104   drive select
fdcsta  equ     $f108   drive status
*
D5INCH  equ     %01000000
DSINGLE equ     %00100000

        org     $f800
        fdb     monitor         00
        fdb     nextcmd         02
        fdb     inch            04 input char (raw)
        fdb     inche           06 input and echo
        fdb     inchek          08 input check (if char is available)
        fdb     outch           0A output char (raw)
        fdb     pdata           0C
        fdb     pcrlf           0E
        fdb     pstrng          10
        fdb     lra             12

* monitor
* vector address string is.....
* $f8a1-$f8a1-$f8a1-$f8a1-$f8a1-$fab0-$ffff-$ffff
monitor ldx     #ramvec point to vector addr. string
        ldy     #trap   point to ram vector location
        ldb     #$10    bytes to move = 16
loopa   lda     ,x+     get vector byte
        sta     ,y+     put vectors in ram / $f7c0-$f7cf
        decb            subtract 1 from number of bytes to move
        bne     loopa   continue until all vectors moved
        clr     flpspd  set for 8", DD

* contents     from         to      function
*  $f8a1       $fe40      $f7c2     user-v
*  $f8a1       $fe42      $f7c4     swi3-v
*  $f8a1       $fe44      $f7c6     swi2-v
*  $f8a1       $fe46      $f7c8     firq-v
*  $f8a1       $fe48      $f7ca     irq-v
*  $fab0       $fe4a      $f7cc     swi-v
*  $ffff       $fe4c      $f7ce     svc-vo
*  $ffff       $fe4e      $f7d0     svc-vl

        ldx     #acias  get control port addr.
        stx     cport   store addr. in ram
        lbsr    xbkpnt  clear outstanding breakpoints
        ldb     #14     clear 14 bytes on stack
clrstk  clr     ,-s
        decb
        bne     clrstk
        leax    monitor,pcr  set pc to sbug-e entry
        stx     12,s    on stack
        lda     #$d0    preset condition codes on stack
        sta     ,s
        tfr     s,u
        lbsr    aciniz  initialize control port
        ldx     #msg1   point to 'sbug 1.8' message
        lbsr    pdata   print msg
* report 60K of RAM, (I did not check it though)
        lda     #$60    fixed size for cpuxxcmi
        lbsr    out2h   output hex byte as ascii
        ldx     #msg2   point to msg 'k' cr/lf + 3 nulls
        lbsr    pdata   print msg

***** nextcmd *****
nextcmd ldx     #msg3   point to msg ">"
        lbsr    pstrng  print msg
        lbsr    inch    get one char. from terminal
        anda    #$7f    strip parity from char.
        cmpa    #$0d    is it carriage return ?
        beq     nextcmd if cr then get another char.
        tfr     a,b     put char. in "b" accum.
        cmpa    #$20    is it control or data char ?
        bge     prtcmd  if cmd char is data, prnt it
        lda     #'^     else cntrl char cmd so...
        lbsr    outch   print "^"
        tfr     b,a     recall cntrl cmd char
        adda    #$40    convert it to ascii letter
prtcmd  lbsr    outch   prnt cmd char
        lbsr    out1s   prnt space
        cmpb    #$60
        ble     nxtch0
        subb    #$20

***** do table lookup *****
*   for command functions
nxtch0  ldx     #jmptab point to jump table
nxtchr  cmpb    ,x+     does command match table entry ?
        beq     jmpcmd  branch if match found
        leax    2,x     point to next entry in table
        cmpx    #tabend reached end of table yet ?
        bne     nxtchr  if not end, check next entry
        ldx     #msg4   point to msg "what?"
        lbsr    pdata   print msg
        bra     nextcmd if no match, prmpt for new cmd
jmpcmd  jsr     [,x]    jump to command routine
        bra     nextcmd prompt for new command
*
* "g" go or continue
go      tfr     u,s
rti     rti
* "r" display registers
regstr  ldx     #msg5   point to msg " - "
        lbsr    pstrng  print msg
        lbsr    prtsp
        lbsr    prtus
        lbsr    prtdp
        lbsr    prtix
        lbsr    prtiy
        ldx     #msg5   point to msg " - "
        lbsr    pstrng  print msg
        lbsr    prtcc
        lbsr    prta
        lbsr    prtb
        lbsr    prte
        lbsr    prtf
        lbra    prtpc

* alter "pc" program counter
altrpc  lbsr    prtpc   print msg " pc = "
        lbsr    out1s   output space
        lbsr    in1adr  get new contents for "pc"
        bvs     altpcd  exit if invalid hex
        stx     12,u    poke in new contents
altpcd  rts

* alter "u" user stack pointer
altru   lbsr    prtus   print msg " us = "
        lbsr    out1s   output space
        lbsr    in1adr
        bvs     altud
        stx     10,u
altud   rts

* alter "y" index register
altry   lbsr    prtiy   print msg " iy = "
        lbsr    out1s   output space
        lbsr    in1adr
        bvs     altyd
        stx     8,u
altyd   rts

* alter "x" index register
altrx   lbsr    prtix   print msg " ix = "
        lbsr    out1s   output space
        lbsr    in1adr
        bvs     altxd
        stx     6,u
altxd   rts

* alter "dp" direct page register
altrdp  lbsr    prtdp   print msg " dp = "
        lbsr    out1s   output space
        lbsr    byte    input byte (2 hex char)
        bvs     altdpd
        sta     5,u
altdpd  rts

altre   lbsr    prte
        lbsr    out1s
        lbsr    byte
        bvs     alted
        sta     3,u
alted   rts

altrf   lbsr    prtf
        lbsr    out1s
        lbsr    byte
        bvs     altfd
        sta     4,u
altfd   rts

* alter "b" accumulator
altrb   lbsr    prtb    print msg " b = "
        lbsr    out1s   output space
        lbsr    byte    input byte (2 hex char)
        bvs     altbd
        sta     2,u
altbd   rts

* alter "a" accumulator
altra   lbsr    prta    print msg " a = "
        lbsr    out1s   output space
        lbsr    byte    input byte (2 hex char)
        bvs     altad
        sta     1,u
altad   rts

* alter "cc" register
altrcc  lbsr    prtcc   print msg " cc: "
        lbsr    out1s   output space
        lbsr    byte    input byte (2 hex char)
        bvs     altccd
        ora     #$80    sets "e" flag in print list
        sta     ,u
altccd  rts

***** "m" memory examine and change *****
memchg  lbsr    in1adr  input address
        bvs     chrtn   if not hex, return
        tfr     x,y     save addr in "y"
memc2   ldx     #msg5   point to msg " - "
        lbsr    pstrng  print msg
        tfr     y,x     fetch address
        lbsr    out4h   print addr in hex
        lbsr    out1s   output space
        lda     ,y      get contents of current addr.
        lbsr    out2h   output contents in ascii
        lbsr    out1s   output space
        lbsr    byte    loop waiting for operator input
        bvc     change  if valid hex go change mem. loc.
        anda    #%11011111 undo 'toupper'
        cmpa    #8      is it a backspace (cntrl h)?
        beq     memc2   prompt operator again
        cmpa    #$18    is it a cancel (cntrl x)?
        beq     memc2   prompt operator again
        cmpa    #'^     is it an up arrow?
        beq     back    display previous byte
        cmpa    #$d     is it a cr?
        bne     forwrd  display next byte
chrtn   rts             exit routine
change  sta     ,y      change byte in memory
        cmpa    ,y      did memory byte change?
        beq     forwrd
        lbsr    out1s   output space
        lda     #'?     load question mark
        lbsr    outch   print it
forwrd  leay    1,y     point to next higher mem location
        bra     memc2   print location & contents
back    leay    -1,y    point to last mem location
        bra     memc2   print location & contents

* "s" display stack
* hex-ascii display of current stack contents from
* current stack pointer to internal stack limit.
disstk  lbsr    prtsp   print current stack pointer
        tfr     u,y
        ldx     #stack  load internal stack as upper limit
        leax    -1,x    point to current stack
        bra     mdump1  enter memory dump of stack contents
* "e" dump memory for examine in hex and ascii
* after calling 'in2adr' lower address in y-reg.
*                        upper address in x-reg.
* if hex addresses are invalid (v)=1.
memdump lbsr    in2adr  input address boundries
        bvs     edprtn  new command if illegal hex
mdump1  pshs    y       compare lower to upper bounds
        cmpx    ,s++    lower bounds > upper bounds?
        bcc     ajdump  if not, dump hex and ascii
edprtn  rts

* adjust lower and upper address limits
* to even 16 byte boundries.
* if lower addr = $4532
* lower bounds will be adjusted to = $4530.
* if upper addr = $4567
* upper bounds will be adjusted to = $4570.
* enter with lower address in x-reg.
*           -upper address on top of stack.
ajdump  tfr     x,d     get upper addr in d-reg
        addd    #$10    add 16 to upper address
        andb    #$f0    mask to even 16 byte boundry
        pshs    a,b     save on stack as upper dump limit
        tfr     y,d     get lower address in d-reg
        andb    #$f0    mask to even 16 byte boundry
        tfr     d,x     put in x-reg as lower dump limit
nxtlin  cmpx    ,s      compare lower to upper limit
        beq     skpdmp  if equal skip hex-ascii dump
        lbsr    inchek  check for input from keyboard
        beq     edump   if none, continue with dump
skpdmp  leas    2,s     readjust stack if not dumping
        rts

* print 16 hex bytes followed by 16 ascii characters
* for each line throughout address limits.
edump   pshs    x       push lower addr limit on stack
        ldx     #msg5   point to msg " - "
        lbsr    pstrng  print msg
        ldx     ,s      load lower addr from top of stack
        lbsr    out4h   print the address
        lbsr    out2s   print 2 spaces
        ldb     #$10    load count of 16 bytes to dump
eloop   lda     ,x+     get from memory hex byte to print
        lbsr    out2h   output hex byte as ascii
        lbsr    out1s   output space
        decb            decrement byte count
        bne     eloop   continue til 16 hex bytes printed

* print 16 ascii characters
* if not printable or not valid
* ascii print a period (.)
        lbsr    out2s   2 spaces
        ldx     ,s++    get low limit frm stack - adj stack
        ldb     #$10    set ascii char to print = 16
edpasc  lda     ,x+     get character from memory
        cmpa    #$20    if less than $20, non-printable?
        bcs     period  if so, print period instead
        cmpa    #$7e    is it valid ascii?
        bls     prasc   if so print it
period  lda     #'.     load a period (.)
prasc   lbsr    outch   print ascii character
        decb            decrement count
        bne     edpasc
        bra     nxtlin

***** "q" memory test *****
memtst  clr     ,-s     clear byte on stack
        clr     ,-s     clear another byte
        lbsr    in2adr  get begin(y) & end(x) addr. limits
        pshs    x,y     save addresses on stack
        bvs     adjsk6  exit if not valid hex
        cmpx    2,s     compare begin to end addr.
        bcs     adjsk6  exit if begin > end addr.
        lbsr    out1s   output space
memset  tfr     y,d     put begin addr. in 'd'-accum.
        addd    4,s     add pass count to begin addr
        pshs    b       add ls byte to ms byte of begin addr
        adda    ,s+
        sta     ,y+     save this data byte at begin addr
        cmpy    ,s      compare end to begin addr
        bcs     memset  if begin lower, continue to set memory
        ldy     2,s     reload begin address
test1   tfr     y,d     put begin addr in 'd'-acc.
        addd    4,s     add pass count to address
        pshs    a       add ms byte to ls byte of address
        addb    ,s+
        eorb    ,y+     ex-or this data with data in memory loc.
        beq     gudpas  if (z) set, memory byte ok
        ldx     #msg5   point to msg " - "
        lbsr    pstrng  print msg
        leax    -1,y    get error address in x-reg
        lbsr    out4h   output it
        pshs    x       push error addr on stack
        ldx     #msg8   point to msg " =>"
        lbsr    pdata   print msg
        puls    x       pop error addr from stack
        lbsr    lra     get physical addr from lra
        lbsr    xascii  output extended 4 bits of physical addr
        lbsr    out4h   output ls 16 bits of physical addr
        ldx     #msg6   point to msg ", pass "
        lbsr    pdata   print msg
        ldx     4,s     load pass count
        lbsr    out4h   output it
        ldx     #msg7   point to msg ", bits in error
        lbsr    pdata   print msg
        tfr     b,a     get error byte into a-acc
        ldx     #msg9   point to msg "76543210"
        lbsr    biasci  output in binary/ascii format
        lbsr    inchek  check for input from keyboard $fa56
        bne     adjsk6  if so, exit memory test
gudpas  cmpy    ,s      compare end addr to begin addr
        bcs     test1
        lda     #'+     get "pass" symbol if memory pass ok
        lbsr    outch   output symbol to terminal
        lbsr    inchek  input from keyboard?
        bne     adjsk6  if so, exit memory test
        ldy     2,s     load begin address
        inc     5,s     increment ls byte of pass count
        bne     memset  if not zero, set next memory byte
        inc     4,s     increment ms byte of pass count
        bne     memset  done with 65,535 passes of memory?
adjsk6  leas    6,s     adj stack pointer by 6
        rts

***** "b" set breakpoint *****
brkpnt  lbsr    in1adr  get breakpoint address
        bvs     exitbp  exit if invalid hex addr.
        cmpx    #stack  address illegal if >= stack
        bcc     bperr   if error print (?), exit
        pshs    x       push bp address on stack
        ldx     #$ffff  load dummy addr to test bp table
        bsr     bptest  test bp table for free space
        puls    x       pop bp address from stack
        beq     bperr   (z) set, out of bp table space
        lda     ,x      get data at breakpoint address
        cmpa    #$3f    is it a swi?
        beq     bperr   if swi already, indicate error
        sta     ,y+     save data byte in bp table
        stx     ,y      save bp address in bp table
        lda     #$3f    load a swi ($3f)
        sta     ,x      save swi at breakpoint address
exitbp  rts
*  indicate error setting breakpoint
bperr   lbsr    out1s   output space
        lda     #'?     load (?), indicate breakpoint error
        lbra    outch   print "?"

*** "x" clear outstanding breakpoints ***
xbkpnt  ldy     #bptbl  point to breakpoint table
        ldb     #8      load breakpoint counter
xbplp   bsr     rplswi  remove used entry in bp table
        decb            decrement bp counter
        bne     xbplp   end of breakpoint table?
        rts

***** swi entry point *****
swie    tfr     s,u     transfer stack to user pointer
        ldx     12,u    load pc from stack into x-reg
        leax    -1,x    adjust addr down 1 byte.
        bsr     bptest  find breakpoint in bp table
        beq     regpr   if found, replace data at bp addr
        stx     12,u    save breakpoint addr in stack
        bsr     rplswi  go replace swi with original data
regpr   lbsr    regstr  go print registers
        lbra    nextcmd get next command
rplswi  ldx     1,y     load bp address from bp table
        cmpx    #stack  compare to top available user memory
        bcc     ffstbl  go reset table entry to $ff's
        lda     ,x      get data from bp address
        cmpa    #$3f    is it swi?
        bne     ffstbl  if not, reset table entry to $ff's
        lda     ,y      get original data from bp table
        sta     ,x      restore data at bp address
ffstbl  lda     #$ff    load $ff in a-acc
        sta     ,y+     reset breakpoint table data to $ff's
        sta     ,y+     reset breakpoint table addr to $ff's
        sta     ,y+
        rts

** search breakpoint table for match **
bptest  ldy     #bptbl  point to breakpoint table
        ldb     #8      load breakpoint counter
fndbp   lda     ,y+     load data byte
        cmpx    ,y++    compare address, is it same?
        beq     bpadj   if so, adjust pointer for table entry
        decb            if not, decrement breakpoint counter
        bne     fndbp   and look for next possible match
        rts
bpadj   leay    -3,y    move pointer to begin of bp entry
        rts

***** "u" minidisk boot *****
minboot lda     #%00000001 drive 0
        sta     cable set flag formatn.cmd
        tst     fdcsel
minbo1  ldb     flpspd     8/5"
        andb    #D5INCH
        asrb    move bit6 to bit2
        asrb
        asrb
        asrb
        ldx     #MODTAB  4 byte entry
        abx
        jsr     pdata
* drive select 1, PC compatible
        lda     flpspd
        ora     #DSINGLE      FLEX has SD boot only
        ora     cable   drive 0
        sta     fdcsel  select drive 0
* delay before issuing restore command
        ldb     #3
        ldx     #0
loop    leax    1,x
        tst     fdcsel  keep active
        cmpx    #0
        bne     loop
        decb
        bne     loop
        ldb     fdccmd
        bmi     loop9
*
        lda     #$09    *load head, verify, 12msec/step
        sta     fdccmd  issue restore command
loop1   ldb     fdcsta
        aslb                fdc INT
        bpl     loop1   loop until thru
*
        lda     #1
        sta     fdcsec  set sector register to one
        lda     #$88    load head, delay 10msec,
        sta     fdccmd  and read single record
        ldx     #$c100  where to put 1st bootsector
        bra     loop3
loop2   lda     fdcdat
        sta     ,x+
loop3   ldb     fdcsta  fetch status
        bmi     loop2   fdc DRQ
        beq     loop3   fdc INT
        ldb     fdccmd
        bitb    #%00011100 crc error or lost data?
        beq     loop4
*
        lda     flpspd
        eora    #D5INCH
        sta     flpspd
        bita    #D5INCH
        bne     minbo1
*
loop9   rts

loop4   ldx     #$c100   start boot code
        stx     12,u
        tfr     u,s
        rti

lra     clra
        rts

***** "l" load mikbug tape *****
load    lda     #$11    load 'dc1' cass. read on code
        lbsr    outch   output it to terminal port
        clr     echo    turn off echo flag
load1   lbsr    echon   input 8 bit byte with no echo
load2   ora     #$20    make lower
        cmpa    #'s     is it an "s", start character ?
        bne     load1   if not, discard and get next char.
        lbsr    echon
        cmpa    #'9     is it a "9" , end of file char ?
        beq     load21  if so, exit load
        cmpa    #'1     is it a "1" , file load char ?
        bne     load2   if not, look for start char.
        lbsr    byte    input byte count
        pshs    a       push count on stack
        bvs     loderr  (v) c-code set, illegal hex
        lbsr    in1adr  input load address
        bvs     loderr  (v) c-code set, addr not hex
        pshs    x       push addr on stack
        ldb     ,s+     load msb of addr as checksum byte
        addb    ,s+     add lsb of addr to checksum
        addb    ,s      add byte count byte to checksum
        dec     ,s      decrement byte count 2 to bypass
        dec     ,s      address bytes.
load10  pshs    b       push checksum on stack
        lbsr    byte    input data byte (2 hex char)
        puls    b       pop checksum from stack
        bvs     loderr  (v) set, data byte not hex
        pshs    a       push data byte on stack
        addb    ,s+     add data to checksum, auto inc stack
        dec     ,s      decrement byte count 1
        beq     load16  if byte count zero, test checksum
        sta     ,x+     save data byte in memory
        bra     load10  get next data byte
loderr  clrb            error condition, zero checksum
load16  puls    a       adjust stack (remove byte count)
        cmpb    #$ff    checksum ok?
        beq     load    if so, load next line
        lda     #'?     load (?) error indicator
        lbsr    outch   output it to terminal
load21  com     echo    turn echo on
        lda     #$13    load 'dc3' cass. read off code
        lbra    outch   output it
***** "p" punch mikbug tape *****
punch clr  ,-s clear reserved byte on stack
 lbsr in2adr get begin and end address
 pshs x,y save addresses on stack
 bvs  punext (v) c-code set, exit punch
 cmpx 2,s compare begin to end addr
 bcs  punext if begin greater than end, exit punch
 leax 1,x increment end address
 stx  ,s store end addr on stack
 lda  #$12 load 'dc2' punch on code
 lbsr outch output it to terminal
punch2 ldd  ,s load end addr in d-acc
 subd 2,s subtract begin from end
 beq  punch3 same, punch 32 bytes default
 cmpd #$20 less than 32 bytes?
 bls  punch4 punch that many bytes
punch3 ldb  #$20 load byte count of 32.
punch4 stb  4,s store on stack as byte count
 ldx  #msg20 point to msg "s1"
 lbsr pstrng print msg
 addb #3 add 3 bytes to byte count
 tfr  b,a get byte count in a-acc to punch
 lbsr out2h output byte count
 ldx  2,s load begin address
 lbsr out4h punch address
 addb 2,s add addr msb to checksum
 addb 3,s add addr lsb to checksum
punchl addb ,x add data byte to checksum
 lda  ,x+ load data byte to punch
 lbsr out2h output data byte
 dec  4,s decrement byte count
 bne  punchl not done, punch next byte
 comb 1's compliment checksum byte
 tfr  b,a get it in a-acc to punch
 lbsr out2h output checksum byte
 stx  2,s save x-reg in stack as new punch addr
 cmpx ,s compare it to end addr
 bne  punch2      $fcb5 punch not done, cont.
punext lda  #$14 load 'dc4' punch off code
 lbsr outch output it
 leas 5,s readjust stack pointer
 rts
prtsp ldx  #msg10 point to msg "sp="
 lbsr pdata  print msg
 tfr  u,x
 lbra out4h
prtus ldx  #msg12 point to msg "us="
 lbsr pdata  print msg
 ldx  10,u
 lbra out4h
prtdp ldx  #msg15 point to msg "dp="
 lbsr pdata  print msg
 lda  5,u
 lbra out2h output hex byte as ascii
prtix ldx  #msg14 point to msg "ix="
 lbsr pdata  print msg
 ldx  6,u      $fce6
 lbra out4h
prtiy ldx  #msg13 point to msg "iy="
 lbsr pdata  print msg
 ldx  8,u
 lbra  out4h
prtpc ldx  #msg11 point to msg "pc="
 lbsr pdata  print msg
 ldx  12,u
 lbra  out4h
prta ldx  #msg16 point to msg "a="
 lbsr pdata  print msg
 lda  1,u
 lbra out2h output hex byte as ascii
prtb ldx  #msg17 point to msg "b="
 lbsr pdata  print msg
 lda  2,u
 bra out2h output hex byte as ascii
prte ldx #msg21
 lbsr pdata
 lda 3,u
 bra out2h
prtf ldx #msg22
 lbsr pdata
 lda 4,u
 bra out2h
prtcc ldx  #msg18 point to msg "cc:"
 lbsr pdata  print msg
 lda  ,u
 ldx  #msg19 point to msg "efhinzvc"
 bra biasci output in binary/ascii format
* the following routine loops waiting for the
* operator to input two valid hex addresses.
* the first address input is returned in "iy".
* the second is returned in "ix". the "v" bit
* in the c-code reg. is set if an invalid hex
* address is input.
in2adr bsr in1adr get first address
 bvs nothex exit if not valid hex
 tfr  x,y save first addr. in "iy"
 lda #'-
 lbsr outch print " - "
* the following routine loops waiting for the
* operator to input one valid hex address. the
* address is returned in the "x" register.
in1adr bsr byte input byte (2 hex char)
 bvs nothex exit if not valid hex
 tfr  d,x
 bsr byte input byte (2 hex char)
 bvs nothex
 pshs x
 sta  1,s
 puls x
 rts
***** input byte (2 hex char.) *****
byte bsr inhex get hex left
 bvs nothex exit if not valid hex
 asla
 asla
 asla shift into left nibble
 asla
 tfr  a,b put hexl in "b"
 bsr inhex get hex right
 bvs nothex exit if not valid hex
 pshs b push hexl on stack
 adda ,s+ add hexl to hexr and adj. stk
 rts return with hex l&r in "a"
inhex bsr echon input ascii char.
 cmpa #'0 is it > or = "0" ?
 bcs nothex if less it ain't hex
 cmpa #'9 is it < or = "9" ?
 bhi inhexa if > maybe it's alpha
 suba #$30 ascii adj. numeric
 rts
inhexa ora #$20 make it lower
 cmpa #'a is it > or = "a"
 bcs nothex if less it ain't hex
 cmpa #'f is it < or = "f" ?
 bhi nothex
 suba #$57 ascii adj. alpha
 rts
nothex orcc #2 set (v) flag in c-codes register
 rts
out4h pshs x push x-reg. on the stack
 puls a pop ms byte of x-reg into a-acc.
 bsr outhl output hex left
 puls a pop ls byte of x-reg into a-acc.
outhl equ *
out2h pshs a save it back on stack
 lsra convert upper hex nibble to ascii
 lsra
 lsra
 lsra
 bsr xascii print hex nibble as ascii
outhr puls a convert lower hex nibble to ascii
 anda #$0f strip left nibble
xascii adda #$30 ascii adj
 cmpa #$39 is it < or = "9" ?
 ble  outc if less, output it
 adda #7 if > make ascii letter
outc bra  outch output char
* binary / ascii --- this routine
* outputs a byte in enhanced
* binary format. the enhancement
* is done by substituting ascii
* letters for the ones in the byte.
* the ascii enhancement letters
* are obtained from the string
* pointed to by the index reg. "x".
biasci pshs a save "a" on stack
 ldb  #8 preset loop# to bits per byte
outba lda ,x+ get letter from string
 asl  ,s test byte for "1" in b7
 bcs prtba if one print letter
 lda #'- if zero print "-"
prtba bsr outch print it
 bsr out1s print space
 decb sub 1 from #bits yet to print
 bne outba
 puls a
 rts
* print string preceeded by a cr & lf.
pstrng bsr pcrlf print cr/lf
 bra  pdata  print string pointed to by ix
* pcrlf
pcrlf pshs x save ix
 ldx  #msg2+1  point to msg cr/lf + 3 nuls
 bsr pdata  print msg
 puls x restore ix
 rts
print bsr outch
* pdata
pdata lda  ,x+ get 1st char. to print
 cmpa #4 is it eot?
 bne  print if not eot print it
 rts
echon tst  echo is echo required ?
 beq  inch echo not req. if clear
* inche
* ---gets character from terminal and
* echos same. the character is returned
* in the "a" accumulator with the parity
* bit masked off. all other registers
* are preserved.
inche bsr inch get char from terminal
 anda #$7f      strip parity from char.
 bra  outch     echo char to terminal
* inch
* get character from terminal. return
* character in "a" accumulator and preserve
* all other registers. the input character
* is 8 bits and is not echoed.
inch pshs x save ix
 ldx  cport point to terminal port
getsta lda  ,x  fetch port status
 bita #1 test ready bit, rdrf ?
 beq  getsta if not rdy, then try again
 lda  1,x fetch char
 puls x restore ix
 rts
* inchek
* check for a character available from
* the terminal. the serial port is checked
* for read ready. all registers are
* preserved, and the "z" bit will be
* clear if a character can be read.
inchek pshs a save a accum.
 lda  [cport] fetch port status
 bita #1 test ready bit, rdrf ?
 puls a restore a accum.
 rts
out2s bsr out1s output 2 spaces
out1s lda  #$20  output 1 space
* outch
* output character to terminal.
* the char. to be output is
* passed in the a register.
* all registers are preserved.
outch pshs a,x save a accum and ix
 ldx  cport get addr. of terminal
fetsta lda  ,x fetch port status
 bita #2 test tdre, ok to xmit ?
 beq  fetsta if not loop until rdy
 puls a get char. for xmit
 sta  1,x xmit char.
 puls x restore ix
 rts
aciniz ldx  cport  point to control port address
 lda  #3  reset acia port code
 sta  ,x  store in control register
 lda  #$11  set 8 data, 2 stop an 0 parity
 sta  ,x  store in control register
 tst  1,x  anything in data register?
 lda  #$ff  turn on echo flag
 sta  echo
 rts
* monitor keyboard command jump table
jmptab equ *
 fcb 1 " ^a "  $f91d
 fdb altra
 fcb 2 " ^b "  $f90f
 fdb altrb
 fcb 3 " ^c "  $f92b
 fdb altrcc
 fcb 4 " ^d "  $f901
 fdb altrdp
 fcb 5 " ^e "
 fdb altre
 fcb 6 " ^f "
 fdb altrf
 fcb $10 " ^p "  $f8c9
 fdb altrpc
 fcb $15 " ^u "  $f8d7
 fdb altru
 fcb $18 " ^x "  $f8f3
 fdb altrx
 fcb $19 " ^y "  $f8e5
 fdb altry
 fcc 'B'
 fdb brkpnt *$fa78
 fcc 'E'
 fdb memdump *$f990
 fcc 'G'
 fdb go *$f89f
 fcc 'L'
 fdb load *$fc09
 fcc 'M'
 fdb memchg *$f93b
 fcc 'P'
 fdb punch *$fc64
 fcc 'Q'
 fdb memtst *$f9ef
 fcc 'R'
 fdb regstr *$f8a2
 fcc 'S'
 fdb disstk *$f984
 fcc 'U'
 fdb minboot *$fbb0
 fcc 'X'
 fdb xbkpnt *$faa4
tabend equ *
* ** 63x09 vector addresses **
* following are the addresses of the vector routines
* for the 63x09 processor. during initialization they
* are relocated to ram from $dfc0 to $dfcf. they are
* relocated to ram so that the user may revector to
* his own routines if he so desires.
ramvec fdb swie  user-v
 fdb rti    swi3-v
 fdb rti    swi2-v
 fdb rti    firq-v
 fdb rti    irq-v
 fdb swie   swi-v
 fdb $ffff  svc-vo
 fdb $ffff  svc-vl
* printable message strings
msg1 fcb $0,$0,$0,$d,$a,$0,$0,$0 * 0, cr/lf, 0
 fcc 'cmi-bug 1.1 - '
 fcb 4
msg2 fcb 'k,$d,$a,$0,$0,$0,4 k, * cr/lf + 3 nuls
msg3 fcc '>'
 fcb 4
msg4 fcc 'what?'
 fcb 4
msg5 fcc ' - '
 fcb 4
msg6 fcc ', pass '
 fcb 4
msg7 fcc ', bits in error: '
 fcb 4
msg8 fcc ' => '
 fcb 4
msg9 fcc '76543210'
msg10 fcc '  sp='
 fcb 4
msg11 fcc '  pc='
 fcb 4
msg12 fcc '  us='
 fcb 4
msg13 fcc '  iy='
 fcb 4
msg14 fcc '  ix='
 fcb 4
msg15 fcc '  dp='
 fcb 4
msg16 fcc '  a='
 fcb 4
msg17 fcc '  b='
 fcb 4
msg21 fcc '  e='
 fcb 4
msg22 fcc '  f='
 fcb 4
msg18 fcc '  cc: '
 fcb 4
msg19 fcc 'efhinzvc'
msg20 fcc 's1'
 fcb 4

MODTAB  equ     *
        fcc     ':8S'
        fcb     4
        fcc     ':5S'
        fcb     4

* power up/ reset/ nmi entry point
start   equ   *
        lds     #romstk     set stack
        clra                 set dp
        tfr     a,dp
        fcb     $11,$3d,$03  set 63x09
*
*  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
* 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k 4k i/o,rom
*
        lbra monitor initialization is complete
v1 jmp  [trap]
v2 jmp  [swi2]
v3 jmp  [firq]
v4 jmp  [irq]
v5 jmp  [swi]
* swi3 entry point
swi3e tfr  s,u
 ldx  10,u      *$ffc8
 ldb  ,x+
 stx  10,u
 clra
 aslb
 rola
 ldx  svcvo
 cmpx #$ffff
 beq  swi3z
 leax d,x
 cmpx svcvl
 bhi  swi3z
 pshs x
 ldd  ,u
 ldx  4,u
 jmp   [,s++]
*
swi3z pulu a,b,x,cc,dp
 ldu  2,u
 jmp  [swi3]
*

        org     vectors
* 6809 vectors
 fdb v1    user-v
 fdb swi3e swi3-v
 fdb v2    swi2-v
 fdb v3    firq-v
 fdb v4    irq-v
 fdb v5    swi-v
 fdb v1    nmi-v
 fdb start restart-v
 end
                                                                                                                                                                     