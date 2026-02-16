# Ocean-240.2 ROM Sources CP/M (V2.2) REL.8 checksum bc0695e4

Source codes of CP/M (V2.2) REL.8 version for Ocean-240.2 with Floppy controller.
In Z80 mnemonics, but limited for i8080 instruction set.

* READ command replaced with Intel HEX loader by **tnt23**
* 192k RAM drive

## Compile

    sjasmplus --sld=cpm.sld --sym=cpm.labels --raw=cpm.obj --fullpath cpm.asm

To compile sources, use [sjasmplus Z80 assembler](https://github.com/z00m128/sjasmplus).

## Convert binary file to Intel HEX

    srec_cat file_name.COM -binary -offset 0x100 -output file_name.hex -Intel

## Send HEX from Linux console

It is assumed that there is a USB-RS232 (USB-TTL) adapter on the /dev/ttyUSB0.

1) At Ocean's CP/M command line:
   
    A>READ

2) At Linux terminal, configure tty for 4800,8N2:

    stty -F /dev/ttyUSB0 4800 cs8 -cstopb

3) At Linux terminal, send file:

    cat okeah.hex > /dev/ttyUSB0


srec_cat - Utility from **srecord** package - collection of tools for manipulating EPROM load files.

[Forum topic](https://zx-pk.ru/threads/35390-zagruzka-hex-fajlov-direktivoj-l-monitora.html)
