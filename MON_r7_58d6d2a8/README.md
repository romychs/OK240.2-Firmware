# Ocean-240.2 ROM Monitor release 7 

**CRC32 checksum**: 58d6d2a8

Source codes of Turbo Monitor r7 for Ocean-240.2 with Floppy controller.

In Z80 mnemonics, but limited for i8080 instruction set.

## Compile:

Code is located in memory at address: 0xE000..0xFFFF

   sjasmplus --sld=monitor.sld --sym=monitor.labels --raw=monitor.obj --fullpath monitor.asm

To compile sources, use [sjasmplus Z80 assembler](https://github.com/z00m128/sjasmplus).
