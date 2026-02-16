# Ocean-240.2 ROM Turbo monitor by Azmaster 

**CRC32 checksum**: bcac5ca0

Source codes of Turbo Monitor for Ocean-240.2 with Floppy controller.

In Z80 mnemonics, but limited for i8080 instruction set.

## Compile:

Code is located in memory at address: 0xE000..0xFFFF

   sjasmplus --sld=turbo_mon.sld --sym=turbo_mon.labels --raw=turbo_mon.obj --fullpath turbo_mon.asm

To compile sources, use [sjasmplus Z80 assembler](https://github.com/z00m128/sjasmplus).
