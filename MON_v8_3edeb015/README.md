# Ocean-240.2 ROM Monitor V8 checksum 3edeb015

Source codes of Monitor v8 for Ocean-240.2 with Floppy controller.
In Z80 mnemonics, but limited for i8080 instruction set.

## Differences:

1) Font. Russian letters б and д;
2) Calculate values for extended ram access in procedures m_ramdisk_read, m_ramdisk_write.

## Compile:

Code is located in memory at address: 0xE000..0xFFFF

   sjasmplus --sld=monitor.sld --sym=monitor.labels --raw=monitor.obj --fullpath monitor.asm

To compile sources, use [sjasmplus Z80 assembler](https://github.com/z00m128/sjasmplus).
