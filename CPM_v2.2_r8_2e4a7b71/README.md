# Ocean-240.2 ROM Sources CP/M (V2.2) REL.8' checksum 2e4a7b71

Source codes of CP/M (V2.2) REL.8' version for Ocean-240.2 with Floppy controller.
In Z80 mnemonics, but limited for i8080 instruction set.

* 192k RAM drive

## Compile:
    sjasmplus --sld=cpm.sld --sym=cpm.labels --raw=cpm.obj --fullpath cpm.asm

To compile sources, use [sjasmplus Z80 assembler](https://github.com/z00m128/sjasmplus).
