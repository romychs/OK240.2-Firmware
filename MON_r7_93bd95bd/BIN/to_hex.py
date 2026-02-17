#!/bin/python3

row = 32
col = 0
width = 7

with open('font.bin', 'rb') as f:
    data = f.read()
    for b in data:
        print("0x{:02x}".format(b), end='')
        if col < (width-1):
            print(", ", end='')
        col = col + 1
        if col == width:
            col = 0
            if row < 127:
                print("  ; '{}'".format(chr(row)))
            else:
                print("  ; xx")
            row = row + 1
