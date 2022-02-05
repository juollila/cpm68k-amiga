#!/usr/bin/python

# Calculate and write Amiga boot block's checksum

import sys
import argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=str,
                        help="calculate Amiga boot block's checksum for the file")
    parser.add_argument("-o", "--output", type=str,
                        help="write checksum to the output file")
    args = parser.parse_args()

    data = bytearray()
    with open(args.file, "rb") as f:
        data += f.read()
    # clear checksum
    data[4:8] = bytearray(4)
    # calculate checksum
    checksum = 0
    for i in range(0, 1024, 4):
        checksum += int((data[i+0] << 24) | (data[i+1] << 16) | (data[i+2] << 8) | (data[i+3]))
        if checksum >= 2**32:
            checksum = checksum % (2**32)
            checksum += 1
    checksum = checksum ^ ((2**32)-1)
    print("Checksum is " + hex(checksum))
    # write checksum
    data[4] = (checksum >> 24) & 0xff
    data[5] = (checksum >> 16) & 0xff
    data[6] = (checksum >> 8) & 0xff
    data[7] = (checksum & 0xff)
    if args.output:
        with open(args.output, "wb") as f:
            f.write(data)
            print(f"Checksum was written to {args.output}")

if __name__ == "__main__":
    sys.exit(main())

