#!/bin/sh
echo "*** Compiling bios.asm"
vasmm68k_mot bios.asm -Fbin -o bios.bin
echo
echo "*** Creating a padding file"
dd if=/dev/zero of=padding.bin bs=3648 count=1
echo
echo "*** Creating cpm.bin"
cat ../../cpm/cpm15000.bin padding.bin bios.bin > cpm.bin
echo
echo "*** Compiling boot loader"
vasmm68k_mot boot_loader.asm -Fbin -o boot.bin
echo
echo "*** Writing checksum"
python3 ../tools/checksum.py --output boot-with-checksum.bin boot.bin
cp ../adf/cpm-blank.adf $1
echo
echo "*** Copying boot tracks to disk image"
mkfs.cpm -f amiga -b boot-with-checksum.bin $1
echo
echo "*** Disk image was created"
