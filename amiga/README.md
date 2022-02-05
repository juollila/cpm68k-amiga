Requirements
============
The following SW is required to compile Amiga BIOS for CP/M-68k and create a  bootable
disk image:
1. Cpmtools, http://www.moria.de/~michael/cpmtools/
2. Vassm68k_mot, http://sun.hasenbraten.de/vasm/
3. A tool which can convert a Motorola S record file to a binary file. For example SDCC compiler suite's
sdobjcopy utility.
4. Python3


Creating blank CP/M disk image
==============================

The following procedure can be used to create a new blank CP/M disk image:
1. Create a blank adf image "cpm-blank.adf" for exampale using UAE emulator.
2. Add the following entry to diskdefs of cpmtools:
```
diskdef amiga
  seclen 128
  tracks 160
  sectrk 44
  blocksize 2048
  maxdir 128
  boottrk 8
  os 2.2
end
```
3. Create a CP/M filesystem: `mkfs.cpm -f amiga cpm-blank.adf`

Note: This image is not bootable.

Convert CPM15000.SR file to binary file
=======================================

CP/M binary distribution contains CCP and BDOS in Motorola S record format.
It can be converted to binary format using for example SDCC compiler suite's
sdobjcopy utility.

```
$ sdobjcopy --input-target srec --output-target binary CPM15000.SR cpm15000.bin
```

Compiling BIOS
==============

Amiga BIOS for CP/M-68k can be compiled using the following command.

```
$ vasmm68k_mot bios.asm -Fbin -o bios.bin
```

Creating CPM.BIN
================

CP/M binary contains CCP, BDOS and BIOS. The starting address of CCP and BDOS (cpm15000.bin)
is $15000 and starting address of BIOS is $1b000. A padding must be added between cpm15000.bin
and bios.bin. At first a padding file is created.
```
$ dd if=/dev/zero of=padding.bin bs=3648 count=1
```

The cpm.bin file is created concatenating cpm15000.bin, padding.bin and bios.bin.
```
$ cat cpm15000.bin padding.bin bios.bin > cpm.bin
```

Creating bootable disk image
============================
At first boot track binary is created using the following command. The binary contains boot loader, CCP, BDOS and BIOS.
```
$ vasmm68k_mot boot_loader.asm -Fbin -o boot.bin
```

Calculate and write Amiga boot block's checksum.

```
$ python3 checksum.py --output boot-with-checksum.bin boot.bin
```

Bootable disk image is created copying boot track binary to a blank CP/M disk image:
```
$ cp cpm-blank.adf cpm-boot.adf
$ mkfs.cpm -f amiga -b boot-with-checksum.bin cpm-boot.adf
```

Copy files to cpm-boot.adf
==========================

TODO: Add files
