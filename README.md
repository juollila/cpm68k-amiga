# Introduction

CP/M (Control Program/Monitor) is an operating system for 8-bit and 16-bit processors.
It was the most famous operating system for 8-bit Intel 8080, Intel 8085 and Zilog Z80 processors.
CP/M-68K is a CP/M operating system for Motorola 68000 and 68010 processors. It was used on
the Motorola EXORmacs developments system, SORD M68 and M68MX computers. CP/M-68K was very
uncommon operating system when compared to 8-bit CP/M-80 variant. Software availability is
limited for the CP/M-68K, but the operating system includes for example an assembler and
a C language compiler.

SturmBIOS is an implementation of CP/M-68K BIOS for Commodore Amiga. It allows the usage
of CP/M-68K on Amiga 500, 1000 and 2000 computers.

# Documentation

CP/M-68k documentation can be found from https://github.com/juollila/cpm68k-amiga/tree/main/cpm/doc

It is recommended that new users read User's Guide. If a user wants program then Programmer's Guide
and C Programming Guide are recommended.

# Bootable Disk Image

A bootable disk image: https://github.com/juollila/cpm68k-amiga/blob/main/amiga/adf/cpm-boot.adf

# Memory Map

|Start Address |End Address |Description                      |
|--------------|------------|---------------------------------|
|0             |3ff         |Exception vectors                |
|400           |5ffff       |Transient program area           |
|60000         |65fff       |BDOS and CCP                     |
|66000         |7ffff       |BIOS, disk buffer, screen buffer |

# Disk Format

Amiga CP/M disk uses Amiga MFM sector format at the lowest level so that Amiga ADF utilities can
be used to read and write physical disks. Amiga CP/M disk uses CP/M file system instead of Amiga
file system (OFS, FFS or SFS).

The structure of disk:

|Start                |End                   |Description                      |
|---------------------|----------------------|---------------------------------|
|Track 0, sector 0    |Track 0, sector 1     |Boot loader                      |
|Track 0, sector 2    |Track 7, sector 10    |CCP, BDOS and BIOS               |
|Track 8, sector 0    |Track 8, sector 7     |CP/M file system, directory area |
|Track 8, sector 8    |Track 159, sector 10  |CP/M file system, data area      |

Disk definition for Cpmtools:
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

# Terminal Emulation

TBD


# Restrictions

Amiga HW restrictions:
- Only 1-2 floppy disk drives are supported.
- Only 68000 and 68010 processors are supported. Other Motorola 68k family processors
may work, but not tested.
- Hard disks are not supported.

BIOS restrictions:
- I/O byte is not supported. For example Kermit does not work.
- Serial port is not supported.
- Parallel port is not supported.

# Boot Process

CP/M-68K's boot process is the following:
1. Amiga Kickstart ROM loads the boot loader from the boot block.
2. Boot loader loads BDOS, CCP and BIOS from the reserved tracks using the trackdisk device.
3. Boot loader enables the supervisor mode.
4. Boot loader jumps to BDOS initialization routine ($60000).
5. BDOS initialization routine calls BIOS init routine ($66000).
6. BIOS takes over Amiga's operating system i.e. disables interrupts and DMA.
7. BIOS initializes screen, CIA timers, keyboard interrupt, floppy and trap number #3.
8. BIOS returns to BDOS.
9. BDOS and CCP performs the rest of initializations.


# Building

## Requirements

The following SW is required to compile Amiga BIOS for CP/M-68k and create a  bootable
disk image:
1. Cpmtools, http://www.moria.de/~michael/cpmtools/
2. Vassm68k_mot, http://sun.hasenbraten.de/vasm/
3. A tool which can convert a Motorola S record file to a binary file. For example SDCC compiler suite's
sdobjcopy utility.
4. Python3

## Creating blank CP/M disk image

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

## Makefile

Use the following commands to create a bootable floppy disk image after you have installed all requirements and added Amiga disk format to diskdefs of cpmtools:

```
$ cd amiga/bios
$ make
```

Or you may follow the manual procedure which is described in the following sections.

See also Makefile for further details.


## Convert CPM60000.SR file to binary file

CP/M binary distribution contains CCP and BDOS in Motorola S record format.
It can be converted to binary format using for example SDCC compiler suite's
sdobjcopy utility.

```
$ sdobjcopy --input-target srec --output-target binary cpm60000.SR cpm60000.bin
```

## Compiling BIOS

Amiga BIOS for CP/M-68k can be compiled using the following command.

```
$ vasmm68k_mot bios.asm -Fbin -o bios.bin
```

## Creating CPM.BIN

CP/M binary contains CCP, BDOS and BIOS. The starting address of CCP and BDOS (cpm60000.bin)
is $60000 and starting address of BIOS is $66000. A padding must be added between cpm60000.bin
and bios.bin. At first a padding file is created.

```
$ dd if=cpm60000.bin ibs=24k count=1 of=cpm60000-padded.bin conv=sync
```

The cpm.bin file is created concatenating cpm60000-padded.bin, padding.bin and bios.bin.

```
$ cat cpm60000-padded.bin padding.bin bios.bin > cpm.bin
```

## Creating bootable disk image

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

## Copy files to cpm-boot.adf

CP/M programs can be copied to cpm-boot.adf using Cpmtools.

For example:
```
$ cpmcp -f amiga cpm-boot.adf program 0:
```

# References


CP/M-68K System Guide: https://github.com/juollila/cpm68k-amiga/blob/main/cpm/doc/CPM-68K_System_Guide_Jan83.pdf

Amiga Hardware Manual: http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0000.html

SturmBIOS floppy drive routines were inspired by AROS and EmuTOS floppy drive implementations:

EmuTOS - Amiga specific funtions: https://github.com/emutos/emutos/blob/master/bios/amiga.c

AROS - Amiga HW floppy stuff: https://github.com/aros-development-team/AROS/blob/master/arch/m68k-amiga/devs/trackdisk/trackdisk_hw.c
