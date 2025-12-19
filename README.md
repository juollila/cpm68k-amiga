# Introduction

CP/M (Control Program/Monitor) is an operating system for 8-bit and 16-bit processors.
It was the most famous operating system for 8-bit Intel 8080, Intel 8085 and Zilog Z80 processors.
CP/M-68K is a CP/M operating system for Motorola 68000 and 68010 processors. It was used on
the Motorola EXORmacs developments system, SORD M68 and M68MX computers. CP/M-68K was very
uncommon operating system when compared to 8-bit CP/M-80 variant. Software availability is
limited for the CP/M-68K, but the operating system includes for example an assembler and
a C language compiler.

SturmBIOS (also known as cpm68k-amiga) is an implementation of CP/M-68K BIOS for Commodore Amiga.
It allows the usage of CP/M-68K on Amiga 500, 600, 1000 and 2000 computers.

Note: This project is still in "work in progress" state. There are bugs and a new version can be
buggier than a little bit older version. Testing is done mainly with UAE so that development is
quicker. But real HW is planned to be supported.

# Documentation

CP/M-68k documentation can be found from https://github.com/juollila/cpm68k-amiga/tree/main/cpm/doc

It is recommended that new users read User's Guide. If a user wants to program then Programmer's Guide
and C Programming Guide are recommended.

Amiga CP/M-68k supports the following additional commands:
|Program       |Purpose                                           |
|:-------------|:-------------------------------------------------|
|format.68k    |Format disk and install CP/M to reserved tracks   |
|setbaud.68k   |Set baud rate for the serial port                 |



# Bootable Disk Image

A bootable disk image: https://github.com/juollila/cpm68k-amiga/blob/main/amiga/adf/cpm-boot.adf

# Memory Map

|Start Address |End Address |Description                      |
|:-------------|:-----------|:--------------------------------|
|00000         |003ff       |Exception vectors                |
|00400         |5ffff       |Transient program area           |
|60000         |65fff       |BDOS and CCP                     |
|66000         |7ffff       |BIOS, disk buffer, screen buffer |

# Disk Format

Amiga CP/M disk uses Amiga MFM sector format at the lowest level so that Amiga ADF utilities can
be used to read and write physical disks. Amiga CP/M disk uses CP/M file system instead of Amiga
file system (OFS, FFS or SFS).

The structure of disk:

|Start                |End                   |Description                      |
|:--------------------|:---------------------|:--------------------------------|
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

SturmBIOS support both ADM-3A and VT-52 terminal emulation.

ADM-3a control codes:
|Control code    |Description                                               |
|:---------------|:---------------------------------------------------------|
|^G              |Bell                                                      |
|^H or backspace |Cursor left / backspace                                   |
|^I or TAB       |Tabulator                                                 |
|^J              |Line feed i.e. cursor down + scrolling if needed          |
|^K              |Cursor up / vertical tab                                  |
|^L              |Cursor right / form feed                                  |
|^M              |Carriage return                                           |
|^Z              |Move cursor home and clear screen                         |
|ESC = row col   |Move cursor position, add $20 to row and col              |

VT-52 control codes:
|Control code    |Description                                               |
|:---------------|:---------------------------------------------------------|
|^G              |Bell                                                      |
|^H or backspace |Cursor left / backspace                                   |
|^I or TAB       |Tabulator                                                 |
|^J		           |Line feed i.e. cursor down + scrolling if needed          |
|^K              |Cursor up / vertical tab                                  |
|^L              |Cursor right / form feed                                  |
|^M              |Carriage return                                           |
|^Z              |Move cursor home and clear screen                         |
|ESC A           |Cursor up without scrolling                               |
|ESC B           |Cursor down without scrolling                             |
|ESC C           |Cursor right                                              |
|ESC D           |Cursor left / backspace                                   |
|ESC H           |Move cursor home                                          |
|ESC I           |Cursor up + scrolling if needed                           |
|ESC J           |Erase to end of screen                                    |
|ESC K           |Erase to end of line                                      |
|ESC Y col row   |Cursor position, add $20 to col and row                   |

# HW Restrictions

Amiga HW restrictions:
- Only 1-2 floppy disk drives are supported.
- Only 68000 and 68010 processors are supported. Other Motorola 68k family processors
may work, but not tested.
- Hard disks are not supported.
- Parallel port is not supported.

# BIOS Functions

Standard BIOS functions are documented in System Guide:
https://github.com/juollila/cpm68k-amiga/blob/main/cpm/doc/CPM-68K_System_Guide_Jan83.pdf

SturmBIOS does not support following standard BIOS functions:
- List Character Output
- Return List Status
- Get I/O Byte
- Set I/O Byte

The missing List device support means that parallel port is not supported. The missing I/O byte support means that for example Kermit does not work.

# XBIOS Functions

SturmBIOS supports some non standard eXtended BIOS (XBIOS) functions. User applications do not usually call XBIOS functions. When an application calls an XBIOS function, it places the function number in register D0.W, and function parameters to other registers as specified in the following table. After that it executes a `trap #4`instruction.

<table>
  <tr>
    <th>Function Number:</th>
    <th>Function Name:</th>
    <th>Description:</th>
  </tr>
    <tr>
    <td>
      <b>0</b>
    </td>
    <td>
      <b>RESERVED</b>
    </td>
    <td>
      Reserved for future use
    </td>
  </tr>

  <tr>
    <td>
      <b>1</b>
    </td>
    <td>
      <b>GET BAUD RATE</b>
    </td>
    <td>
      <b>Entry Parameters:</b><br>
      Register D0.W: $1<br>
      <br>
      <b>Returned Values:</b><br>
      Register D0.W: Baud Rate (300, 1200, 2400, 4800, 9600, 14400, 19200, 38400, or 57600)<br>
      Register D1.W: Data Bits (8)<br>
      Register D2.W: Parity (0=None)<br>
      Register D3.W: Stop Bits (1)<br>
      <br>
      Get Serial Parameters Function fetches baud rate, a number of data bits, parity, a number of stop bits for the serial port.
    </td>
  </tr>
  <tr>
    <td>
      <b>2</b>
    </td>
    <td>
      <b>SET BAUD RATE</b>
    </td>
    <td>
      <b>Entry Parameters:</b><br>
      Register D0.W: $2<br>
      Register D1.W: Baud Rate (300, 1200, 2400, 4800, 9600, 14400, 19200, 38400, or 57600)<br>
      Register D2.W: Data Bits (8)<br>
      Register D3.W: Parity (0=None)<br>
      Register D4.W: Stop Bits (1)<br>
      <br>
      <b>Returned Values:</b><br>
      Register D0.W: $0000=configuration was successful<br>
      Register D0.W: $ffff=configuration failed<br>
      <br>
      Set Serial Parameters Function configures baud rate, a number of data bits, parity, a number of stop bits for the serial port. Baud rate 9600, 8 data bits, no parity and 1 stop bit are defaults which are taken into use during the SturmBIOS start-up. Baud rate 19200 is the reliable maximum for Amiga 500.
    </td>
  </tr>
    <tr>
    <td>
      <b>3</b>
    </td>
    <td>
      <b>GET FLOW CONTROL</b>
    </td>
    <td>
      <b>Entry Parameters:</b><br>
      Register D0.W: $3<br>
      <br>
      <b>Returned Values:</b><br>
      Register D0.W: RTS/CTS Flow Control (0=No, 1=Yes)<br>
      <br>
      Get RTS/CTS flow control for the serial port.
    </td>
  </tr>
  <tr>
    <td>
      <b>4</b>
    </td>
    <td>
      <b>SET FLOW CONTROL</b>
    </td>
    <td>
      <b>Entry Parameters:</b><br>
      Register D0.W: $4<br>
      Register D1.W: RTS/CTS Flow Control (0=No, 1=Yes)
      <br>
      <b>Returned Values:</b><br>
      Register D0.W: $0000=configuration was successful<br>
      Register D0.W: $ffff=configuration failed<br>
      <br>
      Set RTS/CTS flow control for the serial port. RTS/CTS flow control is enabled by default during the SturmBIOS start-up.
    </td>
  </tr>
    <tr>
    <td>
      <b>5</b>
    </td>
    <td>
      <b>FORMAT</b>
    </td>
    <td>
      <b>Entry Parameters:</b><br>
      Register D0.W: $5<br>
      Register D1.W: Drive (0=A, 1=B)
      <br>
      <b>Returned Values:</b><br>
      Register D0.W: $0000=operation was successful<br>
      Register D0.W: $ffff=operation failed<br>
      <br>
      Format a disk in disk drive. This function does not install reserved tracks.
    </td>
  </tr>
</table>

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

The following SW is required to compile SturmBIOS for CP/M-68k and create a  bootable
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

## Compiling SturmBIOS

SturmBIOS for CP/M-68k can be compiled using the following command.

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
