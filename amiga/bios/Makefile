all: bios.bin cpm.bin boot.bin adf

bios.bin: bios.asm
	vasmm68k_mot bios.asm -Fbin -o bios.bin

cpm.bin: bios.bin ../../cpm/system/cpm60000.bin
	dd if=../../cpm/system/cpm60000.bin ibs=24k count=1 of=cpm60000-padded.bin conv=sync
	cat cpm60000-padded.bin bios.bin > cpm.bin

boot.bin: boot_loader.asm cpm.bin
	vasmm68k_mot boot_loader.asm -Fbin -o boot.bin
	python3 ../tools/checksum.py --output boot-with-checksum.bin boot.bin

adf: boot-with-checksum.bin
	cp ../adf/cpm-blank.adf ../adf/cpm-boot.adf
	mkfs.cpm -f amiga -b boot-with-checksum.bin ../adf/cpm-boot.adf
	# disk 1
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/copy.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK1/CPM.SYS 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/ddt.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK1/DDT68000.68K 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/init.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/pip.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK1/README.TXT 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/reloc.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK1/RELOC1.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/sd.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/stat.68k 0:
	# disk 2
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/as68.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK2/AS68INIT 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK2/CPM.SYS 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK2/DDT10.REL 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK2/DDT68010.REL 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/dump.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK2/RELOC2.SUB 0:
	# disk 3
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/CLIB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/CLINK.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/CLINKE.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/CLINKF.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/ed.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/LIBE.A 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/LIBF.A 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/lo68.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/RELOC3.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK3/S.O 0:
	# disk 4
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/ar68.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/ASSERT.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/cp68.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/CTYPE.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/ERRNO.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/find.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/MORE.C 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/more.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/nm68.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/OPTION.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/OSATTR.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/OSIF.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/OSIFERR.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/PORTAB.H 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/RELOC4.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/SETJMP.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/SIGNAL.H 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK4/STDIO.H 0:
	# disk 5
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK5/C.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/c068.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/c168.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK5/CE.SUB 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK5/RELOC5.SUB 0:
	# disk 6
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/FORMAT.REL 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/FORMAT.S 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/INIT.S 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/LINK68.REL 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/LOADR.O 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/OVHDLR.O 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/RELOC6.SUB 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/sendc68.68k 0:
	cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/c/size68.68k 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/TERM.C 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/TERM.REL 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/TERMA.S 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/XFER86.C 0:
	# cpmcp -f amiga ../adf/cpm-boot.adf ../../cpm/disks/DISK6/XFER86.REL 0:
	#
	cp ../adf/cpm-blank.adf ../adf/cpm-me.adf
	mkfs.cpm -f amiga -b boot-with-checksum.bin ../adf/cpm-me.adf
	cpmcp -f amiga ../adf/cpm-me.adf ../../applications/me68k/*.* 0:

clean:
	rm -f *.bin
	rm -f *.adf

