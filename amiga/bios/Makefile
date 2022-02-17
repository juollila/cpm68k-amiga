all: bios.bin cpm.bin boot.bin adf

bios.bin: bios.asm
	vasmm68k_mot bios.asm -Fbin -o bios.bin

cpm.bin: bios.bin
	dd if=../../cpm/cpm60000.bin ibs=24k count=1 of=cpm60000-padded.bin conv=sync
	cat cpm60000-padded.bin bios.bin > cpm.bin

boot.bin: boot_loader.asm cpm.bin
	vasmm68k_mot boot_loader.asm -Fbin -o boot.bin
	python3 ../tools/checksum.py --output boot-with-checksum.bin boot.bin

adf: boot-with-checksum.bin
	cp ../adf/cpm-blank.adf cpm-boot.adf
	mkfs.cpm -f amiga -b boot-with-checksum.bin cpm-boot.adf
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK1/STAT.REL 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK1/PIP.REL 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK1/README.TXT 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK1/RELOC.REL 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK3/CLIB 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK3/LO68.REL 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK6/SENDC68.REL 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK8/CPMLIB 0:
	cpmcp -f amiga cpm-boot.adf ../../cpm/disks/DISK8/CPM.REL 0:
clean:
	rm -f *.bin
	rm -f *.adf

