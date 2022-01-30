Motorola S Record to binary conversion
======================================

CP/M binary distribution contains CCP and BDOS in Motorola S record format.
It can be converted to binary format using for example SDCC compiler suite's
sdobjcopy utility.

```
$ sdobjcopy --input-target srec --output-target binary CPM15000.SR cpm15000.bin
```
