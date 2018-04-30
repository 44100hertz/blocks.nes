all: rom
rom: chr.bin
	ca65 *.asm
	ld65 -C rom.cfg -o blocks.nes blocks.o

chr.bin: bmp2chr chr.bmp
	./bmp2chr

bmp2chr: bmp2chr.c
	cc bmp2chr.c -o bmp2chr `sdl2-config --cflags --libs` $(CFLAGS)

run: rom
	mednafen blocks.nes

.PHONY: all
