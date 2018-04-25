all:
	ca65 blocks.asm
	ld65 -t nes -o blocks.nes blocks.o

run:	all
	mednafen blocks.nes
