.PHONY: all clean run

all: main.prg

clean:
	rm -f *.o *.prg *.l

run: all
	x16emu -prg main.prg -run

main.prg: basic.o main.o vera.o yaw.cfg lzsa.o
	cl65 -t cx16 -C yaw.cfg -O -o $@ basic.o main.o vera.o lzsa.o

%.o: %.s
	ca65 -t cx16 -g -o $@ $< -l $(@:.o=.l)
	
main.o: vera.inc mem.inc zp.inc data.inc
vera.o: vera.inc mem.inc zp.inc
