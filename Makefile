.PHONY: all clean run

all: main.prg save.o sweet16.o

clean:
	rm -f *.o *.prg *.l

run: all
	x16emu -prg main.prg -run

main.prg: basic.o main.o vera.o save.o sweet16.o yaw.cfg
	cl65 -t cx16 -C yaw.cfg -O -o $@ *.o

%.o: %.s
	ca65 -t cx16 -g -o $@ $< -l $(@:.o=.l)
	
main.o: vera.inc mem.inc zp.inc data.inc
sweet16.o: sweet16.inc
vera.o: vera.inc mem.inc zp.inc
