.PHONY: all clean run

all: main.prg split.prg

clean:
	rm -f *.o *.prg *.l

run: all
	x16emu -prg main.prg -run

MAIN=basic.o main.o vera.o lzsa.o init.o

main.prg: yaw.cfg $(MAIN)
	cl65 -t cx16 -C yaw.cfg -O -o $@ $(MAIN)

SPLIT=basic.o init.o split.o
split.prg: yaw.cfg $(SPLIT)
	cl65 -t cx16 -C yaw.cfg -O -o $@ $(SPLIT)


%.o: %.s
	ca65 -t cx16 -g -o $@ $< -l $(@:.o=.l)
	
main.o: vera.inc mem.inc zp.inc data.inc
vera.o: vera.inc mem.inc zp.inc
