
FLAGS=-O2 -g -D obj -Isrc -gnatwaF -gnaty3aAbdhikmnpr -we
BARGS=-static

all: memsim fixtrace memgen

memsim: src/*.adb src/*.ads
	gnatmake $(FLAGS) memsim.adb -bargs $(BARGS)

fixtrace: src/fixtrace.adb
	gnatmake $(FLAGS) fixtrace.adb -bargs $(BARGS)

memgen: src/*.adb src/*.ads
	gnatmake $(FLAGS) memgen.adb -bargs $(BARGS)

clean:
	rm -f fixtrace memsim memgen obj/*.ali obj/*.o

