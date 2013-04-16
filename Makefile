
FLAGS=-O2 -D obj -Isrc -gnatwaF -gnaty3aAbdhikmnpr -we

all: memsim fixtrace memgen

memsim: src/*.adb src/*.ads
	gnatmake $(FLAGS) memsim.adb

fixtrace: src/fixtrace.adb
	gnatmake $(FLAGS) fixtrace.adb

memgen: src/*.adb src/*.ads
	gnatmake $(FLAGS) memgen.adb

clean:
	rm -f fixtrace memsim memgen obj/*.ali obj/*.o

