
FLAGS=-O2 -D obj -Isrc -gnatwaF -gnaty3aAbdhikmnpr -we

all: memsim fixtrace

memsim: src/*.adb src/*.ads
	gnatmake $(FLAGS) memsim.adb

fixtrace: src/fixtrace.adb
	gnatmake $(FLAGS) fixtrace.adb

clean:
	rm -f fixtrace memsim obj/*.ali obj/*.o

