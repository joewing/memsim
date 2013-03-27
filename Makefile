
all:
	gnatmake -O2 -D obj -Isrc -gnatwaF -gnaty3aAbdhikmnpr -we memsim.adb

clean:
	rm -f memsim obj/*.ali obj/*.o

