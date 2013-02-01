
all:
	gnatmake -D obj -Isrc -gnatwa main.adb

clean:
	rm -f main obj/*.ali obj/*.o

