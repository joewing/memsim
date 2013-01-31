
all:
	gnatmake -Isrc -gnatwa main.adb

clean:
	rm -f main *.ali *.o src/*.ali src/*.o

