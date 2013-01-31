
all:
	gnatmake -gnatwa main.adb

clean:
	rm -f main *.ali *.o

