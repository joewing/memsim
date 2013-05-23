
FILES=ram.vhdl cache.vhdl mem.vhdl tb.vhdl

all:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --wave=dump.ghw --stop-time=1us

