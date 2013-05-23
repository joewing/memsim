
FILES= ram.vhdl spm.vhdl cache.vhdl split.vhdl combine.vhdl mem.vhdl tb.vhdl

all:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --wave=dump.ghw --stop-time=100us

