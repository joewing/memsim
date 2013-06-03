
FILES= ram.vhdl spm.vhdl cache.vhdl split.vhdl combine.vhdl \
       offset.vhdl shift.vhdl mem.vhdl tb.vhdl trace.vhdl

all: run-tb

run-tb:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --stop-time=1ms

run-trace:
	ghdl -a $(FILES)
	ghdl -e trace
	ghdl -r trace 

