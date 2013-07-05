
FILES= ram.vhdl spm.vhdl cache.vhdl split.vhdl combine.vhdl eor.vhdl \
       reg.vhdl prefetch.vhdl offset.vhdl shift.vhdl \
       mem.vhdl tb.vhdl trace.vhdl

all: run-tb

run-tb:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --ieee-asserts=disable --stop-time=1ms --wave=dump.ghw

run-trace:
	ghdl -a $(FILES)
	ghdl -e trace
	ghdl -r trace --ieee-asserts=disable

