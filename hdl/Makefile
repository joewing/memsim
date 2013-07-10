
FILES= ram.vhdl spm.vhdl cache.vhdl split.vhdl combine.vhdl eor.vhdl \
       reg.vhdl prefetch.vhdl offset.vhdl shift.vhdl \
       mem.vhdl tb.vhdl trace.vhdl

all: run-tb

run-tb: compile
	ghdl -e tb
	ghdl -r tb --ieee-asserts=disable --stop-time=5ms

run-trace: compile
	ghdl -e trace
	ghdl -r trace --ieee-asserts=disable

compile:
	ghdl -a $(FILES)

