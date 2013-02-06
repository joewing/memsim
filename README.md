
Memory Simulator
==============================================================================

This is a library to simulate memory hierarchies to determine how long
memory access take.

Currently, the following memory packages are available:

 - *Memory*: The base package.
 - *Memory.RAM*: A simple RAM with constant access time.
 - *Memory.Cache*: A write-back cache with various parameters.
 - *Memory.Bank*: A memory bank which can contain multiple memories.
 - *Memory.Prefetch*: A memory prefetcher (assumes prefetches are free).
 - *Memory.SPM*: A scratchpad memory.
 - *Memory.Stats*: A memory to track memory access statistics.

The following benchmarks are available:

 - *Benchmark* The base package.
 - *Benchmark.Hash* A benchmark to generate random memory accesses.
 - *Benchmark.Heap* A benchmark to perform operations on a binary heap.
 - *Benchmark.Trace* A benchmark to process a memory trace.
 - *Benchmark.Stride* A benchmark to generate strided memory accesses.

The *Parser* package can be used to parse a file containing a memory
description and create a memory.
After constructing a memory hierarchy, a benchmark package is used
to use the memory.

For the trace benchmark, sequences of memory access actions separated by
new lines.  The format of each line is:

   *action* *value*

where *action* is 'R' for read, 'W' for write, and 'I' for idle.
For reads and writes, *value* is a hexadecimal value indicating the
address.  For idle states, *value* is the number of idle cycles.
Idle is used to separate reads and writes.

