
Memory Simulator
==============================================================================

This is a library to simulate memory hierarchies to determine how long
memory access take.

Currently, the following packages are available:

 - *Memory*: The base package.
 - *Memory.RAM*: A simple RAM with constant access time.
 - *Memory.Cache*: A write-back cache with various parameters.
 - *Memory.Bank*: A memory bank which can contain multiple memories.
 - *Memory.Prefetch*: A memory prefetcher (assumes prefetches are free).
 - *Memory.SPM*: A scratchpad memory.
 - *Memory.Stats*: A memory to track memory access statistics.

The *Parser* package can be used to parse a file containing a memory
description and create a memory.
After constructing a memory hierarchy, the *Trace* package can be used
to execute a trace of reads and writes through the hierarchy.
Memory traces are sequences of memory access actions separated by
new lines.  The format of each line is:

   *action* *value*

where *action* is 'R' for read, 'W' for write, and 'I' for idle.
For reads and writes, *value* is a decimal value indicating the
address.  For idle states, *value* is the number of idle cycles.
Idle is used to separate reads and writes.


