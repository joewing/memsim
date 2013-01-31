
Memory Simulator
==============================================================================

This is a library to simluate memory hierarchies to determine how long
memory access take.

Currenty, the following packages are available:

 - *Memory*: The base package.
 - *Memory.RAM*: A simple RAM with constant access time.
 - *Memory.Cache*: A write-back cache with various parameters.
 - *Memory.Bank*: A memory bank which can contain multiple memories.
 - *Memory.Prefetch*: A memory prefetcher (assumes prefetches are free).

After constructing a memory hierarchy, the *Trace* package can be used
to execute a trace of reads and writes through the hierarchy.

