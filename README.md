
Memory Simulator
==============================================================================

Usage
------------------------------------------------------------------------------

This is a memory simulator to determine how long memory access take.
Running memsim with no arguments will show its usage:

<pre>
usage: ./memsim &lt;memory&gt; &lt;benchmark&gt; [&lt;options&gt;]
benchmarks:
  hash [size=1024] [iterations=1000] [spacing=100]
  heap [size=1024] [iterations=1000] [spacing=100]
  stride [size=1024] [iterations=1000] [stride=1] [spacing=100]
  trace [file=trace.txt] [spacing=100]
</pre>

The memory is a memory description.  Memories are specified using
s-expressions.  For example, a simple cached memory would look as follows:

<pre>
(cache (latency 1) (line_size 8) (line_count 1024) (associativity 4)
   (memory (ram (latency 100))))
</pre>

See the *mem* subdirectory for more examples.

The following benchmarks are available:

 - *hash*: A benchmark to generate random 4-byte memory accesses.
 - *heap*: A benchmark to perform random insertions and deletions of
   4-byte items in a full binary heap.
 - *stride*: A benchmark to perform strided 4-byte memory accesses.
 - *trace*: A benchmark to execute a memory access trace.

The optional arguments are shown above for each benchmark with defaults.
The arguments are:

 - *size*: The number of items in the data structure.
 - *iterations*: The number of iterations to perform.
 - *spacing*: The number of cycles to insert between each memory access.
 - *stride*: The stride size in 4-byte words for the *stride* benchmark.
 - *file*: The name of the file to open for the *trace* benchmark.

For the trace benchmark, the trace file contains sequences of memory
access actions.  There are three types of actions: reads, writes, and
idle time.  The format of a read is:

<pre>R&lt;address&gt;:&lt;size&gt;</pre>

Where &lt;address&gt; is the address in hexadecimal and &lt;size&gt;
is the size of the memory access in hexadecimal.
Likewise, the format of a write is:

<pre>W&lt;address&gt;:&lt;size&gt;</pre>

Finally, the format of idle time is:

<pre>I&lt;cycles&gt;</pre>

Where &lt;cycles&gt; is the number of cycles in hexadecimal.

Building
------------------------------------------------------------------------------
Assuming GNAT is installed, simply run 'make'.

Implementation
------------------------------------------------------------------------------

memsim is implemented in Ada 2005.  There are two main package hierarchies:
the *Memory* package and the *Benchmark* package.  The *Parser* package
(which uses the *Lexer* package) is used to build up memories.  The
following memory packages are available:

 - *Memory*: The base package.
 - *Memory.Bank*: A memory bank which can contain multiple memories.
 - *Memory.Cache*: A write-back cache with various parameters.
 - *Memory.Dup*: Duplicate accesses to other memories.
 - *Memory.Flash*: A RAM with a separate write block size.
 - *Memory.Prefetch*: A memory prefetcher (assumes prefetches are free).
 - *Memory.RAM*: A simple RAM with constant access time.
 - *Memory.SPM*: A scratchpad memory.
 - *Memory.Stats*: A memory to track memory access statistics.
 - *Memory.Super*: A memory hierarchy optimizer.
 - *Memory.Trace*: A memory to write a memory access trace.
 - *Memory.Transform*: Base class for memory address transformers.
 - *Memory.Transform.Offset*: Transform memory addresses by an offset.
 - *Memory.Transform.Shift*: Transform memory addresses by shifting.

The following benchmark packages are available:

 - *Benchmark* The base package.
 - *Benchmark.Hash* A benchmark to generate random memory accesses.
 - *Benchmark.Heap* A benchmark to perform operations on a binary heap.
 - *Benchmark.Trace* A benchmark to process a memory trace.
 - *Benchmark.Stride* A benchmark to generate strided memory accesses.

