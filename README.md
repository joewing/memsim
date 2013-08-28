
Memory Simulator
==============================================================================
This is a memory simulator and memory subsystem optimizer.  It is capable
of optimizing memory subsystems for a particular main memory system.  It
can then generate synthesizable VHDL for implementation on FPGAs.

Building
------------------------------------------------------------------------------
Assuming GNAT is installed, simply run 'make'.
Note that GNAT is part of GCC: http://gcc.gnu.org.

Usage
------------------------------------------------------------------------------
There are two programs of interest: memsim and memgen.  memsim
is the memory simulator and subsystem optimizer.  It takes a main memory
specification and generates an optimized memory subsystem using memory
traces or one of the built-in benchmarks.  memgen takes a memory
subsystem specification as input and generates synthesizable VHDL.

Running memsim with no arguments will show its usage:

<pre>
usage: ./memsim [&lt;options&gt;] &lt;memory&gt; { &lt;benchmark&gt; [&lt;options&gt;] }
options:
   -addr\_bits The number of bits in an address
   -device     The device type
benchmarks:
   hash [size=1024][iterations=1000][spacing=0][seed=15]
   heap [size=1024][spacing=0][seed=15]
   mm [size=256][iterations=1][spacing=0][seed=15]
   stride [size=1024][iterations=1000][stride=1][spacing=0]
   trace [file=trace.txt][spacing=0]
   show
</pre>

The &lt;memory&gt; parameter specifies a memory description file.  This
file contains the memory system to simulate.  If the special *super*
component is used, the simulator will generate a superoptimized
memory subsystem (see mem/opt.mem for an example).

The &lt;benchmark&gt; parameter specifies the benchmark to use.
The following benchmarks are available:

 - *hash*: A benchmark to generate random 4-byte memory accesses.
 - *heap*: A benchmark to perform random insertions and deletions of
   4-byte items in a full binary heap.
 - *mm*: A benchmark to simulate matrix-matrix multiplication on 4-byte
   values.
 - *stride*: A benchmark to perform strided 4-byte memory accesses.
 - *trace*: A benchmark to execute a memory access trace.
 - *show*: A benchmark that simply shows information about the memory.

The optional arguments are shown above for each benchmark with defaults.
The arguments are:

 - *size*: The number of items in the data structure.
 - *iterations*: The number of iterations to perform.
 - *spacing*: The number of cycles to insert between each memory access.
 - *seed*: The random number seed to use.
 - *stride*: The stride size in 4-byte words for the *stride* benchmark.
 - *file*: The name of the file to open for the *trace* benchmark.

For the *trace* benchmark, the trace file contains sequences of memory
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

Memories
------------------------------------------------------------------------------
Memories are specified using s-expressions.  The type of memory comes first
and then pairs containing argument name/value follow.
For example, a simple cached memory would look as follows:

<pre>
(cache (latency 1) (line_size 8) (line_count 1024) (associativity 4)
   (memory (ram (latency 100))))
</pre>

See the *mem* subdirectory for more examples.

There are two types of memories: components and main memories.
The main memories are:

 - ram: A RAM simulator with a fixed word size and latency.  Takes the
   following parameters:
   - word\_size: The size of a word in bytes (defaults to 8).
   - latency: The latency of an access (defaults to 1).
   - word\_count: The total number of words
     (defaults to 0, this is only for VHDL simulation).
   - burst: The latency for accessing words after the first word
     (defaults to 0, which disables burst behavior).
 - flash: A Flash memory simulator with a separate block write size and
   latency.  Takes the following parameters:
   - word\_size: The size of a word in bytes (defaults to 8).
   - block\_size: The size of a block in bytes (defaults to 256).
   - read\_latency: The latency of a read (defaults to 10).
   - write\_latency: The latency of a write (defaults to 1000).
 - option: A list of main memories from which the superoptimizer can
   select.

The following memory components are available:

 - cache: Cache.  The following parameters are available:
   - memory: The contained memory.
   - line\_count: The number of cache lines (1).
   - line\_size: The size of each cache line in bytes (8).
   - associativity: The set associativity of the cache (1).
   - latency: The latency (3).
   - policy: The replacement policy (lru, mru, fifo, or random,
     defaults is lru).
   - write\_back: Set if the cache should use write back instead of
     write through (defaults to false).
 - dup: Access duplicator.  This can be used to test several memories at
   once. Takes a list of memories.
 - perfect\_prefetch: Perfect prefetcher.  Takes the following parameter:
   - memory: The contained memory.
 - prefetch: Strided prefetcher.  Prefetches the address computed by
   (address + stride) Takes the following parameters:
   - memory: The contained memory.
   - stride: The stride size (defaults to 1).
 - spm: Scratchpad memory.  Takes the following parameters:
   - memory: The contained memory.
   - size: The size of the scratchpad in bytes (defaults to 0).
   - latency: The latency of a hit in the scratchpad.
 - stats: Access statistics logger.  Takes the following parameter:
   - memory: The contained memory.
 - super: Memory subsystem superoptimizer.
   Takes the following parameters:
   - memory: The contained memory.  Basic memory components are not changed
     by the superoptimizer, but any other components are only used as the
     initial state.
   - optimize: The optimization target (time or writes, defaults to time).
   - max\_cost: The maximum cost in block RAMs (defaults to 0).
   - max\_iterations: The maximum iterations to run without improvement.
   - seed: Random number seed (defaults to 15).
   - permute\_only: Set to "true" to restrict the optimizer to permuting
     an existing memory subsystem (defaults to "false").
 - trace: Access trace logger.  Takes the following parameter:
   - memory: The contained memory.
 - offset: Address offset transform.  Takes the following parameters:
   - bank: The memory to see the transformed addresses (if present).
     If not present, addresses are transformed for the memory parameter.
   - memory: The contained memory.
   - value: A value to add to the address.
 - shift: Address shift transform.  Takes the following parameters:
   - bank: The memory to see the transformed addresses (if present).
     If not present, addresses are transformed for the memory parameter.
   - memory: The contained memory.
   - value: The number of bits to shift left.
 - eor: Address XOR transform.  Takes the following parameters:
   - bank: The memory to see the transformed addresses (if present).
     If not present, addresses are transformed for the memory parameter.
   - memory: The contained memory.
   - value: The value to XOR with the address.
 - flip: Address flip transform.  Takes the following parameters:
   - bank: The memory to see the transformed addresses (if present).
     If not present, addresses are transformed for the memory parameter.
   - memory: The contained memory.

Implementation
------------------------------------------------------------------------------
memsim is implemented in Ada 2005.  There are two main package hierarchies:
the *Memory* package to describe memories and the *Benchmark* package
to describe benchmarks.  The *Parser* package (which uses the *Lexer*
package) is used to build up memories.  In addition, the *Test* package
contains unit tests for the various components.

Testing
------------------------------------------------------------------------------
The memsim program includes a set of unit tests.  To execute the unit
tests run './memsim test'.

