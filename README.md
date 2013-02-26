
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
  trace [file=trace.txt] [iterations=1] [spacing=100]
</pre>

The &lt;memory&gt; parameter specifies a memory description file.

The &lt;benchmark&gt; parameter specifies the benchmark to use.
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

Memories
------------------------------------------------------------------------------
Memories are specified using s-expressions.  The type of memory comes first
and then pairs containing argument (name, value) follow.  
For example, a simple cached memory would look as follows:

<pre>
(cache (latency 1) (line_size 8) (line_count 1024) (associativity 4)
   (memory (ram (latency 100))))
</pre>

See the *mem* subdirectory for more examples.

There are two types of memories: containers and basic memories.
The basic memories are:

 - ram: A RAM simulator with a fixed word size and latency.  Takes the
   following parameters:
   - word\_size: The size of a word in bytes (defaults to 8).
   - latency: The latency of an access (defaults to 1).
 - flash: A Flash memory simulator with a separate block write size and
   latency.  Takes the following parameters:
   - word\_size: The size of a word in bytes (defaults to 8).
   - block\_size: The size of a block in bytes (defaults to 256).
   - read\_latency: The latency of a read (defaults to 10).
   - write\_latency: The latency of a write (defaults to 1000).

The following memory containers are available:

 - bank: Memory bank. Divide memory accesses based on address.  Banks take
   a list for the banks.  Each bank must specify a memory, key, and mask.
   The mask is a bit mask for the address and the key is the value that
   the masked address is tested against.
 - cache: Cache.  The following parameters are available:
   - memory: The contained memory.
   - line\_count: The number of cache lines (1).
   - line\_size: The size of each cache line in bytes (8).
   - associativity: The set associativity of the cache (1).
   - latency: The latency of a cache hit (1).
   - policy: The replacement policy (lru, mru, fifo, or random,
     defaults is lru).
   - exclusive: Determine if read values are cached (defaults to true).
   - write\_back: Set if the cache should use write back instead of
     write through (defaults to false).
 - dup: Access duplicator.  This can be used to test several memories at
   once. Takes a list of memories.
 - perfect\_prefetch: Perfect prefetcher.  Takes the following parameter:
   - memory: The contained memory.
 - prefetch: Strided prefetcher.  Prefetches the address computed by
   (address * multiplier + stride) Takes the following parameters:
   - memory: The contained memory.
   - stride: The stride size (defaults to 1).
   - multiplier: The stride multiplier (defaults to 1).
 - spm: Scratchpad memory.  Takes the following parameters:
   - memory: The contained memory.
   - size: The size of the scratchpad (defaults to 0).
   - latency: The latency of a hit in the scratchpad.
 - stats: Access statistics logger.  Takes the following parameter:
   - memory: The contained memory.
 - super: Memory hierarchy optimizer (for use with the trace benchmark).
   Takes the following parameters:
   - memory: The contained memory.
   - optimize: The optimization target (time or writes, defaults to time).
   - max\_cost: The maximum cost in transistors (defaults to 0).
 - trace: Access trace logger.  Takes the following parameter:
   - memory: The contained memory.
 - transform: Address transformer.  Takes the following parameters:
   - memory: The contained memory.
   - offset: An address offset (exactly one of shift or offset must be
     specified).
   - shift: A number of bits to rotate the address left.

Building
------------------------------------------------------------------------------
Assuming GNAT is installed, simply run 'make'.

Implementation
------------------------------------------------------------------------------

memsim is implemented in Ada 2005.  There are two main package hierarchies:
the *Memory* package to describe memories and the *Benchmark* package
to describe benchmarks.  The *Parser* package (which uses the *Lexer*
package) is used to build up memories.

