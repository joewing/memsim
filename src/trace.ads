
with Memory; use Memory;

package Trace is

   -- Memory traces are sequences of memory access actions separated by
   -- new lines.  The format of each line is:
   --
   --    <action> <value>
   --
   -- where <action> is 'R' for read, 'W' for write, and 'I' for idle.
   -- For reads and writes, <value> is a decimal value indicating the
   -- address.  For idle states, <value> is the number of idle cycles.
   -- Idle is used to separate reads and writes.

   -- Process a memory trace.
   procedure Process(mem      : in out Memory_Type'class;
                     name     : in String;
                     spacing  : in Time_Type := 1);

end Trace;

