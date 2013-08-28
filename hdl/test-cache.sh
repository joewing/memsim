#!/bin/bash

MEM="(ram (word_size 8)(latency 10)(burst 0))"

function run()
{
   make run-tb
   if [[ $? -ne 0 ]] ; then
      echo "Run failed"
      exit -1
   fi
}

for ((lsize=8; lsize<=32; lsize=lsize*2)) ; do
   for ((lcount=1; lcount<=16; lcount=lcount*2)) ; do
      for ((a=1; a<=$lcount; a=a*2)) ; do
         for p in 'lru' 'mru' 'fifo' 'plru' ; do
            for wb in 'true' 'false' ; do
               echo -n "(cache (line_size $lsize)" > input.mem
               echo -n "(line_count $lcount)"      >> input.mem
               echo -n "(associativity $a)"        >> input.mem
               echo -n "(policy $p)"               >> input.mem
               echo -n "(write_back $wb)"          >> input.mem
               echo "(memory $MEM))"               >> input.mem
               ../memgen input.mem > mem.vhdl
               run
            done
         done
      done
   done
done
echo "Success"

