# process_trace.py
#
# Simple script that takes an output text file from simulating the rocket core
# and produced a tandem verification trace compatible with Bluespec tools
#
# Usage: python process_trace.py input.out trace_file.bin
#
# Dylan Hand <dhand@galois.com>

import sys
import re
import binascii

prog = re.compile("\[DUT\] \[HEX\] \[([\d\s]+)\] \s*(\S*)")

infile = sys.argv[1]
outfile = sys.argv[2]

print("Converting %s to %s ..." % (infile, outfile))

with open(infile, 'r') as f:
   with open(outfile, 'wb') as o:
      for line in f.readlines():
         if "HEX" in line:
            result = prog.match(line)
            if result:
               count = int(result.group(1))
               hex = result.group(2)
               rhex = "".join(list(reversed([hex[i:i+2] for i in range(0, len(hex), 2)]))[0:count])
               #print(rhex)
               o.write(binascii.unhexlify(rhex))
