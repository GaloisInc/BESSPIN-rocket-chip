
package galois.devices

import freechips.rocketchip.devices.debug.dtmJTAGAddrs

// Custom implementation of dtmJtagAddrs to support our particular instantiation of Xilinx BSCANE primitives
class xilinxAddrs extends dtmJTAGAddrs (
  IDCODE       = 0x002924,
  DTM_INFO     = 0x022924,
  DMI_ACCESS   = 0x003924
)
