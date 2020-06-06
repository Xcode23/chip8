module Lib 
          (

          ) where

import qualified Data.Vector as V
import qualified Data.Word as W

data ChipState = Chip {
                        getRegisters :: V.Vector W.Word8, -- 16 registers
                        getIndex :: W.Word16, 
                        getDelay :: W.Word8,
                        getSound :: W.Word8,
                        getPC :: W.Word16,
                        getSP :: W.Word8,
                        getGFX :: V.Vector Bool, -- 64x32 pixels 128x64 (super) Matrix?  keep? 
                        getKeypad :: V.Vector Bool,  -- 16 keys keep?
                        getStack :: V.Vector W.Word16, -- 16 levels max
                        getMemory :: V.Vector W.Word8 -- 4096 bytes 4KB
                      }

--load file chip = 
