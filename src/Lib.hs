module Lib 
          (
            ChipState(..),
            startChip,
            updateTimers
          ) where

import qualified Data.Vector as V
import qualified Data.Word as W

data ChipState = Chip {
                        registers :: V.Vector W.Word8, -- 16 registers
                        index :: W.Word16, 
                        delay :: W.Word8,
                        sound :: W.Word8,
                        timer :: W.Word32,
                        pc :: W.Word16,
                        sp :: W.Word8,
                        gfx :: V.Vector Bool, -- 64x32 pixels 128x64 (super) Matrix?  keep? 
                        keypad :: V.Vector Bool,  -- 16 keys keep?
                        stack :: V.Vector W.Word16, -- 16 levels max
                        memory :: V.Vector W.Word8 -- 4096 bytes 4KB
                      } deriving (Show)

startChip :: V.Vector W.Word8 -> W.Word32 -> ChipState
startChip startingMemory startingTimer = 
  Chip {
        registers = V.replicate 16 0,
        index = 0, 
        delay = 0,
        sound = 0,
        timer = startingTimer,
        pc = 0,
        sp = 0,
        gfx = V.replicate 8192 False,
        keypad = V.replicate 16 False,
        stack = V.replicate 16 0,
        memory = startingMemory
       }

updateTimers :: W.Word32 -> ChipState -> ChipState
updateTimers newTime chip =
    let tick = ceiling (1.0 / 60.0 * 1000.0)
        oldTime = timer chip
        shouldUpdate = newTime > oldTime && newTime - tick >= oldTime
        newDelay = if newDelay > 0 && shouldUpdate then delay chip - 1 else delay chip
        newSound = if newSound > 0 && shouldUpdate then sound chip - 1 else sound chip
      in chip { timer = newTime, delay = newDelay, sound = newSound }
