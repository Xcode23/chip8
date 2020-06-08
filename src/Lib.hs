module Lib 
          (
            ChipState(..),
            startChip,
            updateTimers
          ) where

import qualified Data.Vector as V
import qualified Data.Word as W
import SDL

data ChipState = Chip {
                        registers :: V.Vector W.Word8, -- 16 registers
                        index :: W.Word16, 
                        delayTimer :: W.Word8,
                        soundTimer :: W.Word8,
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
        delayTimer = 0,
        soundTimer = 0,
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
        newDelay = if newDelay > 0 && shouldUpdate then delayTimer chip - 1 else delayTimer chip
        newSound = if newSound > 0 && shouldUpdate then soundTimer chip - 1 else soundTimer chip
      in chip { timer = newTime, delayTimer = newDelay, soundTimer = newSound }

updateKeypad :: (Scancode -> Bool) -> ChipState -> ChipState
updateKeypad keyState chip = chip { keypad = newKeys }
  where newKeys = V.map keyState keyboard
  
keyboard = V.fromList [
  Scancode1,
  Scancode2,
  Scancode3,
  Scancode4,
  ScancodeQ,
  ScancodeW,
  ScancodeE,
  ScancodeR,
  ScancodeA,
  ScancodeS,
  ScancodeD,
  ScancodeF,
  ScancodeZ,
  ScancodeX,
  ScancodeC,
  ScancodeV
  ]

fontSet = V.fromList
  [0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
  ,0x20, 0x60, 0x20, 0x20, 0x70 -- 1
  ,0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
  ,0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
  ,0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
  ,0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
  ,0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
  ,0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
  ,0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
  ,0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
  ,0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
  ,0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
  ,0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
  ,0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
  ,0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
  ,0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
  ]