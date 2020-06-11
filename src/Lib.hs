module Lib 
          (
            ChipState(..),
            startChip,
            updateTimers
          ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Word as W
import qualified Data.ByteString as BS
import Control.Monad.ST
import SDL
import SDL.Internal.Numbered

data ChipState = Chip {
                        registers :: V.Vector W.Word8, -- 16 registers
                        index :: W.Word16, 
                        delayTimer :: W.Word8,
                        soundTimer :: W.Word8,
                        timer :: W.Word32,
                        pc :: W.Word16,
                        sp :: W.Word8,
                        gfx :: V.Vector Bool, -- 128x64
                        keypad :: V.Vector Bool,  -- 16 keys
                        stack :: V.Vector W.Word16, -- 16 levels
                        memory :: V.Vector W.Word8 -- 4096 bytes
                      } deriving (Show)

startChip :: W.Word32 -> ChipState
startChip startingTimer = 
  Chip {
        registers = V.replicate 16 0,
        index = 0, 
        delayTimer = 0,
        soundTimer = 0,
        timer = startingTimer,
        pc = 0x200,
        sp = 0,
        gfx = V.replicate 8192 False,
        keypad = V.replicate 16 False,
        stack = V.replicate 16 0,
        memory = V.replicate 4096 0
       }

updateTimers :: W.Word32 -> ChipState -> ChipState
updateTimers newTime chip =
    let tick = ceiling (1.0 / 60.0 * 1000.0)
        oldTime = timer chip
        shouldUpdate = newTime > oldTime && newTime - tick >= oldTime
        updatedTimer = if shouldUpdate then newTime else oldTime
        newDelay = if newDelay > 0 && shouldUpdate then delayTimer chip - 1 else delayTimer chip
        newSound = if newSound > 0 && shouldUpdate then soundTimer chip - 1 else soundTimer chip
      in chip { timer = updatedTimer, delayTimer = newDelay, soundTimer = newSound }

updateKeypad :: (Scancode -> Bool) -> ChipState -> ChipState
updateKeypad keyState chip = chip { keypad = newKeys }
  where newKeys = V.map (keyState . fromNumber) keyboard

loadGame :: BS.ByteString -> ChipState -> ChipState
loadGame game chip = chip { memory = newMemory }
  where newMemory = runST $ do
          mutableGame <- V.thaw $ V.fromList $ BS.unpack game
          mutableMemory <- V.thaw (memory chip)
          let mutableMemorySlice = VM.slice 0x200 (VM.length mutableGame) mutableMemory
          VM.copy mutableMemorySlice mutableGame
          V.freeze mutableMemorySlice
  -- let listFile = BS.unpack game
  --     listMemory = replicate 512 0 ++ listFile ++ replicate (4096 - (512 + length listFile)) 0
  --  in chip { memory = V.fromList listMemory } 

interpretOpCode :: W.Word16 -> ChipState -> ChipState
interpretOpCode opCode chip
  | opCode == 0x00E0                      = chip
  | opCode == 0x00EE                      = chip
  | opCode >= 0x0000 && opCode <= 0x0FFF  = chip --noOp
  | opCode >= 0x1000 && opCode <= 0x1FFF  = chip
  | opCode >= 0x2000 && opCode <= 0x2FFF  = chip
  | opCode >= 0x3000 && opCode <= 0x3FFF  = chip
  | opCode >= 0x4000 && opCode <= 0x4FFF  = chip
  | opCode >= 0x5000 && opCode <= 0x5FFF  = chip
  | opCode >= 0x6000 && opCode <= 0x6FFF  = chip
  | opCode >= 0x7000 && opCode <= 0x7FFF  = chip
  | opCode >= 0x8000 && opCode <= 0x8FFF  = 
      case opCode `mod` 0x10 of
        0x0 -> chip
        0x1 -> chip
        0x2 -> chip
        0x3 -> chip
        0x4 -> chip
        0x5 -> chip
        0x6 -> chip
        0x7 -> chip
        0xE -> chip
        _   -> chip
  | opCode >= 0x9000 && opCode <= 0x9FFF  = chip
  | opCode >= 0xA000 && opCode <= 0xAFFF  = chip
  | opCode >= 0xB000 && opCode <= 0xBFFF  = chip
  | opCode >= 0xC000 && opCode <= 0xCFFF  = chip
  | opCode >= 0xD000 && opCode <= 0xDFFF  = chip
  | opCode >= 0xE000 && opCode <= 0xEFFF  = 
      case opCode `mod` 0x100 of
        0x9E -> chip
        0xA1 -> chip
        _    -> chip
  | opCode >= 0xF000 && opCode <= 0xFFFF  =
      case opCode `mod` 0x100 of
        0x07 -> chip
        0x0A -> chip
        0x15 -> chip
        0x18 -> chip
        0x1E -> chip
        0x29 -> chip
        0x33 -> chip
        0x55 -> chip
        0x65 -> chip
        _    -> chip
  | otherwise = chip

keyboard :: V.Vector W.Word32  
keyboard = V.fromList $ map toNumber [
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

fontSet :: V.Vector W.Word8
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