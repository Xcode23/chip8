module Chip8 
          (
            ChipState(..),
            startChip,
            updateTimers
          ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Word as W
import Data.Bits
import qualified Data.ByteString as BS
import Control.Monad.ST
import Control.Monad
import SDL
import SDL.Internal.Numbered
import Utils
import System.Random

data ChipState = Chip {
                        registers :: V.Vector W.Word8, -- 16 registers
                        flagRegister :: Bool, -- VF
                        index :: W.Word16, 
                        delayTimer :: W.Word8,
                        soundTimer :: W.Word8,
                        timer :: W.Word32,
                        pc :: W.Word16,
                        sp :: W.Word8,
                        gfx :: V.Vector Bool, -- 128x64
                        keypad :: V.Vector Bool,  -- 16 keys
                        stack :: V.Vector W.Word16, -- 16 levels
                        memory :: V.Vector W.Word8, -- 4096 bytes
                        seed :: StdGen
                      } deriving (Show)

tickDuration = ceiling (1.0 / 60.0 * 1000.0)

startChip :: W.Word32 -> IO ChipState
startChip startingTimer = do
  random <- newStdGen 
  return $ Chip {
        registers = V.replicate 15 0,
        flagRegister = False,
        index = 0, 
        delayTimer = 0,
        soundTimer = 0,
        timer = startingTimer,
        pc = 0x200,
        sp = 0,
        gfx = V.replicate 8192 False,
        keypad = V.replicate 16 False,
        stack = V.replicate 16 0,
        memory = V.concat [fontSet, V.replicate (4096 - V.length fontSet) 0],
        seed = random
       }

updateTimers :: ChipState -> IO ChipState
updateTimers chip = do
    newTime <- SDL.ticks
    when (soundTimer chip == 0) (print $ fromIntegral newTime / 1000.0)
    let oldTime = timer chip
        shouldUpdate = newTime > oldTime && newTime - tickDuration >= oldTime
      in  if shouldUpdate 
          then return $ auxUpdateTimers newTime chip
          else return chip

auxUpdateTimers :: W.Word32 -> ChipState -> ChipState
auxUpdateTimers newTime chip =
  let oldTime = timer chip
      updatedTimer = newTime - (newTime `mod` tickDuration)
      decrementValue = div (newTime - oldTime) tickDuration
      newDelay = decrementTimer (delayTimer chip) decrementValue
      newSound = decrementTimer (soundTimer chip) decrementValue
    in chip { timer = updatedTimer, delayTimer = newDelay, soundTimer = newSound }

decrementTimer :: (Ord a, Num a, Integral b) => a -> b -> a
decrementTimer oldTimer decrementValue =
  if oldTimer > 0
    then max 0 oldTimer - fromIntegral decrementValue
    else oldTimer

updateKeypad :: (Scancode -> Bool) -> ChipState -> ChipState
updateKeypad keyState chip = chip { keypad = newKeys }
  where newKeys = V.map (keyState . fromNumber) keyboard

loadGame :: BS.ByteString -> ChipState -> ChipState
loadGame game chip = chip { memory = newMemory }
  where newMemory = writeVectorToVector (memory chip) 0x200 (V.fromList $ BS.unpack game) 

executeNextInstruction :: ChipState -> ChipState
executeNextInstruction chip = 
  let address = pc chip
      incrementedChip = chip { pc = address + 2 }
      byte1 = accessMemory chip address * 0x100
      opCode = byte1 + accessMemory chip (address + 1)
    in interpretOpCode opCode incrementedChip

interpretOpCode :: W.Word16 -> ChipState -> ChipState
interpretOpCode opCode
  | opCode == 0x00E0                      = clearDisplayOp
  | opCode == 0x00EE                      = returnOp
  | opCode >= 0x0000 && opCode <= 0x0FFF  = id --noOp
  | opCode >= 0x1000 && opCode <= 0x1FFF  = jumpOp opCode
  | opCode >= 0x2000 && opCode <= 0x2FFF  = callOp opCode
  | opCode >= 0x3000 && opCode <= 0x3FFF  = skipEqualValueOp opCode
  | opCode >= 0x4000 && opCode <= 0x4FFF  = skipNotEqualValueOp opCode
  | opCode >= 0x5000 && opCode <= 0x5FFF  = skipEqualRegOp opCode
  | opCode >= 0x6000 && opCode <= 0x6FFF  = loadValueOp opCode
  | opCode >= 0x7000 && opCode <= 0x7FFF  = addValueOp opCode
  | opCode >= 0x8000 && opCode <= 0x8FFF  = 
      case opCode `mod` 0x10 of
        0x0 -> loadRegOp opCode
        0x1 -> orRegOp opCode
        0x2 -> andRegOp opCode
        0x3 -> xorRegOp opCode
        0x4 -> addRegOp opCode
        0x5 -> subRegOp opCode
        0x6 -> rightShiftRegOp opCode
        0x7 -> subnRegOp opCode
        0xE -> leftShiftRegOp opCode
        _   -> id
  | opCode >= 0x9000 && opCode <= 0x9FFF  = skipNotEqualRegOp opCode
  | opCode >= 0xA000 && opCode <= 0xAFFF  = setIndexWithValueOp opCode
  | opCode >= 0xB000 && opCode <= 0xBFFF  = jumpV0Op opCode
  | opCode >= 0xC000 && opCode <= 0xCFFF  = randomAndOp opCode
  | opCode >= 0xD000 && opCode <= 0xDFFF  = id
  | opCode >= 0xE000 && opCode <= 0xEFFF  = 
      case opCode `mod` 0x100 of
        0x9E -> id
        0xA1 -> id
        _    -> id
  | opCode >= 0xF000 && opCode <= 0xFFFF  =
      case opCode `mod` 0x100 of
        0x07 -> id
        0x0A -> id
        0x15 -> id
        0x18 -> id
        0x1E -> id
        0x29 -> id
        0x33 -> id
        0x55 -> id
        0x65 -> id
        _    -> id
  | otherwise = id

clearDisplayOp :: ChipState -> ChipState
clearDisplayOp chip = chip { gfx = V.replicate 8192 False }

returnOp :: ChipState -> ChipState
returnOp chip = 
  chip { 
    pc = readFromVector (stack chip) $ sp chip, 
    sp = sp chip - 1
  }

jumpOp :: W.Word16 -> ChipState -> ChipState
jumpOp opCode chip = chip { pc = opCode `mod` 0x1000 }

callOp :: W.Word16 -> ChipState -> ChipState
callOp opCode chip = 
  chip { 
    pc = opCode `mod` 0x1000,
    sp = sp chip + 1
  }

skipEqualValueOp :: W.Word16 -> ChipState -> ChipState
skipEqualValueOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      value = fromIntegral $ opCode `mod` 0x100
    in  chip {
          pc = if vx == value then pc chip + 2 else pc chip
        }

skipNotEqualValueOp :: W.Word16 -> ChipState -> ChipState
skipNotEqualValueOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      value = fromIntegral $ opCode `mod` 0x100
    in  chip {
          pc = if vx /= value then pc chip + 2 else pc chip
        }

skipEqualRegOp :: W.Word16 -> ChipState -> ChipState
skipEqualRegOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
    in  chip {
          pc = if vx == vy then pc chip + 2 else pc chip
        }

skipNotEqualRegOp :: W.Word16 -> ChipState -> ChipState
skipNotEqualRegOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
    in  chip {
          pc = if vx /= vy then pc chip + 2 else pc chip
        }

loadValueOp :: W.Word16 -> ChipState -> ChipState
loadValueOp opCode chip =
  let index = fromIntegral (opCode `div` 0x100 `mod` 0x10)
      value = fromIntegral $ opCode `mod` 0x100
      newRegisters = writeValueToVector (registers chip) index value
    in chip { registers = newRegisters }

loadRegOp :: W.Word16 -> ChipState -> ChipState
loadRegOp opCode chip =
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector (registers chip) index vy
    in chip { registers = newRegisters }

addValueOp :: W.Word16 -> ChipState -> ChipState
addValueOp opCode chip =
  let index = fromIntegral (opCode `div` 0x100 `mod` 0x10)
      value = fromIntegral $ opCode `mod` 0x100
      newRegisters = modifyValueInVector (registers chip)  (+ value) index
    in chip { registers = newRegisters } 

orRegOp :: W.Word16 -> ChipState -> ChipState
orRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (registers chip) (.|. vy) index
    in chip { registers = newRegisters }

andRegOp :: W.Word16 -> ChipState -> ChipState
andRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (registers chip) (.&. vy) index
    in chip { registers = newRegisters }

xorRegOp :: W.Word16 -> ChipState -> ChipState
xorRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (registers chip) (xor vy) index
    in chip { registers = newRegisters }

addRegOp :: W.Word16 -> ChipState -> ChipState
addRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector (registers chip) index (vx + vy)
    in  chip { 
          registers = newRegisters,
          flagRegister = vx > (255 - vy)
        }

subRegOp :: W.Word16 -> ChipState -> ChipState
subRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector (registers chip) index (vx - vy)
    in  chip { 
          registers = newRegisters,
          flagRegister = vx > vy
        }

rightShiftRegOp :: W.Word16 -> ChipState -> ChipState
rightShiftRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      newRegisters = writeValueToVector (registers chip) index (shiftR vx 1)
    in  chip { 
          registers = newRegisters,
          flagRegister = (vx `mod` 2) == 1
        }

leftShiftRegOp :: W.Word16 -> ChipState -> ChipState
leftShiftRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      newRegisters = writeValueToVector (registers chip) index (shiftL vx 1)
    in  chip { 
          registers = newRegisters,
          flagRegister = vx >= 128
        }

subnRegOp :: W.Word16 -> ChipState -> ChipState
subnRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector (registers chip) index (vy - vx)
    in  chip { 
          registers = newRegisters,
          flagRegister = vy > vx
        }

setIndexWithValueOp :: W.Word16 -> ChipState -> ChipState
setIndexWithValueOp opCode chip =
  let value = opCode `mod` 0x1000
    in chip { index = value }

jumpV0Op :: W.Word16 -> ChipState -> ChipState
jumpV0Op opCode chip =
  let value = opCode `mod` 0x1000
      v0 = fromIntegral $ accessRegister chip 0
    in chip { pc = value + v0 }

randomAndOp :: W.Word16 -> ChipState -> ChipState
randomAndOp opCode chip =
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      value = fromIntegral $ opCode `mod` 0x100
      (rand, newStdGen) = random $ seed chip :: (W.Word8, StdGen)
      newRegisters = writeValueToVector (registers chip) index (rand .&. value)
    in chip { registers = newRegisters, seed = newStdGen }

accessRegister :: Integral a => ChipState -> a -> W.Word8
accessRegister chip= readFromVector (registers chip)

accessMemory :: Integral a => ChipState -> a -> W.Word16
accessMemory chip index = fromIntegral $ readFromVector (memory chip) index

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
