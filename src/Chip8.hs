module Chip8 
          (
            ChipState(..),
            startChip,
            getScreen,
            loadGame,
            run
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
--import Debug.Trace --traceShowId

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
                        memory :: V.Vector W.Word8, -- 4096 bytes
                        seed :: StdGen
                      } deriving (Show)

instance Eq ChipState where
  (==) chip1 chip2 =
    equals registers 
    && equals index 
    && equals delayTimer
    && equals soundTimer
    && equals timer
    && equals pc
    && equals sp
    && equals gfx
    && equals keypad
    && equals stack
    && equals memory
      where equals func = func chip1 == func chip2

tickDuration = ceiling (1.0 / 60.0 * 1000.0)
screenSize = 8192
memorySize = 4096
registersSize = 16
keypadSize = 16
stackSize = 16
gameStartAddress = 0x200
spriteWidth = 8
screenWidth = 128

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

startChip :: IO ChipState
startChip = do
  random <- newStdGen 
  startingTimer <- SDL.ticks 
  return $ Chip {
        registers = V.replicate registersSize 0,
        index = 0, 
        delayTimer = 0,
        soundTimer = 0,
        timer = startingTimer,
        pc = gameStartAddress,
        sp = 0,
        gfx = V.replicate screenSize False,
        keypad = V.replicate keypadSize False,
        stack = V.replicate stackSize 0,
        memory = V.concat [fontSet, V.replicate (memorySize - V.length fontSet) 0],
        seed = random
       }

run :: ChipState -> IO ChipState
run chip = do
  let newChip = executeNextInstruction chip
  -- if previously run instruction was wait for key then newChip == key and timers should not be updated
  let updatedTimersChipIO = if chip /= newChip then updateTimers newChip else return newChip
  updatedTimersChip <- updatedTimersChipIO
  pumpEvents
  keyState <- getKeyboardState
  return $ updateKeypad keyState updatedTimersChip
   
updateTimers :: ChipState -> IO ChipState
updateTimers chip = do
    newTime <- SDL.ticks
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
  where newMemory = writeVectorToVector gameStartAddress (V.fromList $ BS.unpack game) $ memory chip 

executeNextInstruction :: ChipState -> ChipState
executeNextInstruction chip = 
  let address = pc chip
      byte1 = accessMemory chip address * 0x100
      opCode = byte1 + accessMemory chip (address + 1)
    in interpretOpCode opCode chip

interpretOpCode :: W.Word16 -> ChipState -> ChipState
interpretOpCode opCode
  | opCode == 0x00E0                      = clearDisplayOp
  | opCode == 0x00EE                      = returnOp
  | opCode >= 0x0000 && opCode <= 0x0FFF  = noOp
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
        _   -> noOp
  | opCode >= 0x9000 && opCode <= 0x9FFF  = skipNotEqualRegOp opCode
  | opCode >= 0xA000 && opCode <= 0xAFFF  = setIndexWithValueOp opCode
  | opCode >= 0xB000 && opCode <= 0xBFFF  = jumpV0Op opCode
  | opCode >= 0xC000 && opCode <= 0xCFFF  = randomAndOp opCode
  | opCode >= 0xD000 && opCode <= 0xDFFF  = drawOp opCode
  | opCode >= 0xE000 && opCode <= 0xEFFF  = 
      case opCode `mod` 0x100 of
        0x9E -> skipEqualKeyOp opCode
        0xA1 -> skipNotEqualKeyOp opCode
        _    -> noOp
  | opCode >= 0xF000 && opCode <= 0xFFFF  =
      case opCode `mod` 0x100 of
        0x07 -> id
        0x0A -> waitForKeyOp opCode
        0x15 -> id
        0x18 -> id
        0x1E -> id
        0x29 -> id
        0x33 -> id
        0x55 -> id
        0x65 -> id
        _    -> noOp
  | otherwise = noOp

noOp :: ChipState -> ChipState
noOp chip = chip { pc = pc chip + 2 }

clearDisplayOp :: ChipState -> ChipState
clearDisplayOp chip = chip { gfx = V.replicate screenSize False, pc = pc chip + 2 }

returnOp :: ChipState -> ChipState
returnOp chip = 
  chip { 
    pc = accessStack chip $ sp chip, 
    sp = sp chip - 1
  }

jumpOp :: W.Word16 -> ChipState -> ChipState
jumpOp opCode chip = chip { pc = opCode `mod` 0x1000 }

callOp :: W.Word16 -> ChipState -> ChipState
callOp opCode chip = 
  let newSP = sp chip + 1
      returnPC = fromIntegral $ pc chip + 2
      newStack = writeValueToVector newSP returnPC $ stack chip
  in  chip { 
        pc = opCode `mod` 0x1000,
        sp = newSP,
        stack = newStack
      }

skipEqualValueOp :: W.Word16 -> ChipState -> ChipState
skipEqualValueOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      value = fromIntegral $ opCode `mod` 0x100
    in  chip {
          pc = if vx == value then pc chip + 4 else pc chip + 2
        }

skipNotEqualValueOp :: W.Word16 -> ChipState -> ChipState
skipNotEqualValueOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      value = fromIntegral $ opCode `mod` 0x100
    in  chip {
          pc = if vx /= value then pc chip + 4 else pc chip + 2
        }

skipEqualRegOp :: W.Word16 -> ChipState -> ChipState
skipEqualRegOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
    in  chip {
          pc = if vx == vy then pc chip + 4 else pc chip + 2
        }

skipNotEqualRegOp :: W.Word16 -> ChipState -> ChipState
skipNotEqualRegOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
    in  chip {
          pc = if vx /= vy then pc chip + 4 else pc chip + 2
        }

loadValueOp :: W.Word16 -> ChipState -> ChipState
loadValueOp opCode chip =
  let index = fromIntegral (opCode `div` 0x100 `mod` 0x10)
      value = fromIntegral $ opCode `mod` 0x100
      newRegisters = writeValueToVector index value $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 }

loadRegOp :: W.Word16 -> ChipState -> ChipState
loadRegOp opCode chip =
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector index vy $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 }

addValueOp :: W.Word16 -> ChipState -> ChipState
addValueOp opCode chip =
  let index = fromIntegral (opCode `div` 0x100 `mod` 0x10)
      value = fromIntegral $ opCode `mod` 0x100
      newRegisters = modifyValueInVector (+ value) index $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 } 

orRegOp :: W.Word16 -> ChipState -> ChipState
orRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (.|. vy) index $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 }

andRegOp :: W.Word16 -> ChipState -> ChipState
andRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (.&. vy) index $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 }

xorRegOp :: W.Word16 -> ChipState -> ChipState
xorRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = modifyValueInVector (xor vy) index $ registers chip
    in chip { registers = newRegisters, pc = pc chip + 2 }

addRegOp :: W.Word16 -> ChipState -> ChipState
addRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector index (vx + vy) $ registers chip
      vf = if vx > (255 - vy) then 1 else 0
      newRegistersWithVf = writeValueToVector 0xF vf newRegisters
    in  chip { 
          registers = newRegistersWithVf,
          pc = pc chip + 2
        }

subRegOp :: W.Word16 -> ChipState -> ChipState
subRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector index (vx - vy) $ registers chip
      vf = if vx > vy then 1 else 0
      newRegistersWithVf = writeValueToVector 0xF vf newRegisters
    in  chip { 
          registers = newRegistersWithVf,
          pc = pc chip + 2
        }

rightShiftRegOp :: W.Word16 -> ChipState -> ChipState
rightShiftRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      newRegisters = writeValueToVector index (shiftR vx 1) $ registers chip
      vf = if (vx `mod` 2) == 1 then 1 else 0
      newRegistersWithVf = writeValueToVector 0xF vf newRegisters
    in  chip { 
          registers = newRegistersWithVf,
          pc = pc chip + 2
        }

leftShiftRegOp :: W.Word16 -> ChipState -> ChipState
leftShiftRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      newRegisters = writeValueToVector index (shiftL vx 1) $ registers chip
      vf = if vx >= 128 then 1 else 0
      newRegistersWithVf = writeValueToVector 0xF vf newRegisters 
    in  chip { 
          registers = newRegistersWithVf,
          pc = pc chip + 2
        }

subnRegOp :: W.Word16 -> ChipState -> ChipState
subnRegOp opCode chip = 
  let index = (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
      vx = accessRegister chip index
      vy = accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit
      newRegisters = writeValueToVector index (vy - vx) $ registers chip
      vf = if vy > vx then 1 else 0
      newRegistersWithVf = writeValueToVector 0xF vf newRegisters
    in  chip { 
          registers = newRegistersWithVf,
          pc = pc chip + 2
        }

setIndexWithValueOp :: W.Word16 -> ChipState -> ChipState
setIndexWithValueOp opCode chip =
  let value = opCode `mod` 0x1000
    in chip { index = value, pc = pc chip + 2 }

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
      newRegisters = writeValueToVector index (rand .&. value) $ registers chip 
    in chip { registers = newRegisters, seed = newStdGen, pc = pc chip + 2 }

waitForKeyOp :: W.Word16 -> ChipState -> ChipState
waitForKeyOp opCode chip =
  case V.elemIndex True (keypad chip) of
    Nothing  -> chip
    Just key -> chip { 
                  registers = writeValueToVector index wordKey $ registers chip, 
                  pc = pc chip + 2 
                }
      where index = opCode `div` 0x100 `mod` 0x10 -- second opCode hex digit
            wordKey = fromIntegral key

skipEqualKeyOp :: W.Word16 -> ChipState -> ChipState
skipEqualKeyOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
    in chip {
          pc = if accessKeypad chip vx then pc chip + 4 else pc chip + 2
        }

skipNotEqualKeyOp :: W.Word16 -> ChipState -> ChipState
skipNotEqualKeyOp opCode chip =
  let vx = accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
    in chip {
          pc = if not (accessKeypad chip vx) then pc chip + 4 else pc chip + 2
        }

drawOp :: W.Word16 -> ChipState -> ChipState
drawOp opCode chip = 
  chip { 
    gfx = newScreen,
    registers = newRegisters,
    pc = pc chip + 2 
  }
  where newScreen = modifyListInVector xor sprite $ gfx chip
        newRegisters = writeValueToVector 0xF vf $ registers chip
        vf = if checkCollision chip sprite then 1 else 0
        sprite = getSprite chip (index chip) size position
        size = fromIntegral $ opCode `mod` 0x10 -- fourth opCode hex digit
        position = (vx, vy)
        vx = fromIntegral $ accessRegister chip (opCode `div` 0x100 `mod` 0x10) -- second opCode hex digit
        vy = fromIntegral $ accessRegister chip (opCode `div` 0x10 `mod` 0x10) -- third opCode hex digit

checkCollision :: ChipState -> [(Int, Bool)] -> Bool
checkCollision _ [] = False
checkCollision chip ((index, value) : next) = (value && currentScreenValue) || checkCollision chip next
  where currentScreenValue = readFromVector index $ gfx chip

getSprite :: ChipState -> W.Word16 -> Int -> (Int, Int) -> [(Int, Bool)]
getSprite chip index size (xPosition, yPosition) =
  let sprite = V.toList . readVectorFromVector index size $ memory chip
      boolSprite = concatMap convertToBits sprite
      auxIndexCalculatorFunc x = x `mod` spriteWidth + x `div` spriteWidth * screenWidth
      position = xPosition + yPosition * screenWidth
      indexList = map auxIndexCalculatorFunc [0..(length boolSprite - 1)]
      addedPositionIndexList = map (auxAddPosition position) indexList
    in zip addedPositionIndexList boolSprite

auxAddPosition :: Int -> Int -> Int
auxAddPosition position index = coordinatesAdjustedIndex
  where coordinatesAdjustedIndex = if xCoordinateAdjustedIndex `div` screenSize > 0
                              then xCoordinateAdjustedIndex `mod` screenSize
                              else xCoordinateAdjustedIndex
        xCoordinateAdjustedIndex = if index `div` screenWidth < newIndex `div` screenWidth
                              then newIndex - screenWidth
                              else newIndex
        newIndex = index + position

accessRegister :: Integral a => ChipState -> a -> W.Word8
accessRegister chip index = readFromVector index $ registers chip

accessMemory :: Integral a => ChipState -> a -> W.Word16
accessMemory chip index = fromIntegral . readFromVector index $ memory chip

accessStack :: Integral a => ChipState -> a -> W.Word16
accessStack chip index = readFromVector index $ stack chip

accessKeypad :: Integral a => ChipState -> a -> Bool
accessKeypad chip index = readFromVector index $ keypad chip

getScreen :: ChipState -> [W.Word8]
getScreen chip = concatMap (\x -> if x then replicate 4 255 else replicate 4 0) $ V.toList (gfx chip)

convertToBits :: W.Word8 -> [Bool]
convertToBits x = map (testBit x) [7,6..0]

boolTest :: V.Vector Bool
boolTest = V.fromList [True,False,True,False,False,True,False,True,True,False,True,False,False,True,False,True]
