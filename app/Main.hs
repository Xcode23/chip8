import SDL
import SDL.Mixer as Mixer
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Word as W
import Data.Bits as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector as V
import Chip8
import Control.Monad
import qualified Data.Vector.Unboxed as VB
--import Numeric (showHex, showIntAtBase)
--import Data.Char (intToDigit)







main :: IO ()
main = --load "/home/bass/chip8/app/TETRIS" >>= print . V.length
  do
  initializeAll
  renderer <- prepareRenderer
  prepareAudio
  sound <- Mixer.load "/home/bass/chip8/app/sound.wav"
  play sound
  chip <- startChip
  let chipWithGame = loadGame testScreenGame chip
  mainLoop False chipWithGame 0 renderer
  closeAudio
  SDL.quit

mainLoop :: Bool -> ChipState -> Int -> Renderer -> IO ()
mainLoop True _ _ _ = return ()
mainLoop False chip accumulator renderer = do
  SDL.delay 1 -- slight delay to help controlTimings keep up
  pumpEvents
  keyState <- getKeyboardState
  newTicks <- controlTimings (timer chip)
  updatedChip <- run chip
  screenData <- convertToScreenData $ getScreen updatedChip
  -- screenData <- if keyState ScancodeA
  --   then test
  --   else test2
  updateScreen screenData renderer
  --printFps accumulator
  mainLoop (keyState ScancodeEscape) updatedChip (accumulator+1) renderer

controlTimings :: W.Word32 -> IO W.Word32
controlTimings previousTicks = do
  newTicks <- SDL.ticks
  let maxIntendedDelay = ceiling ((1.0 / 100.0) * 1000.0)
      tickDifference = newTicks - previousTicks
      condition = maxIntendedDelay - tickDifference < maxIntendedDelay && -- checks that tickDifference is positive
                    maxIntendedDelay - tickDifference > 0                 -- checks that ticks difference is smaller than maxIntendedDelay
  when condition $ SDL.delay (maxIntendedDelay - tickDifference)
  SDL.ticks

updateScreen :: VSM.IOVector W.Word8 -> Renderer -> IO ()
updateScreen screenData renderer = do
  surface <- createRGBSurfaceFrom screenData (V2 128 64) 512 RGBA8888 -- (V2 width height) width * 4
  texture <- createTextureFromSurface renderer surface
  clear renderer
  copy renderer texture Nothing Nothing
  present renderer

printFps :: Int -> IO ()
printFps frames = do
  testTicks <- SDL.ticks
  let secs = fromIntegral testTicks / 1000.0 :: Float
      fps = fromIntegral frames / secs :: Float
  --print secs
  --print frames
  print ("FPS:" ++ show fps)

prepareAudio :: IO ()
prepareAudio = do
  let audio = Audio 44100 FormatU8 Mixer.Stereo
  myAudio <- openAudio audio 2048
  return ()

prepareRenderer :: IO Renderer
prepareRenderer = do
  window <- createWindow (T.pack "Chip-8") defaultWindow 
  createRenderer window (-1) defaultRenderer

toWord16 :: W.Word8 -> W.Word8 -> W.Word16
toWord16 x y = fromInteger $ shiftL (fromIntegral x) 8 .|. fromIntegral y
--print $ showHex (0x8154 `mod` 0x0100) ""

loadGameFile :: String -> IO BS.ByteString
loadGameFile = BS.readFile

--dummy surfaces for testing
test :: IO (VSM.IOVector W.Word8)
test = VS.thaw $ VS.fromList [255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255]

test2 :: IO (VSM.IOVector W.Word8)
test2 = VS.thaw $ VS.fromList [0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0]

convertToScreenData :: [W.Word8] -> IO (VSM.IOVector W.Word8)
convertToScreenData screen = VS.thaw $ VS.fromList screen

testScreenGame :: BS.ByteString
testScreenGame = BS.pack [0xA0, 55, 0x60, 0x40, 0x61, 0x20, 0xD0, 0x15, 0xA0, 0, 0xD0, 0x15, 0x12, 0x0C]