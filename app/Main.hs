import SDL
import SDL.Mixer as Mixer
import Linear (V4(..))
import Data.StateVar 
import qualified Data.Text as T
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Word as W
import Data.Bits as B
import Text.Printf
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
--import qualified Data.Vector.Mutable as VMM







main :: IO ()
main = --load "/home/bass/chip8/app/TETRIS" >>= print . V.length
  do
  initializeAll
  renderer <- prepareRenderer
  prepareAudio
  sound <- Mixer.load "/home/bass/chip8/app/sound.wav"
  play sound
  mainLoop False renderer
  closeAudio
  SDL.quit

mainLoop :: Bool -> Renderer -> IO ()
mainLoop True _ = return ()
mainLoop False renderer = do
  events <- pollEvents
  keyState <- getKeyboardState
  screenData <- if keyState ScancodeA
    then test
    else test2
  surface <- createRGBSurfaceFrom screenData (V2 4 4) 16 RGBA8888
  texture <- createTextureFromSurface renderer surface
  clear renderer
  copy renderer texture Nothing Nothing
  present renderer
  delay 20
  keyState <- getKeyboardState
  mainLoop (keyState ScancodeEscape) renderer

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

load :: String -> IO (V.Vector W.Word8)
load file = do
  content <- BS.readFile file
  let listFile = BS.unpack content
      listMemory = replicate 512 0 ++ listFile ++ replicate (4096 - (512 + length listFile)) 0
   in return $ V.fromList listMemory

--dummy surfaces for testing
test :: IO (VM.IOVector W.Word8)
test = V.thaw $ V.fromList [255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255]

test2 :: IO (VM.IOVector W.Word8)
test2 = V.thaw $ V.fromList [0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,255,255,255,255,0,0,0,0]
