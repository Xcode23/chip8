import SDL
import SDL.Mixer as Mixer
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Word as W
import Data.Bits as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Chip8
import Control.Monad
import System.Environment


main :: IO ()
main = do
  initializeAll
  renderer <- prepareRenderer
  prepareAudio
  chip <- startChip
  args <- getArgs
  let romName = head args 
  rom <- loadGameFile $ "./" ++ romName
  let chipWithGame = loadGame rom chip
  mainLoop False chipWithGame renderer
  closeAudio
  SDL.quit

mainLoop :: Bool -> ChipState -> Renderer -> IO ()
mainLoop True _ _ = return ()
mainLoop False chip renderer = do
  SDL.delay 1 -- slight delay to help controlTimings keep up
  pumpEvents
  keyState <- getKeyboardState
  updatedChip <- run chip
  let updateFlag = newScreen updatedChip
  let (screen, newChip) = getScreen updatedChip
  when updateFlag (convertToScreenData screen >>= updateScreen renderer)
  mainLoop (keyState ScancodeEscape) newChip renderer

updateScreen :: Renderer -> VSM.IOVector W.Word8 -> IO ()
updateScreen renderer screenData = do
  surface <- createRGBSurfaceFrom screenData (V2 128 64) 512 RGBA8888 -- (V2 width height) width * 4
  texture <- createTextureFromSurface renderer surface
  clear renderer
  copy renderer texture Nothing Nothing
  present renderer

prepareAudio :: IO ()
prepareAudio = do
  let audio = Audio 44100 FormatU8 Mixer.Stereo
  myAudio <- openAudio audio 2048
  return ()

prepareRenderer :: IO Renderer
prepareRenderer = do
  window <- createWindow (T.pack "Chip-8") defaultWindow 
  createRenderer window (-1) defaultRenderer

loadGameFile :: String -> IO BS.ByteString
loadGameFile = BS.readFile

convertToScreenData :: [W.Word8] -> IO (VSM.IOVector W.Word8)
convertToScreenData screen = VS.thaw $ VS.fromList screen