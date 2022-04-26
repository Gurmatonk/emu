module Screen where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Clock (updateClock)
import CPU (initCpu)
import PPU (toByteString)
import Emulator (runCycles)

import Types

-- TODO: Move somewhere less...bad
runGame :: Cartridge -> IO ()
runGame c =
  play window background 60 (initialWorld c) toPicture handleInputs gameStep

initialWorld :: Cartridge -> CPU
initialWorld c = initCpu & cpuMCU . mcuCartridge .~ c

window :: Display
window = InWindow "GAMEBOY" (windowHeight * 4, windowWidth * 4) (10, 10)

background :: Color
background = makeColorI 155 188 15 255

toPicture :: CPU -> Picture
toPicture cpu = 
  scale 4.0 4.0 $
  bitmapOfByteString 
    windowWidth 
    windowHeight 
    (BitmapFormat TopToBottom PxRGBA) 
    (cpu ^. cpuMCU . mcuPPU . to toByteString)
    True

handleInputs :: Event -> CPU -> CPU
handleInputs _e = id -- TODO: Implement

gameStep :: Float -> CPU -> CPU
gameStep secs cpu =
  cpu 
    & runCycles cycles
    & cpuMCU . mcuClock %~ updateClock cycles
    & undefined -- Graphics
    & undefined -- Interrupts
  where
    cycles = ceiling (secs * cyclesPerSecond)

windowHeight :: Int
windowHeight = 160

windowWidth :: Int
windowWidth = 144

cyclesPerSecond :: Float
cyclesPerSecond = 4194304