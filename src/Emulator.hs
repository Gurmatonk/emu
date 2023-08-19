module Emulator where

import CPU (initCpu, runInstruction)
import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import PPU (toTestScreenPicture, toScreenByteString)
import Types

runGame :: Cartridge -> IO ()
runGame c =
  play window background fps (initialWorld c) toPicture handleInputs gameStep

fps :: Int
fps = 1

runCycles :: Cycles -> CPU -> CPU
runCycles target cpu =
  if remainingCycles > 0
    then runCycles remainingCycles newCpu
    else newCpu
  where
    (newCpu, usedCycles) = runInstruction cpu
    remainingCycles = target - usedCycles

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
      (cpu ^. cpuMCU . mcuPPU . to toScreenByteString)
      False

handleInputs :: Event -> CPU -> CPU
handleInputs _e = id -- TODO: Implement

gameStep :: Float -> CPU -> CPU
gameStep secs cpu =
  cpu & runCycles cycles
  where
    cycles = ceiling (secs * cyclesPerSecond)

windowHeight :: Int
windowHeight = 144

windowWidth :: Int
windowWidth = 160

cyclesPerSecond :: Float
cyclesPerSecond = 200000
-- cyclesPerSecond = 4194304 -- actual value, probably