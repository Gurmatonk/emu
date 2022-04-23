module Screen where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import CPU (initCpu)

import Types

-- TODO: Move somewhere less...bad
runGame :: Cartridge -> IO ()
runGame c =
  play window background 60 (initialWorld c) toPicture handleInputs gameStep

initialWorld :: Cartridge -> CPU
initialWorld c = initCpu & cpuMCU . mcuCartridge .~ c

window :: Display
window = InWindow "GAMEBOY" (160, 144) (10, 10)

background :: Color
background = makeColorI 155 188 15 255

toPicture :: CPU -> Picture
toPicture _cpu = undefined

handleInputs :: Event -> CPU -> CPU
handleInputs _e = undefined

gameStep :: Float -> CPU -> CPU
gameStep _ = undefined

windowHeight :: Int
windowHeight = 160

windowWidth :: Int
windowWidth = 144