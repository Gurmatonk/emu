module Screen where

import Graphics.Gloss

window :: Display
window = InWindow "GAMEBOY" (160, 144) (10, 10)

background :: Color
background = makeColorI 155 188 15 255

data ScreenWriter = ScreenWriter
  { _currentX :: Int,
    _currentY :: Int
  }

data Pixel = Red | Green | Blue

windowHeight :: Int
windowHeight = 160

windowWidth :: Int
windowWidth = 144

test :: [Pixel]
test = [Red, Red, Green, Green, Blue, Blue]