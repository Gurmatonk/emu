{-# LANGUAGE RankNTypes #-}

module PPU 
  (
    PPU,
    initPpu,
    toByteString,
    updatePPU,
    vRamLookup,
    vRamWrite
  ) where

import Control.Lens
import Data.Bits (bit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Word (Word8, Word16)

import Utils (bitwiseValue, boolIso, dualBit)
import Types

initPpu :: PPU
initPpu =
  PPU
    { _ppuLCDC = 0x91,
      _ppuSTAT = 0x85,
      _ppuScrollY = 0x00,
      _ppuScrollX = 0x00,
      _ppuLCDY = 0x00,
      _ppuLCDX = 0x00,
      _ppuLYCompare = 0x00,
      _ppuDMA = 0xFF,
      _ppuBGPalette = 0xFC,
      _ppuOBJPalette0 = undefined,
      _ppuOBJPalette1 = undefined,
      _ppuWindowY = 0x00,
      _ppuWindowX = 0x00,
      _ppuVRAM = mempty,
      _ppuBGQueue = mempty,
      _ppuOAMQueue = mempty,
      _ppuPixelBuffer = testData
    }

testData :: [Colour]
testData = [toColour i | i <- [1..]]
  where
    toColour i | i `mod` 3 == 0 && i `mod` 5 == 0 = Black
               | i `mod` 3 == 0 = LightGray
               | i `mod` 5 == 0 = DarkGray
               | otherwise = White

vRamLookup :: Word16 -> PPU -> Word8
vRamLookup a ppu =
  case ppu ^. ppuMode of
    LCDTransfer -> 0xFF
    _ -> fromMaybe 0xFF . view (ppuVRAM . at a) $ ppu

vRamWrite :: Word16 -> Word8 -> PPU -> PPU
vRamWrite a w ppu =
  case ppu ^. ppuMode of
    LCDTransfer -> ppu
    _ -> ppu & ppuVRAM . at a ?~ w

ppuDisplayEnableFlag :: Lens' PPU Bool
ppuDisplayEnableFlag = ppuLCDC . bitwiseValue (bit 7)

windowTileMapArea :: Iso' Bool WindowTileMapArea
windowTileMapArea = boolIso WTMA9C00To9FFF WTMA9800To9BFF

ppuWindowTileMapArea :: Lens' PPU WindowTileMapArea
ppuWindowTileMapArea = ppuLCDC . bitwiseValue (bit 6) . windowTileMapArea

ppuWindowEnableFlag :: Lens' PPU Bool
ppuWindowEnableFlag = ppuLCDC . bitwiseValue (bit 5)

bGWindowTileDataArea :: Iso' Bool BGWindowTileDataArea
bGWindowTileDataArea = boolIso TDA8000To8FFF TDA8800To97FF

ppuBGWindowTileDataArea :: Lens' PPU BGWindowTileDataArea
ppuBGWindowTileDataArea = ppuLCDC . bitwiseValue (bit 4) . bGWindowTileDataArea

bGTileMapArea :: Iso' Bool BGTileMapArea
bGTileMapArea = boolIso BTMA9C00To9FFF BTMA9800To9BFF

ppuBGTileMapArea :: Lens' PPU BGTileMapArea
ppuBGTileMapArea = ppuLCDC . bitwiseValue (bit 3) . bGTileMapArea

spriteSize :: Iso' Bool SpriteSize
spriteSize = boolIso Size8x16 Size8x8

ppuSpriteSize :: Lens' PPU SpriteSize
ppuSpriteSize = ppuLCDC . bitwiseValue (bit 2) . spriteSize

ppuSpritesEnableFlag :: Lens' PPU Bool
ppuSpritesEnableFlag = ppuLCDC . bitwiseValue (bit 1)

ppuBGWindowEnableFlag :: Lens' PPU Bool
ppuBGWindowEnableFlag = ppuLCDC . bitwiseValue (bit 0)

ppuLYCInterrupt :: Lens' PPU Bool
ppuLYCInterrupt = ppuSTAT . bitwiseValue (bit 6)

ppuMode2OAMInterrupt :: Lens' PPU Bool
ppuMode2OAMInterrupt = ppuSTAT . bitwiseValue (bit 5)

ppuMode1VBlankInterrupt :: Lens' PPU Bool
ppuMode1VBlankInterrupt = ppuSTAT . bitwiseValue (bit 4)

ppuMode0HBlankInterrupt :: Lens' PPU Bool
ppuMode0HBlankInterrupt = ppuSTAT . bitwiseValue (bit 3)

-- TODO: Equivalent to ppuLYC == ppuLY - "updated constantly" according to pandocs... also read only, so no memory writes on this one!
ppuLYCLYFlag :: Lens' PPU Bool
ppuLYCLYFlag = ppuSTAT . bitwiseValue (bit 2)

mode :: Iso' (Bool, Bool) Mode
mode = iso from to
  where
    from (True, True) = LCDTransfer
    from (True, False) = SearchingOAM
    from (False, True) = VBlank
    from (False, False) = HBlank
    to LCDTransfer = (True, True)
    to SearchingOAM = (True, False)
    to VBlank = (False, True)
    to HBlank = (False, False)

-- TODO: PPU Mode is also read only
ppuMode :: Lens' PPU Mode
ppuMode = lens getter setter
  where
    getter ppu = ppu ^. ppuSTAT . dualBit 1 0 . mode
    setter ppu m = ppu & ppuSTAT . dualBit 1 0 .~ m ^. from mode

colour :: Iso' (Bool, Bool) Colour
colour = iso from to
  where
    from (False, False) = White
    from (False, True) = LightGray
    from (True, False) = DarkGray
    from (True, True) = Black
    to White = (False, False)
    to LightGray = (False, True)
    to DarkGray = (True, False)
    to Black = (True, True)

ppuBGColourIndex3 :: Lens' PPU Colour
ppuBGColourIndex3 = lens getter setter
  where
    getter ppu = ppu ^. ppuBGPalette . dualBit 7 6 . colour
    setter ppu c = ppu & ppuBGPalette . dualBit 7 6 .~ c ^. from colour

ppuBGColourIndex2 :: Lens' PPU Colour
ppuBGColourIndex2 = lens getter setter
  where
    getter ppu = ppu ^. ppuBGPalette . dualBit 5 4 . colour
    setter ppu c = ppu & ppuBGPalette . dualBit 5 4 .~ c ^. from colour

ppuBGColourIndex1 :: Lens' PPU Colour
ppuBGColourIndex1 = lens getter setter
  where
    getter ppu = ppu ^. ppuBGPalette . dualBit 3 2 . colour
    setter ppu c = ppu & ppuBGPalette . dualBit 3 2 .~ c ^. from colour

ppuBGColourIndex0 :: Lens' PPU Colour
ppuBGColourIndex0 = lens getter setter
  where
    getter ppu = ppu ^. ppuBGPalette . dualBit 1 0 . colour
    setter ppu c = ppu & ppuBGPalette . dualBit 1 0 .~ c ^. from colour

ppuOBJ0ColourIndex3 :: Lens' PPU Colour
ppuOBJ0ColourIndex3 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette0 . dualBit 7 6 . colour
    setter ppu c = ppu & ppuOBJPalette0 . dualBit 7 6 .~ c ^. from colour

ppuOBJ0ColourIndex2 :: Lens' PPU Colour
ppuOBJ0ColourIndex2 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette0 . dualBit 5 4 . colour
    setter ppu c = ppu & ppuOBJPalette0 . dualBit 5 4 .~ c ^. from colour

ppuOBJ0ColourIndex1 :: Lens' PPU Colour
ppuOBJ0ColourIndex1 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette0 . dualBit 3 2 . colour
    setter ppu c = ppu & ppuOBJPalette0 . dualBit 3 2 .~ c ^. from colour

ppuOBJ1ColourIndex3 :: Lens' PPU Colour
ppuOBJ1ColourIndex3 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette1 . dualBit 7 6 . colour
    setter ppu c = ppu & ppuOBJPalette1 . dualBit 7 6 .~ c ^. from colour

ppuOBJ1ColourIndex2 :: Lens' PPU Colour
ppuOBJ1ColourIndex2 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette1 . dualBit 5 4 . colour
    setter ppu c = ppu & ppuOBJPalette1 . dualBit 5 4 .~ c ^. from colour

ppuOBJ1ColourIndex1 :: Lens' PPU Colour
ppuOBJ1ColourIndex1 = lens getter setter
  where
    getter ppu = ppu ^. ppuOBJPalette1 . dualBit 3 2 . colour
    setter ppu c = ppu & ppuOBJPalette1 . dualBit 3 2 .~ c ^. from colour

withinWindow :: PPU -> Bool
withinWindow ppu = 
  ppu ^. ppuLCDX > ppu ^. ppuWindowX - 7
    && ppu ^. ppuLCDY > ppu ^. ppuWindowY -- Unclear whether this is needed, pandocs only mention the X coordinate...

updatePPU :: Cycles -> PPU -> PPU
updatePPU _c _ppu = undefined

pixelFetcher :: PPU -> PPU
pixelFetcher ppu = undefined ppu
  where
    getTile ppu | ppu ^. ppuBGTileMapArea == BTMA9C00To9FFF && not (withinWindow ppu) = 0x9C00
                | ppu ^. ppuWindowTileMapArea == WTMA9C00To9FFF && withinWindow ppu = 0x9C00
                | otherwise = 0x9800

toByteString :: PPU -> ByteString
toByteString ppu = BS.pack . concatMap toRgba $ screen
  where
    screen = take (160 * 144) . drop (ppu ^. ppuLCDX . to fromIntegral) . view ppuPixelBuffer $ ppu -- TOOD: Implement

toRgba :: Colour -> [Word8]
toRgba White = [155, 188, 15, 255]
toRgba LightGray = [139, 172, 15, 255]
toRgba DarkGray = [48, 98, 48, 255]
toRgba Black = [15, 56, 15, 255]