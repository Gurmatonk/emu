{-# LANGUAGE RankNTypes #-}

module PPU 
  (
    PPU,
    initPpu,
    vRamLookup,
    vRamWrite
  ) where

import Control.Lens
import Data.Bits (bit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word16)

import Utils (bitwiseValue, boolIso, dualBit)
import Types

initPpu :: PPU
initPpu = undefined

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