{-# LANGUAGE RankNTypes #-}

module PPU
  ( PPU,
    initPpu,
    lcdLookup,
    lcdWrite,
    oamLookup,
    oamWrite,
    toByteString,
    updatePPU,
    vRamLookup,
    vRamWrite,
  )
where

import Control.Lens
import Data.Bits (bit)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Word (Word16, Word8)
import Types
import Utils (bitwiseValue, boolIso, dualBit)
import Data.Sequence ((><), Seq)

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
      _ppuOBJPalette0 = 0xFF, -- actually uninitialized
      _ppuOBJPalette1 = 0xFF, -- actually uninitialized
      _ppuWindowY = 0x00,
      _ppuWindowX = 0x00,
      _ppuVRAM = mempty,
      _ppuOAM = mempty,
      _ppuBGQueue = mempty,
      _ppuOAMQueue = mempty,
      _ppuPixelBuffer = testData,
      _ppuElapsedCycles = 0
    }

testData :: [Colour]
testData = [toColour i | i <- [1 ..]]
  where
    toColour i
      | i `mod` 3 == 0 && i `mod` 5 == 0 = Black
      | i `mod` 3 == 0 = LightGray
      | i `mod` 5 == 0 = DarkGray
      | otherwise = White

oamLookup :: Address -> PPU -> Word8
oamLookup a ppu =
  case ppu ^. ppuMode of
    HBlank -> fromMaybe 0xFF . view (ppuOAM . at a) $ ppu
    VBlank -> fromMaybe 0xFF . view (ppuOAM . at a) $ ppu
    SearchingOAM -> 0xFF
    LCDTransfer -> 0xFF

oamWrite :: Address -> Word8 -> PPU -> PPU
oamWrite a w ppu = 
  case ppu ^. ppuMode of
    HBlank -> ppu & ppuOAM . at a ?~ w
    VBlank -> ppu & ppuOAM . at a ?~ w
    SearchingOAM -> ppu
    LCDTransfer -> ppu

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

lcdLookup :: Word16 -> PPU -> Word8
lcdLookup a ppu
  | a == 0xFF40 = ppu ^. ppuLCDC
  | a == 0xFF41 = ppu ^. ppuSTAT
  | a == 0xFF42 = ppu ^. ppuScrollY
  | a == 0xFF43 = ppu ^. ppuScrollX
  | a == 0xFF44 = ppu ^. ppuLCDY
  | a == 0xFF45 = ppu ^. ppuLYCompare
  | a == 0xFF46 = ppu ^. ppuDMA
  | a == 0xFF47 = ppu ^. ppuBGPalette
  | a == 0xFF48 = ppu ^. ppuOBJPalette0
  | a == 0xFF49 = ppu ^. ppuOBJPalette1
  | a == 0xFF4A = ppu ^. ppuWindowY
  | a == 0xFF4B = ppu ^. ppuWindowX
  | otherwise   = undefined

lcdWrite :: Word16 -> Word8 -> PPU -> PPU
lcdWrite a w ppu
  | a == 0xFF40 = ppu & ppuLCDC .~ w
  | a == 0xFF41 = ppu & ppuSTAT .~ w -- TODO: Make bit 0, 1, 2 Read-only
  | a == 0xFF42 = ppu & ppuScrollY .~ w
  | a == 0xFF43 = ppu & ppuScrollX .~ w
  | a == 0xFF44 = ppu -- Read only
  | a == 0xFF45 = ppu & ppuLYCompare .~ w
  | a == 0xFF46 = ppu & ppuDMA .~ w
  | a == 0xFF47 = ppu & ppuBGPalette .~ w
  | a == 0xFF48 = ppu & ppuOBJPalette0 .~ w
  | a == 0xFF49 = ppu & ppuOBJPalette1 .~ w
  | a == 0xFF4A = ppu & ppuWindowY .~ w
  | a == 0xFF4B = ppu & ppuWindowX .~ w
  | otherwise   = undefined

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

updatePPU :: Cycles -> PPU -> (PPU, PPUInterrupts)
updatePPU c ppu =
  if ppu ^. ppuDisplayEnableFlag
  then (cyclePPU, foldr (<>) cycleInterrupts [lycInterrupts, modeInterrupts])
  else
    -- reset things, display is off
    (ppu & ppuElapsedCycles .~ 0
      & ppuLCDY .~ 0
      & ppuLCDX .~ 0
      & ppuMode .~ VBlank,
      NoPPUInterrupt)
  where
    (cyclePPU, cycleInterrupts) = execCycles c lycPPU
    (lycPPU, lycInterrupts) = updateLYC modePPU
    (modePPU, modeInterrupts) = updateMode ppu

instance Semigroup PPUInterrupts where
  NoPPUInterrupt <> i = i
  i <> NoPPUInterrupt = i
  LCDStatInterrupt <> LCDStatInterrupt = LCDStatInterrupt
  LCDStatInterrupt <> VBlankInterrupt = LCDStatAndVBlankInterrupt
  VBlankInterrupt <> VBlankInterrupt = VBlankInterrupt
  VBlankInterrupt <> LCDStatInterrupt = LCDStatAndVBlankInterrupt
  LCDStatAndVBlankInterrupt <> _ = LCDStatAndVBlankInterrupt
  _ <> LCDStatAndVBlankInterrupt = LCDStatAndVBlankInterrupt

execCycles :: Cycles -> PPU -> (PPU, PPUInterrupts)
execCycles c ppu =
  if newCycles >= 456
  then
    (ppu & ppuElapsedCycles .~ newCycles `mod` 456
      & ppuLCDY .~ newScanline `mod` 154
      & if newScanline < 144 then drawScanline else id,
      bool NoPPUInterrupt VBlankInterrupt (newScanline == 144)
    )
  else
    (ppu & ppuElapsedCycles .~ newCycles, NoPPUInterrupt)
  where
    newScanline = ppu ^. ppuLCDY + 1
    newCycles = ppu ^. ppuElapsedCycles + c

updateLYC :: PPU -> (PPU, PPUInterrupts)
updateLYC ppu
  | ppu ^. ppuLCDY == ppu ^. ppuLYCompare = 
    (ppu & ppuLYCLYFlag .~ True, bool NoPPUInterrupt LCDStatInterrupt (ppu ^. ppuLYCInterrupt))
  | otherwise = (ppu & ppuLYCLYFlag .~ False, NoPPUInterrupt)

updateMode :: PPU -> (PPU, PPUInterrupts)
updateMode ppu
  -- Scanlines 144-153 are always VBlank
  | ppu ^. ppuLCDY >= 144 = 
    (ppu & ppuMode .~ VBlank, bool NoPPUInterrupt VBlankInterrupt (ppu ^. ppuMode1VBlankInterrupt && (oldMode /= VBlank)))
  | ppu ^. ppuElapsedCycles <= 80 = 
    (ppu & ppuMode .~ SearchingOAM, bool NoPPUInterrupt LCDStatInterrupt (ppu ^. ppuMode2OAMInterrupt && (oldMode /= SearchingOAM)))
  -- Note: not entirely correct, assumes fixed duration of 172 dots/cycles for SearchingOAM
  -- see https://gbdev.io/pandocs/pixel_fifo.html
  | ppu ^. ppuElapsedCycles <= 252 = (ppu & ppuMode .~ LCDTransfer, NoPPUInterrupt)
  | otherwise = 
    (ppu & ppuMode .~ HBlank, bool NoPPUInterrupt LCDStatInterrupt (ppu ^. ppuMode0HBlankInterrupt && (oldMode /= HBlank)))
  where
    oldMode = ppu ^. ppuMode

drawScanline :: PPU -> PPU
drawScanline ppu =
  ppu & (bool id drawTiles . view ppuBGWindowEnableFlag $ ppu)
    & (bool id drawSprites . view ppuSpritesEnableFlag $ ppu)

-- TODO: Currently just floats everything with window pixels
drawTiles :: PPU -> PPU
drawTiles ppu =
    ppu & ppuBGQueue %~ flip (<>) lookupTiles
  where
    -- TODO: for each group of 8 in xPosition 0 - 159:
    -- a) load window pixels
    --    depending on ppuWindowTileMapArea and offset by current group of 8 get 2 bytes from VRAM
    --    toBGPixels
    -- b) load bg pixels
    --    depending on ppuBGTileMapArea and offset by current group of 8 get 2 bytes from VRAM
    --    toBGPixelColours
    -- c) check window y
    --    if not on scanline, use bg pixels
    --    if on scanline, check window x and if within tile, merge window and bg accordingly (window always above bg)
    lookupTiles = fold [toWindowPixels ppu (vramByte $ x * 2) (vramByte $ x * 2 + 1) | x <- [0..20]]
    vramByte x = fromMaybe 0x00 $ M.lookup (startAddress + (x * 2)) (ppu ^. ppuVRAM) -- TODO: Maybe we need an actual lookup function
    startAddress =
      case ppu ^. ppuWindowTileMapArea of
        WTMA9800To9BFF -> 0x9800
        WTMA9C00To9FFF -> 0x9C00
    -- Tile Numbers in region 8000 are unsigned, while in 8800 they are signed, hence they need to be offset by 128
    tileDataAddress TDA8000To8FFF tileNr = 0x8000 + (tileNr * 16)
    tileDataAddress TDA8800To97FF tileNr = 0x8800 + ((tileNr + 128) * 16)

toBGPixels :: PPU -> Word8 -> Word8 -> [Pixel]
toBGPixels ppu lo hi =
  Pixel Background <$>
      [
        (hi ^. bitwiseValue (bit 7), lo ^. bitwiseValue (bit 7)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 6), lo ^. bitwiseValue (bit 6)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 5), lo ^. bitwiseValue (bit 5)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 4), lo ^. bitwiseValue (bit 4)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 3), lo ^. bitwiseValue (bit 3)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 2), lo ^. bitwiseValue (bit 2)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 1), lo ^. bitwiseValue (bit 1)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 0), lo ^. bitwiseValue (bit 0)) ^. to lookupColourIndex
      ]
  where
    lookupColourIndex (False, False) = ppu ^. ppuBGColourIndex0
    lookupColourIndex (False, True) = ppu ^. ppuBGColourIndex1
    lookupColourIndex (True, False) = ppu ^. ppuBGColourIndex2
    lookupColourIndex (True, True) = ppu ^. ppuBGColourIndex3

toWindowPixels :: PPU -> Word8 -> Word8 -> [Pixel]
toWindowPixels ppu lo hi =
  Pixel Window <$>
      [
        (hi ^. bitwiseValue (bit 7), lo ^. bitwiseValue (bit 7)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 6), lo ^. bitwiseValue (bit 6)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 5), lo ^. bitwiseValue (bit 5)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 4), lo ^. bitwiseValue (bit 4)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 3), lo ^. bitwiseValue (bit 3)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 2), lo ^. bitwiseValue (bit 2)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 1), lo ^. bitwiseValue (bit 1)) ^. to lookupColourIndex,
        (hi ^. bitwiseValue (bit 0), lo ^. bitwiseValue (bit 0)) ^. to lookupColourIndex
      ]
  where
    lookupColourIndex (False, False) = ppu ^. ppuBGColourIndex0
    lookupColourIndex (False, True) = ppu ^. ppuBGColourIndex1
    lookupColourIndex (True, False) = ppu ^. ppuBGColourIndex2
    lookupColourIndex (True, True) = ppu ^. ppuBGColourIndex3


drawSprites :: PPU -> PPU
drawSprites = undefined

pixelFetcher :: PPU -> PPU
pixelFetcher ppu = undefined ppu
  where
    getTile ppu
      | ppu ^. ppuBGTileMapArea == BTMA9C00To9FFF && not (withinWindow ppu) = 0x9C00
      | ppu ^. ppuWindowTileMapArea == WTMA9C00To9FFF && withinWindow ppu = 0x9C00
      | otherwise = 0x9800

toByteString :: PPU -> ByteString
toByteString ppu = BS.pack . concatMap toRgba $ screen
  where
    screen = take (160 * 144) . toListOf (ppuBGQueue . folded . pixelColour) $ ppu -- TOOD: Implement. This also 'MUST' be cleared after taking a screen

toRgba :: Colour -> [Word8]
toRgba White = [155, 188, 15, 255]
toRgba LightGray = [139, 172, 15, 255]
toRgba DarkGray = [48, 98, 48, 255]
toRgba Black = [15, 56, 15, 255]