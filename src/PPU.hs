{-# LANGUAGE RankNTypes #-}

module PPU (module PPU) where
--   ( PPU,
--     initPpu,
--     lcdLookup,
--     lcdWrite,
--     oamLookup,
--     oamWrite,
--     toByteString,
--     updatePPU,
--     vRamLookup,
--     vRamWrite,
--   )
-- where

import Control.Lens
import Data.Bits (bit, testBit)
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
import Utils (boolIso, dualBit, testBitF, setBitF, clearBitF)
import Data.Sequence ((><), Seq)

initPpu :: PPU
initPpu =
  PPU
    { _ppuLCDC = 0x91,
      _ppuSTAT = 0x85,
      _ppuScrollY = 0x00,
      _ppuScrollX = 0x00,
      _ppuLCDY = 0x00,
      _ppuLYCompare = 0x00,
      _ppuDMA = 0xFF,
      _ppuBGPalette = 0xFC,
      _ppuOBJPalette0 = 0xFF, -- actually uninitialized
      _ppuOBJPalette1 = 0xFF, -- actually uninitialized
      _ppuWindowY = 0x00,
      _ppuWindowX = 0x00,
      _ppuVRAM = mempty,
      _ppuOAM = mempty,
      _ppuScreen = initialScreen,
      _ppuElapsedCycles = 0
    }

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
    LCDTransfer -> error "VRAM read during LCDTransfer" -- 0xFF
    _ -> fromMaybe 0xFF . view (ppuVRAM . at a) $ ppu

vRamWrite :: Word16 -> Word8 -> PPU -> PPU
vRamWrite a w ppu =
  case ppu ^. ppuMode of
    LCDTransfer -> error "VRAM write during LCDTransfer - most likely sync issue?!" -- ppu
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
  | a == 0xFF44 = ppu & ppuLCDY .~ 0 -- Read only, according to this (http://www.codeslinger.co.uk/pages/projects/gameboy/lcd.html) it is reset upon trying to write it
  | a == 0xFF45 = ppu & ppuLYCompare .~ w
  | a == 0xFF46 = ppu & ppuDMA .~ w
  | a == 0xFF47 = ppu & ppuBGPalette .~ w
  | a == 0xFF48 = ppu & ppuOBJPalette0 .~ w
  | a == 0xFF49 = ppu & ppuOBJPalette1 .~ w
  | a == 0xFF4A = ppu & ppuWindowY .~ w
  | a == 0xFF4B = ppu & ppuWindowX .~ w
  | otherwise   = undefined

-- init: True
ppuDisplayEnabled :: PPU -> Bool
ppuDisplayEnabled = testBitF 7 . view ppuLCDC

-- init: 9800 to 9BFF
ppuWindowTileMapArea :: PPU -> WindowTileMapArea
ppuWindowTileMapArea = windowTileMapArea . testBitF 5 . view ppuLCDC
  where
    windowTileMapArea = bool WTMA9800To9BFF WTMA9C00To9FFF

ppuWindowEnabled :: PPU -> Bool
ppuWindowEnabled ppu = testBitF 5 (ppu ^. ppuLCDC) && testBitF 0 (ppu ^. ppuLCDC)

-- init: 8000 to 8FFF
ppuBGWindowTileDataArea :: PPU -> BGWindowTileDataArea
ppuBGWindowTileDataArea = bGWindowTileDataArea . testBitF 4 . view ppuLCDC
  where
    bGWindowTileDataArea = bool TDA8800To97FF TDA8000To8FFF

-- init: 9800 to 9BFF
ppuBGTileMapArea :: PPU -> BGTileMapArea
ppuBGTileMapArea = bGTileMapArea . testBitF 3 . view ppuLCDC
  where
    bGTileMapArea = bool BTMA9800To9BFF BTMA9C00To9FFF

-- init: 8x8
ppuSpriteSize :: PPU -> SpriteSize
ppuSpriteSize = spriteSize . testBitF 3 . view ppuLCDC
  where
    spriteSize = bool Size8x8 Size8x16

-- init: False
ppuSpritesEnabled :: PPU -> Bool
ppuSpritesEnabled = testBitF 1 . view ppuLCDC

-- init: True
ppuBGWindowEnabled :: PPU -> Bool
ppuBGWindowEnabled = testBitF 0 . view ppuLCDC

ppuLYCInterrupt :: PPU -> Bool
ppuLYCInterrupt = testBitF 6 . view ppuSTAT

ppuMode2OAMInterrupt :: PPU -> Bool
ppuMode2OAMInterrupt = testBitF 5  . view ppuSTAT

ppuMode1VBlankInterrupt :: PPU -> Bool
ppuMode1VBlankInterrupt = testBitF 4 . view ppuSTAT

ppuMode0HBlankInterrupt :: PPU -> Bool
ppuMode0HBlankInterrupt = testBitF 3 . view ppuSTAT

ppuLYCLYFlag :: PPU -> Bool
ppuLYCLYFlag = testBitF 2 . view ppuSTAT

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


white :: RGBAColor
white = [155, 188, 15, 255]

lightGray :: RGBAColor
lightGray = [139, 172, 15, 255]

darkGray :: RGBAColor
darkGray = [48, 98, 48, 255]

black :: RGBAColor
black = [15, 56, 15, 255]

rgbaColor :: (Bool, Bool) -> RGBAColor
rgbaColor (False, False) = white
rgbaColor (False, True) = lightGray
rgbaColor (True, False) = darkGray
rgbaColor (True, True) = black

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

ppuBGColorIndex0 :: PPU -> RGBAColor
ppuBGColorIndex0 = rgbaColor . view (ppuBGPalette . dualBit 1 0)

ppuBGColorIndex1 :: PPU -> RGBAColor
ppuBGColorIndex1 = rgbaColor . view (ppuBGPalette . dualBit 3 2)

ppuBGColorIndex2 :: PPU -> RGBAColor
ppuBGColorIndex2 = rgbaColor . view (ppuBGPalette . dualBit 5 4)

ppuBGColorIndex3 :: PPU -> RGBAColor
ppuBGColorIndex3 = rgbaColor . view (ppuBGPalette . dualBit 7 6)

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

updatePPU :: Cycles -> PPU -> (PPU, PPUInterrupts)
updatePPU c ppu =
  if ppuDisplayEnabled ppu
  then (cyclePPU, foldr (<>) cycleInterrupts [lycInterrupts, modeInterrupts])
  else
    -- reset things, display is off
    (ppu & ppuElapsedCycles .~ 0
      & ppuLCDY .~ 0
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
    (ppu
      & (if ppu ^. ppuLCDY < 144 then drawScanline else id)
      & ppuElapsedCycles .~ newCycles `mod` 456
      & ppuLCDY .~ newScanline `mod` 154,
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
    (ppu & ppuSTAT %~ setBitF 2, bool NoPPUInterrupt LCDStatInterrupt (ppuLYCInterrupt ppu))
  | otherwise = (ppu & ppuSTAT %~ clearBitF 2, NoPPUInterrupt)

updateMode :: PPU -> (PPU, PPUInterrupts)
updateMode ppu
  -- Scanlines 144-153 are always VBlank
  | ppu ^. ppuLCDY >= 144 = 
    (ppu & ppuMode .~ VBlank, bool NoPPUInterrupt VBlankInterrupt (ppuMode1VBlankInterrupt ppu && (oldMode /= VBlank)))
  | ppu ^. ppuElapsedCycles <= 80 = 
    (ppu & ppuMode .~ SearchingOAM, bool NoPPUInterrupt LCDStatInterrupt (ppuMode2OAMInterrupt ppu && (oldMode /= SearchingOAM)))
  -- Note: not entirely correct, assumes fixed duration of 172 dots/cycles for SearchingOAM
  -- see https://gbdev.io/pandocs/pixel_fifo.html
  | ppu ^. ppuElapsedCycles <= 252 = (ppu & ppuMode .~ LCDTransfer, NoPPUInterrupt)
  | otherwise = 
    (ppu & ppuMode .~ HBlank, bool NoPPUInterrupt LCDStatInterrupt (ppuMode0HBlankInterrupt ppu && (oldMode /= HBlank)))
  where
    oldMode = ppu ^. ppuMode

drawScanline :: PPU -> PPU
drawScanline ppu =
  ppu
    & drawBGScanline
    & (bool id drawSpriteScanline . ppuSpritesEnabled $ ppu)

drawBGScanline :: PPU -> PPU
drawBGScanline ppu =
  ppu & ppuScreen . unScreen %~ (\lines -> lines & ix lineToDraw .~ resultingPixels)
  where
    resultingPixels =
      if ppuBGWindowEnabled ppu
        then
          -- scanline within enabled window?
          if ppu ^. ppuWindowY <= ppu ^. ppuLCDY && ppuWindowEnabled ppu
            then mergeWindowBGPixels ppu windowPixels bgPixels
            else bgPixels
        else whiteLine
    lineToDraw = ppu ^. ppuLCDY . to fromIntegral

    windowPixels = _pixelColour <$> fold [uncurry (toWindowPixels ppu) (windowVramByte x) | x <- [0..19]]
    windowVramByte x =
      (fromMaybe 0x00 . M.lookup tileAddr $ (ppu ^. ppuVRAM), fromMaybe 0x00 . M.lookup (tileAddr + 1) $ (ppu ^. ppuVRAM))
      where
        tileAddr = tileDataAddress (ppuBGWindowTileDataArea ppu) . fromMaybe 0x00 $ M.lookup (bgStartAddress + x) (ppu ^. ppuVRAM)
    windowStartAddress =
      -- start + offset for scanline
      -- window always starts top left
      areaStart + fromIntegral ((lineToDraw `div` 8) * 32)
      where
        areaStart =
          case ppuWindowTileMapArea ppu of
            WTMA9800To9BFF -> 0x9800
            WTMA9C00To9FFF -> 0x9C00

    bgPixels = _pixelColour <$> fold [uncurry (toBGPixels ppu) (bgVramByte x) | x <- [0..19]]
    bgVramByte :: Address -> (Word8, Word8)
    bgVramByte x =
      (fromMaybe 0x00 . M.lookup tileAddr $ (ppu ^. ppuVRAM), fromMaybe 0x00 . M.lookup (tileAddr + 1) $ (ppu ^. ppuVRAM))
      where
        tileAddr = tileDataAddress (ppuBGWindowTileDataArea ppu) . fromMaybe 0x00 $ M.lookup (bgStartAddress + x) (ppu ^. ppuVRAM)
    bgStartAddress =
      -- start + offset for scanline + scroll x + scroll y
      areaStart
        + fromIntegral ((lineToDraw `div` 8) * 32)
        + fromIntegral (ppu ^. ppuScrollX)
        + fromIntegral ((fromIntegral (ppu ^. ppuScrollY) `div` 8) * 32)
      where
        areaStart =
          case ppuBGTileMapArea ppu of
            BTMA9800To9BFF -> 0x9800
            BTMA9C00To9FFF -> 0x9C00

    tileDataAddress :: BGWindowTileDataArea -> Word8 -> Address
    tileDataAddress TDA8000To8FFF tileNr = 0x8000 + fromIntegral (fromIntegral tileNr * (16 :: Integer) + (fromIntegral (ppu ^. ppuLCDY `mod` 8) * 2))
    tileDataAddress TDA8800To97FF tileNr = 0x8800 + fromIntegral (fromIntegral (tileNr + 128) * (16 :: Integer) + (fromIntegral (ppu ^. ppuLCDY `mod` 8) * 2))

mergeWindowBGPixels :: PPU -> [RGBAColor] -> [RGBAColor] -> [RGBAColor]
mergeWindowBGPixels ppu win bg =
    bgpart <> winpart
  where
    bgpart = take (fromIntegral windowX) bg
    winpart = drop (fromIntegral windowX) win
    windowX = ppu ^. ppuWindowX - 7

drawBGScanlineTest :: PPU -> PPU
drawBGScanlineTest ppu =
  ppu & ppuScreen . unScreen %~ (\lines -> lines & ix lineToDraw .~ if even lineToDraw then whiteLine else darkGrayLine)
  where
    lineToDraw = ppu ^. ppuLCDY . to fromIntegral

toBGPixels :: PPU -> Word8 -> Word8 -> [Pixel]
toBGPixels ppu lo hi =
  Pixel Background <$>
      [
        (testBit hi 7, testBit lo 7) ^. to lookupColourIndex,
        (testBit hi 6, testBit lo 6) ^. to lookupColourIndex,
        (testBit hi 5, testBit lo 5) ^. to lookupColourIndex,
        (testBit hi 4, testBit lo 4) ^. to lookupColourIndex,
        (testBit hi 3, testBit lo 3) ^. to lookupColourIndex,
        (testBit hi 2, testBit lo 2) ^. to lookupColourIndex,
        (testBit hi 1, testBit lo 1) ^. to lookupColourIndex,
        (testBit hi 0, testBit lo 0) ^. to lookupColourIndex
      ]
  where
    lookupColourIndex (False, False) = ppuBGColorIndex0 ppu
    lookupColourIndex (False, True) = ppuBGColorIndex1 ppu
    lookupColourIndex (True, False) = ppuBGColorIndex2 ppu
    lookupColourIndex (True, True) = ppuBGColorIndex3 ppu

toWindowPixels :: PPU -> Word8 -> Word8 -> [Pixel]
toWindowPixels ppu lo hi =
  Pixel Window <$>
      [
        (testBit hi 7, testBit lo 7) ^. to lookupColourIndex,
        (testBit hi 6, testBit lo 6) ^. to lookupColourIndex,
        (testBit hi 5, testBit lo 5) ^. to lookupColourIndex,
        (testBit hi 4, testBit lo 4) ^. to lookupColourIndex,
        (testBit hi 3, testBit lo 3) ^. to lookupColourIndex,
        (testBit hi 2, testBit lo 2) ^. to lookupColourIndex,
        (testBit hi 1, testBit lo 1) ^. to lookupColourIndex,
        (testBit hi 0, testBit lo 0) ^. to lookupColourIndex
      ]
  where
    lookupColourIndex (False, False) = ppuBGColorIndex0 ppu
    lookupColourIndex (False, True) = ppuBGColorIndex1 ppu
    lookupColourIndex (True, False) = ppuBGColorIndex2 ppu
    lookupColourIndex (True, True) = ppuBGColorIndex3 ppu

-- TODO: Implement
drawSpriteScanline :: PPU -> PPU
drawSpriteScanline = id

toRgba :: Colour -> [Word8]
toRgba White = [155, 188, 15, 255]
toRgba LightGray = [139, 172, 15, 255]
toRgba DarkGray = [48, 98, 48, 255]
toRgba Black = [15, 56, 15, 255]

toScreenByteString :: PPU -> ByteString
toScreenByteString = BS.pack . concat . concat . view (ppuScreen . unScreen)

toTestScreenPicture :: PPU -> ByteString
toTestScreenPicture _ppu = BS.pack . concat $ testScreenRaw

initialScreen :: Screen
initialScreen = Screen [whiteLine | x <- [0..143]]


-- TODO: Testing stuff below here
testScreenRaw :: [[Word8]]
testScreenRaw = [decideLine x | x <- [0..143]]
  where
    decideLine x
      | even x = wl
      | otherwise = dgl
    wl = concat [[155,188,15,255] | x <- [0..159]]
    dgl = concat [[48,98,48,255] | x <- [0..159]]

testScreen :: Screen
testScreen = Screen [decideline x | x <- [0..143]]
  where
    decideline x
      | even x = whiteLine
      | otherwise = darkGrayLine

whiteLine :: Line
whiteLine = [white | x <- [0..159]]

darkGrayLine :: Line
darkGrayLine = [darkGray | x <- [0..159]]

testData :: [Colour]
testData = [toColour i | i <- [1 ..]]
  where
    toColour i
      | i `mod` 3 == 0 && i `mod` 5 == 0 = Black
      | i `mod` 3 == 0 = LightGray
      | i `mod` 5 == 0 = DarkGray
      | otherwise = White