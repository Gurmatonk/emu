{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Word (Word16, Word8)

data CPU = CPU
  { _cpuRegisterA :: Word8,
    _cpuRegisterF :: Word8,
    _cpuRegisterB :: Word8,
    _cpuRegisterC :: Word8,
    _cpuRegisterD :: Word8,
    _cpuRegisterE :: Word8,
    _cpuRegisterH :: Word8,
    _cpuRegisterL :: Word8,
    _cpuSP :: Word16,
    _cpuPC :: Word16,
    _cpuMCU :: MCU,
    _cpuIME :: Bool
  }
  deriving (Show)

data MCU = MCU
  { _mcuRAM :: RAM,
    _mcuCartridge :: Cartridge,
    _mcuPPU :: PPU,
    _mcuClock :: Clock
  }
  deriving (Show)

type Address = Word16

type RAM = Map Address Word8

data Cartridge = Cartridge
  { _cartridgeRawData :: [Word8],
    _cartridgeROM00 :: Map Address Word8,
    _cartridgeROMNN :: Map Address Word8
  }
  deriving (Show)

data PPU = PPU
  { _ppuLCDC :: Word8, -- FF40 (R/W)
    _ppuSTAT :: Word8, -- FF41 (R/W) Note: bits 0-2 are READ ONLY according to pandocs, bit 7 appears to be unused
    _ppuScrollY :: Position, -- FF42 (R/W) Top position of visible 160x144 area within 256x256 BG map
    _ppuScrollX :: Position, -- FF43 (R/W) Left position of visible 160x144 area within 256x256 BG map
    _ppuLCDY :: Position, -- FF44 (R) Currently drawn scanline. 0-153 -> 144-153 = VBlank
    _ppuLCDX :: Position, -- No Equivalent register, tracks current pixel X position
    _ppuLYCompare :: Word8, -- FF45 (R/W) LY Compare. If LYC==LY, set flag in STAT and request interrupt if enabled
    _ppuDMA :: Word8, -- TODO
    _ppuBGPalette :: Word8, -- FF47 (R/W) BG Colour Palette
    _ppuOBJPalette0 :: Word8, -- FF48 (R/W) OBJ Colour Palette 0 - Bits 1 0 are ignored, index 0 is always transparent for OBJs
    _ppuOBJPalette1 :: Word8, -- FF49 (R/W) OBJ Colour Palette 1 - Bits 1 0 are ignored, index 0 is always transparent for OBJs
    _ppuWindowY :: Position, -- FF4A (R/W) Top coordinate of Window
    _ppuWindowX :: Position, -- FF4B (R/W) Left coordinate of Window + 7 (WX == 7 is left-aligned with Screen)
    _ppuVRAM :: VRAM,
    _ppuBGQueue :: Seq Pixel,
    _ppuOAMQueue :: Seq Pixel,
    _ppuPixelBuffer :: [Colour], -- final pixel buffer to be output
    _ppuElapsedCycles :: Cycles -- PPU takes 456 cycles to do a scanline, hence we need to keep track
  }
  deriving (Show)

type Position = Word8

type VRAM = Map Address Word8

data BGWindowTileDataArea = TDA8800To97FF | TDA8000To8FFF
  deriving (Show, Eq)

data WindowTileMapArea = WTMA9800To9BFF | WTMA9C00To9FFF
  deriving (Show, Eq)

data BGTileMapArea = BTMA9800To9BFF | BTMA9C00To9FFF
  deriving (Show, Eq)

data SpriteSize = Size8x8 | Size8x16
  deriving (Show, Eq)

data Mode = HBlank | VBlank | SearchingOAM | LCDTransfer
  deriving (Show, Eq)

data Colour = White | LightGray | DarkGray | Black
  deriving (Show, Eq)

data Pixel = Pixel
  { _pixelColour :: Colour,
    _pixelPalette :: PixelPalette,
    _pixelBGPriority :: BGOBJPriority
  }
  deriving (Show)

data PixelPalette = OBPJPalette1 | OBJPalette0
  deriving (Show, Eq)

data BGOBJPriority = OBJOverBG | BGOverOBJ
  deriving (Show, Eq)

data Clock = Clock
  { _clockDivider :: Word8, -- FF04
    _clockTimer :: Word8, -- FF05
    _clockTimerModulo :: Word8, -- FF06
    _clockTimerControl :: Word8, -- FF07
    _clockElapsedCycles :: Cycles,
    _clockElapsedCyclesMod :: Cycles
  }
  deriving (Show)

type Cycles = Int

data ClockFrequency = ClockBy16 | ClockBy64 | ClockBy256 | ClockBy1024

makeLenses ''CPU
makeLenses ''MCU
makeLenses ''Cartridge
makeLenses ''PPU
makeLenses ''Clock