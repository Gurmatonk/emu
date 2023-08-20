{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
-- TODO: Check sometime if we can omit this in favor of actually finding space leak source

module Types where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Word (Word16, Word8)

data CPU = CPU
  { _cpuRegisterA :: !Word8,
    _cpuRegisterF :: !Word8,
    _cpuRegisterB :: !Word8,
    _cpuRegisterC :: !Word8,
    _cpuRegisterD :: !Word8,
    _cpuRegisterE :: !Word8,
    _cpuRegisterH :: !Word8,
    _cpuRegisterL :: !Word8,
    _cpuSP :: !Word16,
    _cpuPC :: !Word16,
    _cpuMCU :: !MCU,
    _cpuIME :: !Bool,
    _cpuHalted :: !Bool
  }
  deriving (Show)

data MCU = MCU
  { _mcuRAM :: !RAM,
    _mcuHIRAM :: !RAM,
    _mcuCartridge :: !Cartridge,
    _mcuPPU :: !PPU,
    _mcuClock :: !Clock,
    _mcuSerial :: !Serial,
    _mcuAPU :: !APU,
    _mcuJoypad :: !Joypad,
    _mcuBootRom :: !Word8, -- FF50
    _mcuInterruptFlag :: !Word8, -- FF0F
    _mcuInterruptEnable :: !Word8 -- FFFF
  }
  deriving (Show)

type Address = Word16

type RAM = Map Address Word8

type Joypad = Word8

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
    _ppuLYCompare :: Word8, -- FF45 (R/W) LY Compare. If LYC==LY, set flag in STAT and request interrupt if enabled
    _ppuDMA :: Word8, -- TODO FF46
    _ppuBGPalette :: Word8, -- FF47 (R/W) BG Colour Palette
    _ppuOBJPalette0 :: Word8, -- FF48 (R/W) OBJ Colour Palette 0 - Bits 1 0 are ignored, index 0 is always transparent for OBJs
    _ppuOBJPalette1 :: Word8, -- FF49 (R/W) OBJ Colour Palette 1 - Bits 1 0 are ignored, index 0 is always transparent for OBJs
    _ppuWindowY :: Position, -- FF4A (R/W) Top coordinate of Window
    _ppuWindowX :: Position, -- FF4B (R/W) Left coordinate of Window - 7 (WX == 7 is left-aligned with Screen)
    _ppuVRAM :: VRAM, -- 8000-9FFF
    _ppuOAM :: OAM, -- FE00-FE9F
    _ppuScreen :: !Screen,
    _ppuElapsedCycles :: Cycles -- PPU takes 456 cycles to do a scanline, hence we need to keep track
  }
  deriving (Show)

-- 144 Lines
newtype Screen = Screen { _unScreen :: [Line] }

instance Show Screen where
  show = const "SomeScreen"

-- r,g,b,a per color
type RGBAColor = [Word8]

-- 160 Pixel colors
type Line = [RGBAColor]

data TimaInterrupt = TimaInterrupt | NoTimaInterrupt
  deriving (Eq, Show)

-- TODO: Maybe improve this type...
data PPUInterrupts = LCDStatInterrupt | VBlankInterrupt | LCDStatAndVBlankInterrupt | NoPPUInterrupt
  deriving (Show, Eq)

type Position = Word8

type VRAM = Map Address Word8

type OAM = Map Address Word8

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
  { _pixelSource :: PixelSource,
    _pixelColour :: RGBAColor
  }
  deriving (Show)

data PixelSource = Background | Window | OBJPalette0 | OBJPalette1
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
  deriving (Eq, Show)

data Serial = Serial
  { _serialTransfer :: Word8, -- FF01
    _serialTransferControl :: Word8 -- FF02
  }
  deriving (Show)

data APU = APU
  { _apuChannel1Sweep :: Word8, -- FF10 Channel 1 Sweep Register (R/W)
    _apuChannel1SoundWave :: Word8, -- FF11 Channel 1 Sound length/Wave pattern duty (R/W)
    _apuChannel1VolumeEnvelope :: Word8, -- FF12 Channel 1 Volume Envelope (R/W)
    _apuChannel1FrequencyLo :: Word8, -- FF13 Channel 1 Frequency lo (Write Only)
    _apuChannel1FrequencyHi :: Word8, -- FF14 Channel 1 Frequency hi (R/W)
    _apuChannel2SoundWave :: Word8, -- FF16 Channel 2 Sound Length/Wave Pattern Duty (R/W)
    _apuChannel2VolumeEnvelope :: Word8, -- FF17 Channel 2 Volume Envelope (R/W)
    _apuChannel2FrequencyLo :: Word8, -- FF18 Channel 2 Frequency lo data (W)
    _apuChannel2FrequencyHi :: Word8, -- FF19 Channel 2 Frequency hi data (R/W)
    _apuChannel3OnOff :: Word8, -- FF1A Channel 3 Sound on/off (R/W)
    _apuChannel3SoundLength :: Word8, -- FF1B Channel 3 Sound Length (W)
    _apuChannel3OutputLevel :: Word8, -- FF1C Channel 3 Select output level (R/W)
    _apuChannel3FrequencyLo :: Word8, -- FF1D Channel 3 Frequency’s lower data (W)
    _apuChannel3FrequencyHi :: Word8, -- FF1E Channel 3 Frequency’s higher data (R/W)
    _apuChannel4SoundLength :: Word8, -- FF20 Channel 4 Sound Length (W)
    _apuChannel4VolumeEnvelope :: Word8, -- FF21 Channel 4 Volume Envelope (R/W)
    _apuChannel4PolyCounter :: Word8, -- FF22 Channel 4 Polynomial Counter (R/W)
    _apuChannel4CounterConsecutive :: Word8, -- FF23 Channel 4 Counter/consecutive; Inital (R/W)
    _apuChannelControl :: Word8, -- FF24 Channel control / ON-OFF / Volume (R/W)
    _apuOutputSelection :: Word8, -- FF25 Selection of Sound output terminal (R/W)
    _apuSoundOnOff :: Word8, -- FF26 Sound on/off
    _apuWavePatternRam :: Map Address Word8 -- FF30-FF3F - Wave Pattern RAM
  }
  deriving (Show)

makeLenses ''CPU
makeLenses ''MCU
makeLenses ''Cartridge
makeLenses ''PPU
makeLenses ''Clock
makeLenses ''Pixel
makeLenses ''Serial
makeLenses ''APU
makeLenses ''Screen