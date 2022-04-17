{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Gameboy where

import Control.Lens
import Data.Bits (bit, complement, shiftL, shiftR, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int (Int8)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Data.Word (Word8, Word16)
import Data.Tuple (swap)

data Gameboy = Gameboy
  { _gbCPU :: CPU,
    _gbRAM :: RAM,
    _gbIME :: Bool
  }
  deriving (Show)

type RAM = Map Word16 Word8

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
    _cpuPC :: Word16
  }
  deriving (Show)

makeLenses ''Gameboy
makeLenses ''CPU

-- flipping single bits like it's 1989 :)
bitwiseValue :: Word8 -> Lens' Word8 Bool
bitwiseValue mask = lens getter setter
  where
    getter w = (mask .&. w) > 0
    setter w = bool (w .&. complement mask) (w .|. mask)

cpuFlagZ :: Lens' CPU Bool
cpuFlagZ = cpuRegisterF . bitwiseValue (bit 7)

cpuFlagN :: Lens' CPU Bool
cpuFlagN = cpuRegisterF . bitwiseValue (bit 6)

cpuFlagH :: Lens' CPU Bool
cpuFlagH = cpuRegisterF . bitwiseValue (bit 5)

cpuFlagC :: Lens' CPU Bool
cpuFlagC = cpuRegisterF . bitwiseValue (bit 4)

combinedRegister :: Lens' CPU Word8 -> Lens' CPU Word8 -> Lens' CPU Word16
combinedRegister hi lo = lens getter setter
  where
    getter cpu = mkWord16 (cpu ^. hi) (cpu ^. lo)
    setter cpu w =
      cpu & hi .~ (fst . splitWord16 $ w)
        & lo .~ (snd . splitWord16 $ w)

cpuRegisterHL :: Lens' CPU Word16
cpuRegisterHL = combinedRegister cpuRegisterH cpuRegisterL

cpuRegisterBC :: Lens' CPU Word16
cpuRegisterBC = combinedRegister cpuRegisterB cpuRegisterC

cpuRegisterDE :: Lens' CPU Word16
cpuRegisterDE = combinedRegister cpuRegisterD cpuRegisterE

-- only used when popping/pushing the stack pointer
cpuRegisterAF :: Lens' CPU Word16
cpuRegisterAF = combinedRegister cpuRegisterA cpuRegisterF

initGameboy :: Gameboy
initGameboy =
  Gameboy
    { _gbCPU = initCpu,
      _gbRAM = mempty,
      _gbIME = False
    }

initCpu :: CPU
initCpu =
  CPU
    { _cpuRegisterA = 0x01,
      _cpuRegisterF = 0xB0,
      _cpuRegisterB = 0xFE,
      _cpuRegisterC = 0x13,
      _cpuRegisterD = 0x00,
      _cpuRegisterE = 0xD8,
      _cpuRegisterH = 0x01,
      _cpuRegisterL = 0x4D,
      _cpuSP = 0xFFFE,
      _cpuPC = 0x0100
    }

mkWord16 ::
  -- | HI
  Word8 ->
  -- | LO
  Word8 ->
  Word16
mkWord16 hi lo = ((fromIntegral hi :: Word16) `shiftL` 8) + fromIntegral lo

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral ((w .&. 0xFF00) `shiftR` 8), fromIntegral (w .&. 0x00FF))

-- s -> (s, a) is actually State... consider!
pcLookup :: Gameboy -> (Gameboy, Word8)
pcLookup gb = (gb & gbCPU . cpuPC +~ 1, res)
  where
    res =
      case M.lookup (gb ^. gbCPU . cpuPC) (gb ^. gbRAM) of
        (Just v) -> v
        Nothing -> 0xFF

ramLookup' :: Word16 -> RAM -> Word8
ramLookup' l ram =
  case M.lookup l ram of
    (Just v) -> v
    Nothing -> 0xFF -- TODO: Check if good default

writeToRam :: Word16 -> Word8 -> Gameboy -> Gameboy
writeToRam k v gb = gb & gbRAM . at k ?~ v