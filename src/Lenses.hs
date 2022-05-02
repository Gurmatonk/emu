{-# LANGUAGE RankNTypes #-}

module Lenses where

import Control.Lens
import Data.Bits (bit)
import Data.Word (Word16, Word8)
import qualified MCU
import Types
import Utils (bitwiseValue, mkWord16, splitWord16)

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

-- s -> (s, a) is actually State... consider!
pcLookup :: CPU -> (CPU, Word8)
pcLookup cpu = (cpu & cpuPC +~ 1, res)
  where
    res = cpu ^. mcuLookup cpuPC

mcuLookup :: Getter CPU Word16 -> Getter CPU Word8
mcuLookup w = to (\c -> MCU.addressLookup (c ^. w) (c ^. cpuMCU))

mcuWrite :: Getter CPU Word16 -> Getter CPU Word8 -> CPU -> CPU
mcuWrite a v cpu = cpu & cpuMCU .~ MCU.addressWrite (cpu ^. a) (cpu ^. v) (cpu ^. cpuMCU)

interruptFlagVBlank :: Lens' MCU Bool
interruptFlagVBlank = mcuInterruptFlag . bitwiseValue (bit 0)

interruptFlagLCDStat :: Lens' MCU Bool
interruptFlagLCDStat = mcuInterruptFlag . bitwiseValue (bit 1)

interruptFlagTimer :: Lens' MCU Bool
interruptFlagTimer = mcuInterruptFlag . bitwiseValue (bit 2)

interruptFlagSerial :: Lens' MCU Bool
interruptFlagSerial = mcuInterruptFlag . bitwiseValue (bit 3)

interruptFlagJoypad :: Lens' MCU Bool
interruptFlagJoypad = mcuInterruptFlag . bitwiseValue (bit 4)