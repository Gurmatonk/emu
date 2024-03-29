{-# LANGUAGE RankNTypes #-}

module Lenses where

import Control.Lens
import Data.Bits (bit)
import Data.Word (Word16, Word8)
import qualified MCU
import Types
import Utils (bitwiseValue', mkWord16, splitWord16, bitTo, testBitF)

setCpuFlagZ :: Bool -> CPU -> CPU
setCpuFlagZ b cpu = cpu & cpuRegisterF %~ bitTo 7 b

getCpuFlagZ :: CPU -> Bool
getCpuFlagZ = testBitF 7 . view cpuRegisterF

cpuFlagN :: Lens' CPU Bool
cpuFlagN = cpuRegisterF . bitwiseValue' 6

cpuFlagH :: Lens' CPU Bool
cpuFlagH = cpuRegisterF . bitwiseValue' 5

cpuFlagC :: Lens' CPU Bool
cpuFlagC = cpuRegisterF . bitwiseValue' 4

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
interruptFlagVBlank = mcuInterruptFlag . bitwiseValue' 0

interruptFlagLCDStat :: Lens' MCU Bool
interruptFlagLCDStat = mcuInterruptFlag . bitwiseValue' 1

interruptFlagTimer :: Lens' MCU Bool
interruptFlagTimer = mcuInterruptFlag . bitwiseValue' 2

interruptFlagSerial :: Lens' MCU Bool
interruptFlagSerial = mcuInterruptFlag . bitwiseValue' 3

interruptFlagJoypad :: Lens' MCU Bool
interruptFlagJoypad = mcuInterruptFlag . bitwiseValue' 4