{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Word (Word8, Word16)
import MCU (MCU)

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

makeLenses ''CPU