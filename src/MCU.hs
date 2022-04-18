{-# LANGUAGE TemplateHaskell #-}

module MCU
  (
    MCU,
    addressLookup,
    addressWrite,
    initMcu
  )
where

import Control.Lens
import Data.Word (Word8, Word16)

import RAM (RAM, ramLookup, ramWrite)

newtype MCU = MCU {_mcuRAM :: RAM}
  deriving (Show)

makeLenses ''MCU

initMcu = MCU mempty

addressLookup :: Word16 -> MCU -> Word8
addressLookup w = view (mcuRAM . to (ramLookup w))

addressWrite :: Word16 -> Word8 -> MCU -> MCU
addressWrite a w = mcuRAM %~ ramWrite a w