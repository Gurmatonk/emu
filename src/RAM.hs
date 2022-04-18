module RAM
  (
    RAM,
    ramLookup,
    ramWrite
  )
  where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word16)

type RAM = Map Word16 Word8

-- TODO: Can we restrict this to 'actual' ram areas?
ramLookup :: Word16 -> RAM -> Word8
ramLookup w = fromMaybe 0xFF . M.lookup w -- TODO: Check if good default

ramWrite :: Word16 -> Word8 -> RAM -> RAM
ramWrite k v = at k ?~ v