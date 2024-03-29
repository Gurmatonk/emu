module RAM
  ( 
    hiramLookup,
    hiramWrite,
    ramLookup,
    ramWrite,
  )
where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Types

-- TODO: Can we restrict this to 'actual' ram areas?
ramLookup :: Word16 -> RAM -> Word8
ramLookup w = fromMaybe 0xFF . M.lookup w -- TODO: Check if good default

ramWrite :: Word16 -> Word8 -> RAM -> RAM
ramWrite k v = at k ?~ v

hiramLookup :: Word16 -> RAM -> Word8
hiramLookup w = fromMaybe 0xFF . M.lookup w -- TODO: Check if good default

hiramWrite :: Word16 -> Word8 -> RAM -> RAM
hiramWrite k v = at k ?~ v