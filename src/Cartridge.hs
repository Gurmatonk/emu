module Cartridge where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Ix (inRange)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word16)
import Numeric (showHex)

import Types

dumpROMNN :: Cartridge -> String
dumpROMNN c = show $ bimap (`showHex` "") (`showHex` "") <$> M.toList (c ^. cartridgeROMNN)

initCartridge :: Cartridge
initCartridge = Cartridge mempty mempty mempty

fromByteString :: ByteString -> Cartridge
fromByteString bs =
  Cartridge
    { _cartridgeRawData = rawData,
      _cartridgeROM00 = rom00,
      _cartridgeROMNN = romNN
    }
  where
    rawData = BS.unpack bs
    rom00 = M.fromList . itoListOf (reindexed fromIntegral ifolded . indices (inRange (0x0000, 0x3FFF))) $ rawData
    -- TODO: this will only load 32kb roms correctly! Add actual MBC identification here
    romNN = M.fromList . itoListOf (reindexed fromIntegral ifolded . indices (inRange (0x4000, 0x7FFF))) $ rawData
