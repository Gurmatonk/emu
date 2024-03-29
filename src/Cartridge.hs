module Cartridge where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Ix (inRange)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Types

dumpROMNN :: Cartridge -> String
dumpROMNN c = show $ bimap (`showHex` "") (`showHex` "") <$> M.toList (c ^. cartridgeROMNN)

initCartridge :: Cartridge
initCartridge = Cartridge mempty mempty mempty mempty

-- TODO: Consider restricting cartridge ram access to cartridges that actually have ram
cartridgeRamWrite :: Word16 -> Word8 -> Cartridge -> Cartridge
cartridgeRamWrite k v c = c & cartridgeRAM . at k ?~ v

fromByteString :: ByteString -> Cartridge
fromByteString bs =
  Cartridge
    { _cartridgeRawData = rawData,
      _cartridgeROM00 = rom00,
      _cartridgeROMNN = romNN,
      _cartridgeRAM = mempty
    }
  where
    rawData = BS.unpack bs
    rom00 = M.fromList . itoListOf (reindexed fromIntegral ifolded . indices (inRange (0x0000, 0x3FFF))) $ rawData
    -- TODO: this will only load 32kb roms correctly! Add actual MBC identification here
    romNN = M.fromList . itoListOf (reindexed fromIntegral ifolded . indices (inRange (0x4000, 0x7FFF))) $ rawData
