module Main where

import qualified Cartridge as C
import qualified Data.ByteString as BS
import Emulator (runGame)
import Types (Cartridge)

main :: IO ()
main = do
  c <- loadCartridge
  runGame c

loadCartridge :: IO Cartridge
loadCartridge = do
  -- bs <- BS.readFile "test.gb"
  -- bs <- BS.readFile "m2_win_en_toggle.gb"
  -- bs <- BS.readFile "D:\\Dev\\GBTest\\rtc3test.gb"
  bs <- BS.readFile "D:\\Dev\\GBTest\\dmg-acid2.gb"
  -- bs <- BS.readFile "D:\\Dev\\GBTest\\ei-halt-dmgC-cgbBCE.gb"
  pure $ C.fromByteString bs