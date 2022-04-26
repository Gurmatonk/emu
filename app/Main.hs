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
  bs <- BS.readFile "D:\\Dev\\haskell\\emu\\test.gb"
  pure $ C.fromByteString bs