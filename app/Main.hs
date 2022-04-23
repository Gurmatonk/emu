module Main where

import qualified Data.ByteString as BS

import Cartridge (Cartridge)
import qualified Cartridge as C

main :: IO ()
main = do
    c <- loadCartridge
    print c

loadCartridge :: IO Cartridge
loadCartridge = do
    bs <- BS.readFile "D:\\Dev\\haskell\\emu\\test.gb"
    pure $ C.fromByteString bs