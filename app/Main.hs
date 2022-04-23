module Main where

import qualified Data.ByteString as BS

import Types (Cartridge)
import qualified Cartridge as C

import Screen(runGame)

main :: IO ()
main = do
    c <- loadCartridge
    runGame c

loadCartridge :: IO Cartridge
loadCartridge = do
    bs <- BS.readFile "D:\\Dev\\haskell\\emu\\test.gb"
    pure $ C.fromByteString bs