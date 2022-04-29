module Serial where

import Control.Lens
import Data.Word (Word8)

import Types

initSerial :: Serial
initSerial = 
  Serial
    { _serialTransfer = 0x00,
      _serialTransferControl = 0x7E
    }

serialLookup :: Address -> Serial -> Word8
serialLookup w
  | w == 0xFF01 = view serialTransfer 
  | w == 0xFF02 = view serialTransferControl 
  | otherwise   = undefined

serialWrite :: Address -> Word8 -> Serial -> Serial
serialWrite k v
  | k == 0xFF01 = serialTransfer .~ v
  | k == 0xFF02 = serialTransferControl .~ v
  | otherwise   = undefined