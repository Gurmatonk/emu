module MCU
  ( MCU,
    addressLookup,
    addressWrite,
    initMcu,
  )
where

import APU (initAPU, apuLookup, apuWrite)
import Cartridge (initCartridge)
import Clock (clockLookup, clockWrite, initClock)
import Control.Lens
import Data.Bits (shiftL, bit)
import Data.Ix (inRange)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import PPU (initPpu, lcdLookup, lcdWrite, oamLookup, oamWrite, vRamLookup, vRamWrite)
import RAM (ramLookup, ramWrite)
import Serial (initSerial, serialLookup, serialWrite)
import Types
import Data.Bool (bool)
import Numeric (showHex)
import Utils (bitwiseValue)

initMcu :: MCU
initMcu = 
  MCU
    { _mcuRAM = mempty,
      _mcuCartridge = initCartridge,
      _mcuPPU = initPpu,
      _mcuClock = initClock,
      _mcuSerial = initSerial,
      _mcuAPU = initAPU,
      _mcuJoypad = 0xCF,
      _mcuBootRom = 0xFF, -- FF50
      _mcuInterruptFlag = 0xE1, -- FF0F
      _mcuInterruptEnable = 0x00 -- FFFF
    }

-- https://gbdev.io/pandocs/Memory_Map.html
-- TODO: Mapping of lookups/writes by Address:
-- 0000 - 3FFF: ROM Bank 00, from Cartridge, fixed
-- 4000 - 7FFF: ROM Banks 01 - NN, from Cartridge, switchable (Probably some cartridge handling needed here)
-- 8000 - 9FFF: VRAM
-- A000 - BFFF: External RAM, from Cartridge
-- C000 - DFFF: RAM
-- E000 - FDFF: Copy of RAMs C000-DDFF, officially prohibited but writable, writes here should also write to the corresponding Address in RAM
-- FE00 - FE9F: Sprite Attribute table (OAM)
-- FEA0 - FEFF: Not usable, prohibited
-- FF00 - FF7F: IO Registers
-- FF80 - FFFE: HRAM
-- FFFF: Interrupt Enable Register (TODO: Currently a flag in Gameboy, move)
inRomBank00 :: Word16 -> Bool
inRomBank00 = inRange (0x0000, 0x3FFF)

inRomBankNN :: Word16 -> Bool
inRomBankNN = inRange (0x4000, 0x7FFF)

inVRam :: Word16 -> Bool
inVRam = inRange (0x8000, 0x9FFF)

inCartridgeRam :: Word16 -> Bool
inCartridgeRam = inRange (0xA000, 0x9FFF)

inRam :: Word16 -> Bool
inRam = inRange (0xC000, 0xDFFF)

inMirrorRam :: Word16 -> Bool
inMirrorRam = inRange (0xE000, 0xFDFF)

dmaTransfer :: Word16 -> Bool
dmaTransfer = (==) 0xFF46

inOAM :: Word16 -> Bool
inOAM = inRange (0xFE00, 0xFE9F)

inProhibited :: Word16 -> Bool
inProhibited = inRange (0xFEA0, 0xFEFF)

inJoypad :: Word16 -> Bool
inJoypad = (==) 0xFF00

inSerialTransfer :: Word16 -> Bool
inSerialTransfer = inRange (0xFF01, 0xFF02)

inClock :: Word16 -> Bool
inClock = inRange (0xFF04, 0xFF07)

inAPU :: Word16 -> Bool
inAPU = inRange (0xFF10, 0xFF3F)

inLCD :: Word16 -> Bool
inLCD = inRange (0xFF40, 0xFF4B)

inBootRom :: Word16 -> Bool
inBootRom = (==) 0xFF50

interruptFlag :: Word16 -> Bool
interruptFlag = (==) 0xFF0F

interruptEnable :: Word16 -> Bool
interruptEnable = (==) 0xFFFF

transferDMA :: Word8 -> MCU -> MCU
transferDMA w mcu =
  foldr (\i m -> flip (addressWrite (targetAddress + i)) m . addressLookup (sourceAddress + i) $ m) mcu [0..0xA0]
  where
    sourceAddress = (fromIntegral w :: Word16) `shiftL` 8
    targetAddress = 0xFE00

addressLookup :: Word16 -> MCU -> Word8
addressLookup a
  | inRomBank00 a = fromMaybe 0xFF . view (mcuCartridge . cartridgeROM00 . to (M.lookup a))
  | inRomBankNN a = fromMaybe 0xFF . view (mcuCartridge . cartridgeROMNN . to (M.lookup a))
  | inVRam a = view (mcuPPU . to (vRamLookup a))
  | inCartridgeRam a = undefined
  | inRam a = view (mcuRAM . to (ramLookup a))
  | inMirrorRam a = view (mcuRAM . to (ramLookup (a - 2000)))
  | inOAM a = view (mcuPPU . to (oamLookup a))
  | inProhibited a = const 0xFF -- For now, actually shows super-weird behavior according to pandocs, see map
  | inJoypad a = view mcuJoypad
  | inSerialTransfer a = view (mcuSerial . to (serialLookup a))
  | inClock a = view (mcuClock . to (clockLookup a))
  | inAPU a = view (mcuAPU . to (apuLookup a))
  | inLCD a = view (mcuPPU . to (lcdLookup a))
  | inBootRom a = view mcuBootRom
  | interruptFlag a = view mcuInterruptFlag
  | interruptEnable a = view mcuInterruptEnable
  | otherwise = error ("Unhandled MCU read from address: " <> showHex a "")

addressWrite :: Word16 -> Word8 -> MCU -> MCU
addressWrite a w
  | inRomBank00 a = id
  | inRomBankNN a = id
  | inVRam a = mcuPPU %~ vRamWrite a w
  | inCartridgeRam a = undefined
  | inRam a = mcuRAM %~ ramWrite a w
  | inMirrorRam a = mcuRAM %~ ramWrite (a - 2000) w
  | inOAM a = bool id (transferDMA w) (dmaTransfer a) . (mcuPPU %~ oamWrite a w)
  | inProhibited a = id -- probably?
  | inJoypad a = mcuJoypad .~ w -- TODO: Bits 3-0 should be read-only
  | inSerialTransfer a = mcuSerial %~ serialWrite a w
  | inClock a = mcuClock %~ clockWrite a w
  | inAPU a = mcuAPU %~ apuWrite a w
  | inLCD a = mcuPPU %~ lcdWrite a w
  | inBootRom a = mcuBootRom .~ w
  | interruptFlag a = mcuInterruptFlag .~ w
  | interruptEnable a = mcuInterruptEnable .~ w
  | otherwise = error ("Unhandled MCU write to address: " <> showHex a "")