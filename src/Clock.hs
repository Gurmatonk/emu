module Clock where

import Control.Lens
import Data.Bits (bit)
import Data.Word (Word8)
import Types
import Utils (bitwiseValue, dualBit)
import Data.Bool (bool)

initClock :: Clock
initClock =
  Clock
    { _clockDivider = 0xAB, -- increments at 16384Hz -> 41943404 cycles/s / 16384 Hz = every 256 cycles
      _clockTimer = 0x00, -- increments at clockFrequency specified in TimerControl, see below
      _clockTimerModulo = 0x00,
      _clockTimerControl = 0xF8,
      _clockElapsedCycles = 0,
      _clockElapsedCyclesMod = 0
    }

updateClock :: Cycles -> Clock -> (Clock, TimaInterrupt)
updateClock c clock =
  (clock & clockElapsedCycles .~ newCycles
    & clockElapsedCyclesMod .~ newCyclesMod
    & clockDivider +~ fromIntegral incDivider
    & updateTimer,
    bool NoTimaInterrupt TimaInterrupt timaOverflow
  )
  where
    updateTimer clock' =
      if clock' ^. clockTimerEnabled
        then
          clock' & clockElapsedCyclesMod .~ newCyclesMod
            & clockTimer .~ if timaOverflow then clock ^. clockTimerModulo else newTima
        else clock'
    newCycles = ((clock ^. clockElapsedCycles) + c) `mod` 256
    incDivider = ((clock ^. clockElapsedCycles) + c) `div` 256
    newCyclesMod = (clock ^. clockElapsedCyclesMod + c) `mod` (clock ^. clockClockFrequency . to clockFrequencyCycles)
    incTima = ((clock ^. clockElapsedCyclesMod) + c) `div` (clock ^. clockClockFrequency . to clockFrequencyCycles)
    newTima = (clock ^. clockTimer) + fromIntegral incTima
    timaOverflow = incTima > 255 || (newTima < (clock ^. clockTimer))

clockTimerEnabled :: Lens' Clock Bool
clockTimerEnabled = clockTimerControl . bitwiseValue (bit 2)

clockFrequency :: Iso' (Bool, Bool) ClockFrequency
clockFrequency = iso from to
  where
    from (True, True) = ClockBy256
    from (True, False) = ClockBy64
    from (False, True) = ClockBy16
    from (False, False) = ClockBy1024
    to ClockBy256 = (True, True)
    to ClockBy64 = (True, False)
    to ClockBy16 = (False, True)
    to ClockBy1024 = (False, False)

clockClockFrequency :: Lens' Clock ClockFrequency
clockClockFrequency = lens getter setter
  where
    getter clock = clock ^. clockTimerControl . dualBit 1 0 . clockFrequency
    setter clock f = clock & clockTimerControl . dualBit 1 0 .~ f ^. from clockFrequency

clockFrequencyCycles :: ClockFrequency -> Cycles
clockFrequencyCycles ClockBy16 = 160
clockFrequencyCycles ClockBy64 = 640
clockFrequencyCycles ClockBy256 = 2560
clockFrequencyCycles ClockBy1024 = 10240

clockLookup :: Address -> Clock -> Word8
clockLookup a
  | a == 0xFF04 = view clockDivider
  | a == 0xFF05 = view clockTimer
  | a == 0xFF06 = view clockTimerModulo
  | a == 0xFF07 = view clockTimerControl
  | otherwise = undefined

clockWrite :: Address -> Word8 -> Clock -> Clock
clockWrite a v
  | a == 0xFF04 = clockDivider .~ 0x00 -- writing any value just resets this
  | a == 0xFF05 = clockTimer .~ v
  | a == 0xFF06 = clockTimerModulo .~ v
  | a == 0xFF07 = clockTimerControl .~ v
  | otherwise = undefined
