module APU where

import Data.Word(Word8)
import Types
import Control.Lens
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)

initAPU :: APU
initAPU =
  APU
    { _apuChannel1Sweep = 0x80,
      _apuChannel1SoundWave = 0xBF,
      _apuChannel1VolumeEnvelope = 0xF3,
      _apuChannel1FrequencyLo = 0xFF,
      _apuChannel1FrequencyHi = 0xBF,
      _apuChannel2SoundWave = 0x3F,
      _apuChannel2VolumeEnvelope = 0x00,
      _apuChannel2FrequencyLo = 0xFF,
      _apuChannel2FrequencyHi = 0xBF,
      _apuChannel3OnOff = 0x7F,
      _apuChannel3SoundLength = 0xFF,
      _apuChannel3OutputLevel = 0x9F,
      _apuChannel3FrequencyLo = 0xFF,
      _apuChannel3FrequencyHi = 0xBF,
      _apuChannel4SoundLength = 0xFF,
      _apuChannel4VolumeEnvelope = 0x00,
      _apuChannel4PolyCounter = 0x00,
      _apuChannel4CounterConsecutive = 0xBF, 
      _apuChannelControl = 0x77,
      _apuOutputSelection = 0xF3,
      _apuSoundOnOff = 0xF1,
      _apuWavePatternRam = mempty
    }

apuLookup :: Address -> APU -> Word8
apuLookup w
  | w == 0xFF10 = view apuChannel1Sweep
  | w == 0xFF11 = view apuChannel1SoundWave
  | w == 0xFF12 = view apuChannel1VolumeEnvelope
  | w == 0xFF13 = const 0xFF -- Write only
  | w == 0xFF14 = view apuChannel1FrequencyHi
  | w == 0xFF16 = view apuChannel2SoundWave
  | w == 0xFF17 = view apuChannel2VolumeEnvelope
  | w == 0xFF18 = const 0xFF -- Write only
  | w == 0xFF19 = view apuChannel2FrequencyHi
  | w == 0xFF1A = view apuChannel3OnOff
  | w == 0xFF1B = const 0xFF -- Write only
  | w == 0xFF1C = view apuChannel3OutputLevel
  | w == 0xFF1D = const 0xFF -- Write only
  | w == 0xFF1E = view apuChannel3FrequencyHi
  | w == 0xFF20 = const 0xFF -- Write only
  | w == 0xFF21 = view apuChannel4VolumeEnvelope
  | w == 0xFF22 = view apuChannel4PolyCounter
  | w == 0xFF23 = view apuChannel4CounterConsecutive
  | w == 0xFF24 = view apuChannelControl
  | w == 0xFF25 = view apuOutputSelection
  | w == 0xFF26 = view apuSoundOnOff
  | inRange (0xFF30, 0xFF3F) w = fromMaybe 0xFF . view (apuWavePatternRam . at w)
  | otherwise   = undefined

apuWrite :: Address -> Word8 -> APU -> APU
apuWrite w v
  | w == 0xFF10 = apuChannel1Sweep .~ v
  | w == 0xFF11 = apuChannel1SoundWave .~ v
  | w == 0xFF12 = apuChannel1VolumeEnvelope .~ v
  | w == 0xFF13 = apuChannel1FrequencyLo .~ v
  | w == 0xFF14 = apuChannel1FrequencyHi .~ v
  | w == 0xFF16 = apuChannel2SoundWave .~ v
  | w == 0xFF17 = apuChannel2VolumeEnvelope .~ v
  | w == 0xFF18 = apuChannel2FrequencyLo .~ v
  | w == 0xFF19 = apuChannel2FrequencyHi .~ v
  | w == 0xFF1A = apuChannel3OnOff .~ v
  | w == 0xFF1B = apuChannel3SoundLength .~ v
  | w == 0xFF1C = apuChannel3OutputLevel .~ v
  | w == 0xFF1D = apuChannel3FrequencyLo .~ v
  | w == 0xFF1E = apuChannel3FrequencyHi .~ v
  | w == 0xFF20 = apuChannel4SoundLength .~ v
  | w == 0xFF21 = apuChannel4VolumeEnvelope .~ v
  | w == 0xFF22 = apuChannel4PolyCounter .~ v
  | w == 0xFF23 = apuChannel4CounterConsecutive .~ v
  | w == 0xFF24 = apuChannelControl .~ v
  | w == 0xFF25 = apuOutputSelection .~ v
  | w == 0xFF26 = apuSoundOnOff .~ v
  | inRange (0xFF30, 0xFF3F) w = apuWavePatternRam . at w ?~ v
  | otherwise   = undefined