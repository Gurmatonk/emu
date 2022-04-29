module APU where
import Types

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