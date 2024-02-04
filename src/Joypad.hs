module Joypad (pressButton, releaseButton, joypadLookup, joypadWrite, initJoypad) where
import Types
import Graphics.Gloss.Interface.Pure.Game (Key (..), SpecialKey (..))
import Data.Word (Word8, Word16)
import Control.Lens
import Utils (testBitF, setBitF, clearBitF, bitTo, bitwiseValue')

joypad :: Lens' CPU Joypad
joypad = cpuMCU . mcuJoypad

initJoypad :: Joypad
initJoypad =
  Joypad
    { _joypadButtonSelect = True,
      _joypadDirectionSelect = True,
      _joypadUpPressed = False,
      _joypadDownPressed = False,
      _joypadRightPressed = False,
      _joypadLeftPressed = False,
      _joypadStartPressed = False,
      _joypadSelectPressed = False,
      _joypadAPressed = False,
      _joypadBPressed = False
    }

joypadLookup :: Joypad -> Word8
joypadLookup j =
  if j ^. joypadButtonSelect
    then
      0x00
        & setBitF 7
        & setBitF 6
        & setBitF 4
        & bitTo 3 (not $ j ^. joypadStartPressed)
        & bitTo 2 (not $ j ^. joypadSelectPressed)
        & bitTo 1 (not $ j ^. joypadBPressed)
        & bitTo 0 (not $ j ^. joypadAPressed)
    else
      0x00
        & setBitF 7
        & setBitF 6
        & setBitF 5
        & bitTo 3 (not $ j ^. joypadDownPressed)
        & bitTo 2 (not $ j ^. joypadUpPressed)
        & bitTo 1 (not $ j ^. joypadLeftPressed)
        & bitTo 0 (not $ j ^. joypadRightPressed)


joypadWrite :: Word8 -> Joypad -> Joypad
joypadWrite w j
  | not (testBitF 5 w) && not (testBitF 4 w) = error "tried to set joypad bit 4 and 5 at the same time"
  | not (testBitF 5 w) = j & joypadButtonSelect .~ True & joypadDirectionSelect .~ False
  | not (testBitF 4 w) = j & joypadButtonSelect .~ False & joypadDirectionSelect .~ True
  | otherwise = j & joypadButtonSelect .~ False & joypadDirectionSelect .~ False

requestJoypadInterrupt :: CPU -> CPU
requestJoypadInterrupt cpu = cpu & cpuMCU . mcuInterruptFlag . bitwiseValue' 4 .~ True

pressButton :: Key -> CPU -> CPU
pressButton (SpecialKey KeyUp) cpu =
  cpu
    & joypad . joypadUpPressed .~ True
    & (\c -> if not pressedBefore && directionSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadUpPressed
    directionSelect = cpu ^. joypad . joypadDirectionSelect
pressButton (SpecialKey KeyDown) cpu =
  cpu
    & joypad . joypadDownPressed .~ True
    & (\c -> if not pressedBefore && directionSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadDownPressed
    directionSelect = cpu ^. joypad . joypadDirectionSelect
pressButton (SpecialKey KeyRight) cpu =
  cpu
    & joypad . joypadRightPressed .~ True
    & (\c -> if not pressedBefore && directionSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadRightPressed
    directionSelect = cpu ^. joypad . joypadDirectionSelect
pressButton (SpecialKey KeyLeft) cpu =
  cpu
    & joypad . joypadLeftPressed .~ True
    & (\c -> if not pressedBefore && directionSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadLeftPressed
    directionSelect = cpu ^. joypad . joypadDirectionSelect
-- start
pressButton (SpecialKey KeyEnter) cpu =
  cpu
    & joypad . joypadStartPressed .~ True
    & (\c -> if not pressedBefore && buttonSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadStartPressed
    buttonSelect = cpu ^. joypad . joypadButtonSelect
-- select
pressButton (SpecialKey KeyBackspace) cpu =
  cpu
    & joypad . joypadSelectPressed .~ True
    & (\c -> if not pressedBefore && buttonSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadSelectPressed
    buttonSelect = cpu ^. joypad . joypadButtonSelect
-- A
pressButton (Char 'd') cpu =
  cpu
    & joypad . joypadAPressed .~ True
    & (\c -> if not pressedBefore && buttonSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadAPressed
    buttonSelect = cpu ^. joypad . joypadButtonSelect
-- B
pressButton (Char 's') cpu =
  cpu
    & joypad . joypadBPressed .~ True
    & (\c -> if not pressedBefore && buttonSelect then requestJoypadInterrupt c else c)
  where
    pressedBefore = cpu ^. joypad . joypadBPressed
    buttonSelect = cpu ^. joypad . joypadButtonSelect
pressButton _ cpu = cpu

releaseButton :: Key -> CPU -> CPU
releaseButton (SpecialKey KeyUp) cpu = cpu & joypad . joypadUpPressed .~ False
releaseButton (SpecialKey KeyDown) cpu = cpu & joypad . joypadDownPressed .~ False
releaseButton (SpecialKey KeyRight) cpu = cpu & joypad . joypadRightPressed .~ False
releaseButton (SpecialKey KeyLeft) cpu = cpu & joypad . joypadLeftPressed .~ False
-- start
releaseButton (SpecialKey KeyEnter) cpu = cpu & joypad . joypadStartPressed .~ False
-- select
releaseButton (SpecialKey KeyBackspace) cpu = cpu & joypad . joypadSelectPressed .~ False
-- A
releaseButton (Char 'd') cpu = cpu & joypad . joypadAPressed .~ False
-- B
releaseButton (Char 's') cpu = cpu & joypad . joypadBPressed .~ False
releaseButton _ cpu = cpu