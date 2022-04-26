module Emulator where

import Types

import CPU (runInstruction)

runCycles :: Cycles -> CPU -> CPU
runCycles target cpu =
  if remainingCycles < 0
    then runCycles remainingCycles newCpu
    else newCpu
  where
    (newCpu, usedCycles) = runInstruction cpu
    remainingCycles = target - usedCycles