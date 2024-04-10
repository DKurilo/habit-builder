module Main where

import HabitBuilder.Adapters.Into.Console (Console (Console))
import HabitBuilder.Adapters.Out.ConsolePrinter (ConsolePrinter (ConsolePrinter))
import HabitBuilder.Adapters.Out.FileGetter (FileGetter (FileGetter))
import HabitBuilder.Adapters.Out.FilePersister (FilePersister (FilePersister))
import HabitBuilder.Adapters.Out.SystemClock (SystemClock (SystemClock))
import HabitBuilder.Ports.Controller (Controller (run))
import HabitBuilder.Usecases (mkAdd, mkRemove, mkView)

main :: IO ()
main = do
  let addUsecase = mkAdd ConsolePrinter FileGetter SystemClock FilePersister
      removeUsecase = mkRemove ConsolePrinter FileGetter SystemClock FilePersister
      viewUsecase = mkView ConsolePrinter FileGetter SystemClock FilePersister

  run $ Console viewUsecase addUsecase removeUsecase
