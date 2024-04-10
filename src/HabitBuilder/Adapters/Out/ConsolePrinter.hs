module HabitBuilder.Adapters.Out.ConsolePrinter
  ( ConsolePrinter (..),
  )
where

import HabitBuilder.Ports.Printer (Printer (..))

data ConsolePrinter = ConsolePrinter

instance Printer ConsolePrinter where
  out _ = putStrLn
