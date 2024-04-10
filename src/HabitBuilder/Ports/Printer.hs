module HabitBuilder.Ports.Printer (Printer (..)) where

class Printer a where
  out :: a -> String -> IO ()
