module HabitBuilder.Ports.Controller
  ( Controller (..),
  )
where

class Controller a where
  run :: a -> IO ()
