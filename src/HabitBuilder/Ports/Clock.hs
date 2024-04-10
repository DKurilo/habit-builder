module HabitBuilder.Ports.Clock (Clock (..)) where

import Data.Time (UTCTime)

class Clock a where
  getTime :: a -> IO UTCTime
