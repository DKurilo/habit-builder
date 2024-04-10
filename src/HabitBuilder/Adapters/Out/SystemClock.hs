module HabitBuilder.Adapters.Out.SystemClock
  ( SystemClock (..),
  )
where

import Data.Time.Clock (getCurrentTime)
import HabitBuilder.Ports.Clock (Clock (..))

data SystemClock = SystemClock

instance Clock SystemClock where
  getTime _ = getCurrentTime
