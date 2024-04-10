module HabitBuilder.Ports.Persister (Persister (..)) where

import HabitBuilder.Domains.Habit (Habit, SavedHabitBuilder)

class Persister a where
  persist :: a -> SavedHabitBuilder -> Habit -> IO ()
