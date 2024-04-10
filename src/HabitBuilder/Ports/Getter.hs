module HabitBuilder.Ports.Getter (Getter (..)) where

import HabitBuilder.Domains.Habit (Habit, SavedHabitBuilder (..))

class Getter a where
  get :: a -> Habit -> IO SavedHabitBuilder
