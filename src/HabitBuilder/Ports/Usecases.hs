module HabitBuilder.Ports.Usecases
  ( AddUsecase (..),
    RemoveUsecase (..),
    ViewUsecase (..),
  )
where

import HabitBuilder.Domains.Habit (Habit)

newtype AddUsecase = Add {unAdd :: Habit -> Int -> IO ()}

newtype RemoveUsecase = Remove {unRemove :: Habit -> Int -> IO ()}

newtype ViewUsecase = View {unView :: Habit -> IO ()}
