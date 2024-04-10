module HabitBuilder.Ports.Usecases
  ( AddUsecase (..),
    RemoveUsecase (..),
    ViewUsecase (..),
  )
where

import HabitBuilder.Domains.Habit (Habit)

newtype AddUsecase = Add {unAdd :: Habit -> IO ()}

newtype RemoveUsecase = Remove {unRemove :: Habit -> IO ()}

newtype ViewUsecase = View {unView :: Habit -> IO ()}
