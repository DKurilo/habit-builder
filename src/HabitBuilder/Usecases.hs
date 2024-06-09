module HabitBuilder.Usecases
  ( mkAdd,
    mkRemove,
    mkView,
  )
where

import HabitBuilder.Domains.Habit
  ( Habit,
    HabitBuilder (..),
    addCredit,
    mkHabitBuilder,
    mkSavedHabitBuilder,
    updateHabitBuilder, removeCredit,
  )
import HabitBuilder.Ports.Clock (Clock (getTime))
import HabitBuilder.Ports.Getter (Getter (get))
import HabitBuilder.Ports.Persister (Persister (persist))
import HabitBuilder.Ports.Printer (Printer (out))
import HabitBuilder.Ports.Usecases (AddUsecase (..), RemoveUsecase (..), ViewUsecase (..))

doer ::
  (Printer p, Getter g, Clock c, Persister r) =>
  (Int -> HabitBuilder -> HabitBuilder) ->
  p ->
  g ->
  c ->
  r ->
  Habit ->
  Int ->
  IO ()
doer f printer getter clock persister name x = do
  shb <- get getter name
  t <- getTime clock
  let hb = updateHabitBuilder . f x . mkHabitBuilder shb $ t
  persist persister (mkSavedHabitBuilder hb) name
  out printer . show . hbCredits $ hb

mkAdd :: (Printer p, Getter g, Clock c, Persister r) => p -> g -> c -> r -> AddUsecase
mkAdd printer getter clock persister = Add $ doer addCredit printer getter clock persister

mkRemove :: (Printer p, Getter g, Clock c, Persister r) => p -> g -> c -> r -> RemoveUsecase
mkRemove printer getter clock persister = Remove $ doer removeCredit printer getter clock persister

mkView :: (Printer p, Getter g, Clock c, Persister r) => p -> g -> c -> r -> ViewUsecase
mkView printer getter clock persister = View $ flip (doer (\_ x -> x) printer getter clock persister) 0
