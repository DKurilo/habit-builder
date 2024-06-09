module HabitBuilder.Adapters.Into.Console
  ( Console (..),
  )
where

import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad ((<=<))
import System.Environment (getArgs)
import Text.Read (readMaybe)
import HabitBuilder.Ports.Controller (Controller (..))
import HabitBuilder.Ports.Usecases (AddUsecase (..), RemoveUsecase (..), ViewUsecase (..))

data Console = Console {cView :: !ViewUsecase, cAdd :: !AddUsecase, cRemove :: !RemoveUsecase}

instance Controller Console where
  run csl = do
    xs <- getArgs
    let (command, xs') = case listToMaybe xs of
                             Just c -> (c, drop 1 xs)
                             Nothing -> ("help", xs)
        (x, xs'') =  case (readMaybe <=< listToMaybe) xs' of
                          Just x' -> (x', drop 1 xs')
                          Nothing -> (1, xs')
        name = fromMaybe "habit" . listToMaybe $ xs''

    case command of
      "view" -> (unView . cView) csl name
      "add" -> (unAdd . cAdd) csl name x
      "remove" -> (unRemove . cRemove) csl name x
      _ -> error "wrong command"
