module HabitBuilder.Adapters.Into.Console
  ( Console (..),
  )
where

import Data.Maybe (fromMaybe, listToMaybe)
import HabitBuilder.Ports.Controller (Controller (..))
import HabitBuilder.Ports.Usecases (AddUsecase (..), RemoveUsecase (..), ViewUsecase (..))
import System.Environment (getArgs)

data Console = Console {cView :: ViewUsecase, cAdd :: AddUsecase, cRemove :: RemoveUsecase}

instance Controller Console where
  run csl = do
    xs <- getArgs
    let command = fromMaybe "help" . listToMaybe $ xs
        name = fromMaybe "habit" . listToMaybe . drop 1 $ xs

    case command of
      "view" -> (unView . cView) csl name
      "add" -> (unAdd . cAdd) csl name
      "remove" -> (unRemove . cRemove) csl name
      _ -> error "wrong command"
