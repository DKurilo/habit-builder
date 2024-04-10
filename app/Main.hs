module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (add, consolePrinter, fileGetter, fileSaver, remove, utcClock, view)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs
  let command = fromMaybe "help" . listToMaybe $ xs
      name = fromMaybe "habit" . listToMaybe . drop 1 $ xs

  case command of
    "view" -> view consolePrinter fileGetter utcClock fileSaver name
    "add" -> add consolePrinter fileGetter utcClock fileSaver name
    "remove" -> remove consolePrinter fileGetter utcClock fileSaver name
    _ -> error "wrong command"
