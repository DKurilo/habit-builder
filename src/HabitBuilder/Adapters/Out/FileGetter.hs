module HabitBuilder.Adapters.Out.FileGetter
  ( FileGetter (..),
  )
where

import HabitBuilder.Ports.Getter (Getter (..))
import System.IO (IOMode (ReadMode), hClose, hGetContents', openFile)

data FileGetter = FileGetter

instance Getter FileGetter where
  get _ name = do
    fh <- openFile name ReadMode
    cs <- hGetContents' fh
    hClose fh
    return . read $ cs
