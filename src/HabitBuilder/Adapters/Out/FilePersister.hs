module HabitBuilder.Adapters.Out.FilePersister
  ( FilePersister (..),
  )
where

import HabitBuilder.Ports.Persister (Persister (..))

data FilePersister = FilePersister

instance Persister FilePersister where
  persist _ shb name = writeFile name . show $ shb
