module HabitBuilder.Domains.Habit
  ( SavedHabitBuilder (..),
    HabitBuilder (..),
    mkSavedHabitBuilder,
    mkHabitBuilder,
    updateHabitBuilder,
    addCredit,
    removeCredit,
    Habit,
  )
where

import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

type Habit = String

data SavedHabitBuilder = SHB {shbCreditPerHours :: !Int, shbLastTime :: !UTCTime, shbCredits :: !Int}

instance Read SavedHabitBuilder where
  readPrec = do
    lastTime <- readPrec
    lift P.skipSpaces
    creditsPer <- readPrec
    lift P.skipSpaces
    SHB creditsPer lastTime <$> readPrec

instance Show SavedHabitBuilder where
  show shb = unlines [show . shbLastTime $ shb, show . shbCreditPerHours $ shb, show . shbCredits $ shb]

mkSavedHabitBuilder :: HabitBuilder -> SavedHabitBuilder
mkSavedHabitBuilder hb = SHB (hbCreditPerHours hb) (hbLastTime hb) (hbCredits hb)

data HabitBuilder = HB {hbCreditPerHours :: !Int, hbLastTime :: !UTCTime, hbCredits :: !Int, hbNow :: !UTCTime}

mkHabitBuilder :: SavedHabitBuilder -> UTCTime -> HabitBuilder
mkHabitBuilder shb = HB (shbCreditPerHours shb) (shbLastTime shb) (shbCredits shb)

updateHabitBuilder :: HabitBuilder -> HabitBuilder
updateHabitBuilder hb = hb {hbLastTime = lastTime, hbCredits = hbCredits hb - credits}
  where
    creditPerSeconds = 3600 * hbCreditPerHours hb
    credits =
      (`div` creditPerSeconds)
        . truncate
        . nominalDiffTimeToSeconds
        . diffUTCTime (hbNow hb)
        . hbLastTime
        $ hb
    lastTime = addUTCTime (fromIntegral (credits * creditPerSeconds)) . hbLastTime $ hb

addCredit :: Int -> HabitBuilder -> HabitBuilder
addCredit x hb = hb {hbCredits = hbCredits hb + x}

removeCredit :: Int -> HabitBuilder -> HabitBuilder
removeCredit x hb = hb {hbCredits = hbCredits hb - x}
