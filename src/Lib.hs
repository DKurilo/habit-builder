module Lib
  ( view,
    add,
    remove,
    fileGetter,
    fileSaver,
    utcClock,
    consolePrinter,
  )
where

import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import System.IO (IOMode (ReadMode), hClose, hGetContents', openFile)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

-- domains

data SavedHabbitBuilder = SHB {shbCreditPerHours :: Int, shbLastTime :: UTCTime, shbCredits :: Int}

instance Read SavedHabbitBuilder where
  readPrec = do
    lastTime <- readPrec
    lift P.skipSpaces
    creditsPer <- readPrec
    lift P.skipSpaces
    SHB creditsPer lastTime <$> readPrec

instance Show SavedHabbitBuilder where
  show shb = unlines [show . shbLastTime $ shb, show . shbCreditPerHours $ shb, show . shbCredits $ shb]

mkSavedHabbitBuilder :: HabbitBuilder -> SavedHabbitBuilder
mkSavedHabbitBuilder hb = SHB (hbCreditPerHours hb) (hbLastTime hb) (hbCredits hb)

data HabbitBuilder = HB {hbCreditPerHours :: Int, hbLastTime :: UTCTime, hbCredits :: Int, hbNow :: UTCTime}

mkHabbitBuilder :: SavedHabbitBuilder -> UTCTime -> HabbitBuilder
mkHabbitBuilder shb = HB (shbCreditPerHours shb) (shbLastTime shb) (shbCredits shb)

updateHabbitBuilder :: HabbitBuilder -> HabbitBuilder
updateHabbitBuilder hb = hb {hbLastTime = lastTime, hbCredits = hbCredits hb - credits}
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

addCredit :: HabbitBuilder -> HabbitBuilder
addCredit hb = hb {hbCredits = hbCredits hb + 1}

removeCredit :: HabbitBuilder -> HabbitBuilder
removeCredit hb = hb {hbCredits = hbCredits hb - 1}

-- adapters

fileGetter :: String -> IO SavedHabbitBuilder
fileGetter fn = do
  fh <- openFile fn ReadMode
  cs <- hGetContents' fh
  hClose fh
  return . read $ cs

fileSaver :: SavedHabbitBuilder -> String -> IO ()
fileSaver shb fn = writeFile fn . show $ shb

utcClock :: IO UTCTime
utcClock = getCurrentTime

consolePrinter :: String -> IO ()
consolePrinter = putStrLn

-- use cases

doer ::
  (HabbitBuilder -> HabbitBuilder) ->
  (String -> IO ()) ->
  (String -> IO SavedHabbitBuilder) ->
  IO UTCTime ->
  (SavedHabbitBuilder -> String -> IO ()) ->
  String ->
  IO ()
doer f printer getter clock saver name = do
  shb <- getter name
  t <- clock
  let hb = updateHabbitBuilder . f . mkHabbitBuilder shb $ t
  saver (mkSavedHabbitBuilder hb) name
  printer . show . hbCredits $ hb

view ::
  (String -> IO ()) ->
  (String -> IO SavedHabbitBuilder) ->
  IO UTCTime ->
  (SavedHabbitBuilder -> String -> IO ()) ->
  String ->
  IO ()
view = doer id

add ::
  (String -> IO ()) ->
  (String -> IO SavedHabbitBuilder) ->
  IO UTCTime ->
  (SavedHabbitBuilder -> String -> IO ()) ->
  String ->
  IO ()
add = doer addCredit

remove ::
  (String -> IO ()) ->
  (String -> IO SavedHabbitBuilder) ->
  IO UTCTime ->
  (SavedHabbitBuilder -> String -> IO ()) ->
  String ->
  IO ()
remove = doer removeCredit
