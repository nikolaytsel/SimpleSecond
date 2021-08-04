import Test.QuickCheck
import Control.Monad

import SimpleSecondPrecise
import SimpleSecondTypes
import ParserSimpleSecond

import qualified Data.Time.Calendar as GC

------------------- Constants -----------------
firstYear = (-3000) :: SSYear
lastYear = 3000 :: SSYear
lastMonth = 4 :: SSMonth
lastLeapMonth = 5 :: SSMonth
lastDay = 72 :: SSDay
denominator = 100000 :: Int


------------------ Instances -------------------

instance Arbitrary SSPDate where
  arbitrary = do
    y <- choose (firstYear, lastYear) :: Gen SSYear
    m <- if isLeapYear y
      then choose (0,lastLeapMonth) :: Gen SSMonth
      else choose (0,lastMonth) :: Gen SSMonth
    d <- if m == lastLeapMonth
           then pure 0
           else choose (0, lastDay) :: Gen SSDay
    ssp <- arbitrary
    pure $ SSPDate y m d ssp


instance Arbitrary SimpleSecondPrecise where
  arbitrary = do
    n <- choose (0, ssecondsInDay - 1) :: Gen Int
    r <- choose (0, denominator - 1) :: Gen Int
    pure $ SimpleSecondPrecise (fromIntegral n + fromIntegral r / (fromIntegral denominator))



------------------- Properties -----------------


prop_fromto :: SSPDate -> Bool
prop_fromto date = fromSimpleSecondsPrecise (toSimpleSecondsPrecise date) == date
       

prop_parse :: SSPDate -> Bool
prop_parse date = case fromString (showFloatSSPDate date) of
                    Right date' -> date == date'
                    _           -> False



prop_nextPrevDay :: SSPDate -> Bool
prop_nextPrevDay date = nextDay (prevDay date) == date &&
                        prevDay (nextDay date) == date



prop_fixSSPDate :: (SSPDate, Int) -> Bool
prop_fixSSPDate (date0@(SSPDate y m d (SimpleSecondPrecise r)), delta) =
  let delta' = abs (delta)
      date = SSPDate y m d (SimpleSecondPrecise (r + (fromIntegral $ delta' * ssecondsInDay) ))
  in fixSSPDate date == (foldr (\i dt -> nextDay dt) date0 [1 .. delta'])


prop_daysInSpecificMonthAndCheckValidDate :: SSPDate -> Bool
prop_daysInSpecificMonthAndCheckValidDate (SSPDate y m d ssp) =
  let (Just maxDays) = daysInSpecificMonth y m
  in all id $ map (\day -> checkValidDate $ SSPDate y m day ssp) [0 .. (maxDays - 1)]


prop_isLeapYear :: SSYear -> Bool
prop_isLeapYear y = isLeapYear y == GC.isLeapYear (y + 1)

prop_ssNum :: (SimpleSecondPrecise, Int) -> Bool
prop_ssNum (ssp@(SimpleSecondPrecise r), delta) =
  SimpleSecondPrecise (fromIntegral delta) + ssp == SimpleSecondPrecise (r + fromIntegral delta) &&
  ssp + SimpleSecondPrecise (fromIntegral delta) == SimpleSecondPrecise (fromIntegral (delta) + r) &&
  SimpleSecondPrecise (r + fromIntegral delta) - ssp == SimpleSecondPrecise (fromIntegral delta) &&
  ssp - SimpleSecondPrecise (r + fromIntegral delta) == SimpleSecondPrecise (fromIntegral (-delta)) &&
  abs ssp == SimpleSecondPrecise (abs r) &&
  negate ssp == SimpleSecondPrecise (negate r) &&
  signum ssp == SimpleSecondPrecise (signum r) &&
  (fromIntegral delta :: SimpleSecondPrecise) == SimpleSecondPrecise (fromIntegral delta)


prop_sspDateAddDiff :: (SSPDate,SSPDate) -> Bool
prop_sspDateAddDiff (date0, date1) =
  sspDateDiff (sspDateAdd date0 date1) date1 == date0 &&
  sspDateDiff (sspDateAdd date1 date0) date0 == date1




-------------------- Params -----------------
maxS = 10000
maxVerbS = 5


runCheck :: (Arbitrary a, Show a) => (a -> Bool) -> String -> Bool -> IO ()
runCheck prop name verbose =
  putStrLn ("Starting " <> name <> " ...") >>
  quickCheck (withMaxSuccess maxS prop) >>
  when (verbose) (verboseCheck ( withMaxSuccess maxVerbS prop))

  
main :: IO ()
main = do
  runCheck prop_fromto "fromto" False
  runCheck prop_parse "parse" False
  runCheck prop_nextPrevDay "nextPrevDay" False
  runCheck prop_fixSSPDate "fixSSPDate" False
  runCheck prop_daysInSpecificMonthAndCheckValidDate "daysInSpecificMonthAndCheckValidDate" False
  runCheck prop_isLeapYear "isLeapYear" False
  runCheck prop_ssNum "ssNum" False
  runCheck prop_sspDateAddDiff "prop_sspDateAddDiff" False
