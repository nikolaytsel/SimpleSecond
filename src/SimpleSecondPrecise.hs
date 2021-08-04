module SimpleSecondPrecise
  (
    SSPDate (..)
  , SimpleSecondPrecise(..)
  , fullPart
  , ratioPart
  , fromSimpleSecondsPrecise
  , toSimpleSecondsPrecise
  , isLeapYear
  , toGregorian
  , fromGregorian
  , checkValidDate
  , daysInMonth
  , daysInWeek
  , ssecondsInDay
  , ssecondsInMonth
  , showFloatSSPDate
  , nextDay
  , prevDay
  , sspDateDiff
  , sspDateAdd
  , daysInSpecificMonth
  , fixSSPDate
  )
  where



import qualified Data.Time.Calendar as GC
import Data.Char (isDigit)
import qualified Data.Ratio as R
import Numeric (showFFloat)


import SimpleSecondTypes



------------ Standart Instances -----------------
instance Show SimpleSecondPrecise where
  show ssp = (show $ fullPart ssp ) <> ".(" <> (show $ ratioPart ssp ) <> ")"


instance Num SimpleSecondPrecise where
  (SimpleSecondPrecise r1) + (SimpleSecondPrecise r0) = SimpleSecondPrecise $ r1 + r0
  (SimpleSecondPrecise r1) * (SimpleSecondPrecise r0) = SimpleSecondPrecise $ r1 * r0
  abs (SimpleSecondPrecise r) = SimpleSecondPrecise $ abs r
  signum (SimpleSecondPrecise r) = SimpleSecondPrecise (signum r)
  fromInteger i = SimpleSecondPrecise (fromInteger i)
  negate (SimpleSecondPrecise r) = SimpleSecondPrecise (negate r)


instance Show SSPDate where
  show (SSPDate y m d ssp) = show y <> "-" <> show m <> "-" <> show d <> " " <> show ssp


instance Ord SSPDate where
  ssp0 <= ssp1 = less (fixSSPDate ssp0) (fixSSPDate ssp1)
    where
      less (SSPDate y0 m0 d0 ssp0) (SSPDate y1 m1 d1 ssp1)
        | y0 < y1 = True
        | y0 == y1 && m0 < m1 = True
        | y0 == y1 && m0 == m1 && d0 < d1 = True
        | y0 == y1 && m0 == m1 && d0 == d1 && ssp0 <= ssp1 = True
        | otherwise = False



------------ Constants --------------

daysInMonth = 73 :: Int
daysInWeek = 5 :: Int
monthesInNormalYear = 5 :: Int
ssecondsInDay = 100000 :: Int
ssecondsInMonth = daysInMonth * ssecondsInDay :: Int


------------ From-To SSeconds --------------

fromSimpleSecondsPrecise :: SimpleSecondPrecise -> SSPDate
fromSimpleSecondsPrecise ssp =
  let preAns =  fromSimpleSecondsPrecise' ssp
      (n',r') = properFraction $ sspRational $ simpleSecond preAns
  in if n' == 0 && r' < 0
       then prevDay $ preAns {simpleSecond = SimpleSecondPrecise (r' + fromIntegral ssecondsInDay)}
       else preAns
  -- What for? Why don't use just fromSimpleSecondsPrecise'?
  -- Because the problem arrives when ssp almost equal to ssecondsInDay
  -- E.g. let date = SSPDate (-343) 0 63 (SimpleSecondPrecise $ 99999 + (92737 / 100000))
  -- fromSimpleSecondsPrecise . toSimpleSecondsPrecise $ date = -343-0-63 99999.(92737 % 100000) -- that's correct
  -- fromSimpleSecondsPrecise' . toSimpleSecondsPrecise $ date = -343-0-64 0.((-7263) % 100000)  -- that's wrong!

  where
    fromSimpleSecondsPrecise' :: SimpleSecondPrecise -> SSPDate
    fromSimpleSecondsPrecise' ssp = 
      let (n,r) = properFraction $ sspRational ssp
          nDays = n `div` ssecondsInDay
          (year,_,_) = GC.toGregorian $ GC.addDays (fromIntegral nDays) gcZeroDay
          daysInYears = GC.diffDays (GC.fromGregorian year 1 1) gcZeroDay
          (month, day) = (nDays - fromIntegral daysInYears) `divMod` daysInMonth
          ss = n - day * ssecondsInDay - month * ssecondsInMonth - (fromIntegral daysInYears * ssecondsInDay)
      in SSPDate (year - 1) month day (SimpleSecondPrecise (fromIntegral ss + r))


  

toSimpleSecondsPrecise :: SSPDate -> SimpleSecondPrecise
toSimpleSecondsPrecise (SSPDate y m d (SimpleSecondPrecise r)) =
  let daysInYears = fromIntegral $ GC.diffDays (GC.fromGregorian (fromIntegral $ y + 1) 1 1) gcZeroDay :: Int
  -- y+1 because SSPDate starts from 0 year, not from 1st
  in SimpleSecondPrecise $ fromIntegral (daysInYears * ssecondsInDay + m * ssecondsInMonth + d * ssecondsInDay) + r





------------ Utils --------------

fullPart :: SimpleSecondPrecise -> Integer
fullPart (SimpleSecondPrecise r) = fst $ properFraction r

ratioPart :: SimpleSecondPrecise -> Rational
ratioPart (SimpleSecondPrecise r) = snd $ properFraction r



fixSSPDate :: SSPDate -> SSPDate
fixSSPDate date@(SSPDate y m d (SimpleSecondPrecise r))
  | r >= 0 && r < (fromIntegral ssecondsInDay) = date
  | otherwise = fromSimpleSecondsPrecise $ toSimpleSecondsPrecise date



daysInSpecificMonth :: SSYear -> SSMonth -> Maybe Int
daysInSpecificMonth y m
  | m < 5 = Just daysInMonth
  | m == 5 = if isLeapYear y
                then Just 1
                else Nothing
  | otherwise = Nothing

  


isLeapYear :: SSYear -> Bool
isLeapYear y
  | (y + 1) `mod` 400 == 0 = True
  | (y + 1) `mod` 100 == 0 = False
  | (y + 1) `mod` 4   == 0 = True
  | otherwise              = False


checkValidDate :: SSPDate -> Bool
checkValidDate (SSPDate y m d ss_)
  | m <  0 || d <  0 = False
  | m <  5 && d < 73 = True
  | m == 5 && d == 0 = isLeapYear y
  | otherwise        = False




sspDateDiff :: SSPDate -> SSPDate -> SSPDate
sspDateDiff date1 date0 = fromSimpleSecondsPrecise $ (toSimpleSecondsPrecise date1) - (toSimpleSecondsPrecise date0)

sspDateAdd :: SSPDate -> SSPDate -> SSPDate
sspDateAdd date1 date0 = fromSimpleSecondsPrecise $ (toSimpleSecondsPrecise date1) + (toSimpleSecondsPrecise date0)


showFloatSSPDate :: SSPDate -> String
showFloatSSPDate  (SSPDate y m d (SimpleSecondPrecise ssp)) =
  let (n, r) = properFraction ssp
  in show y <> "-" <> show m <> "-" <> show d <> " " <> show n <> (tail $ showFFloat Nothing (fromRational r :: Float) "")


nextDay :: SSPDate -> SSPDate
nextDay = next. fixSSPDate
  where
    next :: SSPDate -> SSPDate
    next (SSPDate y m d ssp)
      | d == 0 && m == 5 = SSPDate (y + 1) 0 0 ssp
      | d < daysInMonth - 1 = SSPDate y m (d + 1) ssp
      | m < monthesInNormalYear - 1 || isLeapYear y = SSPDate y (m + 1) 0 ssp
      | otherwise = SSPDate (y + 1) 0 0 ssp


prevDay :: SSPDate -> SSPDate
prevDay = prev . fixSSPDate
  where
    prev :: SSPDate -> SSPDate
    prev (SSPDate y m d ssp)
      | d > 0 = SSPDate y m (d -1) ssp
      | m > 0 = SSPDate y (m-1) (daysInMonth - 1) ssp
      | isLeapYear (y - 1) = SSPDate (y - 1) (monthesInNormalYear) 0 ssp
      | otherwise = SSPDate (y -1) (monthesInNormalYear - 1) (daysInMonth - 1) ssp



    
------------ Georgian Calendar --------------

gcZeroDay :: GC.Day
gcZeroDay = GC.fromGregorian 1 1 1


toGregorian :: SSPDate -> (Integer, Int, Int)
toGregorian date = let ss = toSimpleSecondsPrecise date
                       nDays = fullPart ss `div` (fromIntegral ssecondsInDay)
                   in GC.toGregorian $ GC.addDays nDays gcZeroDay


fromGregorian :: SSYear -> SSMonth -> SSDay -> Maybe SSPDate
fromGregorian y m d = do
  gcDay <- GC.fromGregorianValid y m d
  let allDays = GC.diffDays gcDay gcZeroDay
  let daysInYears = GC.diffDays (GC.fromGregorian y 1 1) gcZeroDay
  let (month, day) = (allDays - daysInYears) `divMod` (fromIntegral daysInMonth)
  pure $ SSPDate y (fromIntegral month) (fromIntegral day) (SimpleSecondPrecise 0)
  
