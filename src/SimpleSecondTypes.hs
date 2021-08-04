module SimpleSecondTypes where


newtype SimpleSecondPrecise = SimpleSecondPrecise {sspRational :: Rational} deriving (Eq,Ord)

type SSText = String

type SSYear = Integer

type SSMonth = Int

type SSWeek = Int

type SSDay = Int

type KiloSS = Integer


data SSPDate = SSPDate {
    ssYear :: SSYear
  , ssMonth :: SSMonth
  , ssDay :: SSDay
  , simpleSecond :: SimpleSecondPrecise
  } deriving (Eq)



