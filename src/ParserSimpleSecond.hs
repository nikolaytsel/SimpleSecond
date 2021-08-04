{-# LANGUAGE RecordWildCards #-}


module ParserSimpleSecond (
    ParseError
  , fromString) where


import Control.Monad.State
import Control.Monad.Except
import qualified Data.Time.Calendar as GC
import Data.Char (isDigit)
import qualified Data.Ratio as R


import SimpleSecondTypes
import SimpleSecondPrecise



data ParseError = WrongFormat
                | NotDigitCharacter
                | WrongMonth
                | WrongLeapMonth
                | WrongDay
                | WrongLeapDay
                | SSecondsMoreThenInOneDay
                | InvalidDate
                deriving Show



------------ Parsing --------------


data ParserState = ParserState {
    parserStateRest :: SSText
  , parserStateYear :: SSYear
  , parserStateMonth :: SSMonth
  , parserStateDay :: SSDay
  , parserStateSimpleSecond :: SimpleSecondPrecise
  } 


type SSParser = ExceptT ParseError (State ParserState)


initParserState :: SSText -> ParserState
initParserState str = ParserState str 0 0 0 (SimpleSecondPrecise 0)



fromString :: SSText -> Either ParseError SSPDate
fromString str = makeSSPDate <$> (evalState (runExceptT $ parseDateYear >> get)  (initParserState str)) >>= \date ->
                 case checkValidDate date of
                   True  -> pure date
                   False -> Left InvalidDate
  where makeSSPDate :: ParserState -> SSPDate
        makeSSPDate ParserState {..} = fixSSPDate $
                                       SSPDate {
                                               ssYear = parserStateYear
                                             , ssMonth = parserStateMonth
                                             , ssDay = parserStateDay
                                             , simpleSecond = parserStateSimpleSecond
                                             }


parseDateYear :: SSParser ()
parseDateYear = get >>= \state -> parse (parserStateRest state) [] 1 >> parseDateMonth
  where
    parse :: SSText -> SSText -> Integer -> SSParser ()
    parse ('-':rest) already factor
      | null already = parse rest already (-1)
      | otherwise = modify (\st -> st { parserStateRest = rest, parserStateYear = factor * (read already)})

    parse (c:rest) already factor
      | isDigit c = parse rest (already ++ [c]) factor
      | otherwise = throwError NotDigitCharacter

    parse _ _ _ = throwError WrongFormat
             


parseDateMonth :: SSParser ()
parseDateMonth = get >>= parse . parserStateRest >> parseDateDay
  where
    parse :: SSText -> SSParser ()
    parse (m:'-':rest)
      | isDigit m = checkMonth (read [m]) rest
      | otherwise = throwError NotDigitCharacter
    parse _ = throwError WrongFormat

    checkMonth :: SSMonth -> SSText -> SSParser ()
    checkMonth mon rest
      | mon < 5 = modify (\state -> state { parserStateRest = rest, parserStateMonth = mon})
      | mon == 5 = do
                   year <- gets parserStateYear
                   if isLeapYear year
                     then modify (\state -> state { parserStateRest = rest, parserStateMonth = mon})
                     else throwError WrongLeapMonth
      | otherwise = throwError WrongMonth



parseDateDay :: SSParser ()
parseDateDay = get >>= parse . parserStateRest >> parseDateSimpleSecond
 where
    parse :: SSText -> SSParser ()
    parse (d1:d2:' ':rest) = parse2digits d1 d2 rest
    parse (d:' ':rest) = parse1digit d rest
    parse (d:[]) = parse1digit d []
    parse (d1:d2:[]) = parse2digits d1 d2 []
    parse _ = throwError WrongFormat

    parse1digit :: Char -> SSText -> SSParser ()
    parse1digit d rest
      | isDigit d = checkDay (read [d]) rest
      | otherwise = throwError NotDigitCharacter

    parse2digits :: Char -> Char -> SSText -> SSParser ()
    parse2digits d1 d2 rest
      | isDigit d1 && isDigit d2 = checkDay (read $ d1:d2:[]) rest
      | otherwise = throwError NotDigitCharacter

    checkDay :: SSDay -> SSText -> SSParser ()
    checkDay day rest
      | day > 72 = throwError WrongDay
      | day == 0 = modify $ \st -> st { parserStateRest = rest, parserStateDay = day}
      | otherwise = do
          mon <- gets parserStateMonth
          case mon of
            5 -> throwError WrongLeapDay
            _ -> modify $ \st -> st { parserStateRest = rest, parserStateDay = day}



parseDateSimpleSecond :: SSParser ()
parseDateSimpleSecond = get >>= \state -> parse (parserStateRest state) [] >> parseDateSimpleSecondRational
  where
    parse :: SSText -> SSText -> SSParser ()
    parse [] already
      | null already = modifyParserState [] 0
      | otherwise = modifyParserState [] $ read already
    parse ('.':rest) already
      | null already = throwError WrongFormat
      | otherwise =  modifyParserState rest $ read already

    parse (c:rest) already
      | isDigit c = parse rest (already ++ [c])
      | otherwise = throwError NotDigitCharacter

    modifyParserState :: SSText -> Int -> SSParser ()  
    modifyParserState rest sec
      | sec >= ssecondsInDay = throwError SSecondsMoreThenInOneDay
      | otherwise = modify $ \st -> st { parserStateRest = rest, parserStateSimpleSecond = SimpleSecondPrecise (fromIntegral sec) }
        



parseDateSimpleSecondRational :: SSParser ()  
parseDateSimpleSecondRational = get >>= \state -> parse (parserStateRest state) []
  where
    parse :: SSText -> SSText -> SSParser ()
    parse [] already
      | null already = modifyParserState [] 0
      | otherwise = let r = (read $ already :: Integer) R.% (10^(length already) :: Integer) :: Rational
                    in modifyParserState [] r
         
    parse (c:rest) already
      | isDigit c = parse rest (already ++ [c])
      | otherwise = throwError NotDigitCharacter

    modifyParserState :: SSText -> Rational -> SSParser ()
    modifyParserState rest r = modify $ \st -> let (SimpleSecondPrecise sec) = parserStateSimpleSecond st
                                               in st { parserStateRest = rest
                                                     , parserStateSimpleSecond = SimpleSecondPrecise (sec +r)}


