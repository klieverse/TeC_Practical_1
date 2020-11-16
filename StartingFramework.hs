module StartingFramework where

import ParseLib.Abstract
import System.Environment

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = (\d _ t b -> DateTime d t b) <$> parseDate <*> symbol 'T' <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool
parseUTC = const True <$> symbol 'Z' 
        <|> const False <$> epsilon

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = concat1000 Year <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseMonth :: Parser Char Month
parseMonth = concat10 Month <$> newdigit <*> newdigit

parseDay :: Parser Char Day
parseDay = concat10 Day <$> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = concat10 Hour <$> newdigit <*> newdigit

parseMinute :: Parser Char Minute
parseMinute = concat10 Minute <$> newdigit <*> newdigit

parseSecond :: Parser Char Second
parseSecond = concat10 Second <$> newdigit <*> newdigit

concat10 :: (Int -> a) -> Int -> Int -> a
concat10 f a b = f (10*a + b)

concat1000 :: (Int -> a) -> Int -> Int -> Int -> Int -> a
concat1000 f a b c d = f (1000*a + 100*b + 10*c + d)

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p xs = case parse p xs of
                [] -> Nothing
                (x:_) -> Just $ fst x

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime dt = printDate (date dt) ++ "T" ++ printTime (time dt) ++ printUtc (utc dt)

printDate :: Date -> String
printDate d = print1000 (unYear $ year d) ++ print10 (unMonth $ month d) ++ print10 (unDay $ day d)

printTime :: Time -> String
printTime t = print10 ( unHour $ hour t) ++ print10 (unMinute $ minute t) ++ print10 (unSecond $ second t)

print10 :: Int -> String
print10 a | a < 1 = "00"
          | a < 10 = "0" ++ show a
          | otherwise = show a

print1000 :: Int -> String
print1000 a | a < 1 = "0000"
            | a < 10 = "000" ++ show a
            | a < 100 = "00" ++ show a
            | a < 1000 = "0" ++ show a
            | otherwise = show a

printUtc :: Bool -> String
printUtc b | b          = "Z"
           | otherwise  = []

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined

-- Exercise 6
data Calendar = Calendar
    deriving (Eq, Ord, Show)

data Event = Event
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

