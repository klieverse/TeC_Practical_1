module StartingFramework where

import ParseLib.Abstract 
import System.Environment

import System.IO
import Data.Char
import Data.List
import Text.PrettyPrint.Boxes

import Prelude hiding ((<$), (<*), (*>))

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }

instance Eq DateTime where
    (==) (DateTime d1 t1 _) (DateTime d2 t2 _) = d1 == d2 && t1 == t2

instance Ord DateTime where
    (>) (DateTime d1 t1 _) (DateTime d2 t2 _) = d1 > d2 || (d1 == d2 && t1 > t2)
    (<) (DateTime d1 t1 _) (DateTime d2 t2 _) = d1 < d2 || (d1 == d2 && t1 < t2)
    (<=) dt1 dt2 = dt1 < dt2 || dt1 == dt2
    (>=) dt1 dt2 = dt1 > dt2 || dt1 == dt2

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
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUTC

parseDate :: Parser Char Date
parseDate = (\y m d -> Date (Year y) (Month m) (Day d) ) 
            <$> concatNrOfDigits 4 <*> concatNrOfDigits 2 <*> concatNrOfDigits 2

parseTime :: Parser Char Time
parseTime = (\h m s -> Time (Hour h) (Minute m) (Second s) ) 
            <$> concatNrOfDigits 2 <*> concatNrOfDigits 2 <*> concatNrOfDigits 2

parseUTC :: Parser Char Bool
parseUTC =  True  <$ symbol 'Z' 
        <|> False <$ epsilon

-- Gets i number of digits and turns it into a valid integer
concatNrOfDigits :: Int -> Parser Char Int
concatNrOfDigits 0 = succeed 0
concatNrOfDigits i = (\a b -> a * (10 ^ (i - 1)) + b ) <$> newdigit <*> concatNrOfDigits (i - 1)


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p xs = checkResult $ parse p xs
    where   checkResult []         = Nothing
            checkResult ((x,[]):_) = Just x
            checkResult (_:xs)     = checkResult xs

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime dt = printDate (date dt) ++ "T" ++ printTime (time dt) ++ printUtc (utc dt)

printDate :: Date -> String
printDate date = printWith0 (unYear . year)   date 4 
              ++ printWith0 (unMonth . month) date 2 
              ++ printWith0 (unDay . day)     date 2

printTime :: Time -> String
printTime time = printWith0 (unHour . hour)     time 2 
              ++ printWith0 (unMinute . minute) time 2 
              ++ printWith0 (unSecond . second) time 2

-- Prints an integer of the requested length i
printWith0 :: (a -> Int) -> a -> Int -> String   
printWith0 f a i = let ss = show (f a) 
                   in replicate (i - length ss) '0' ++ ss

printUtc :: Bool -> String
printUtc b | b          = "Z"
           | otherwise  = []

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt = checkDate (date dt) && checkTime (time dt)

checkDate :: Date -> Bool
checkDate d =  Year 0  < year d 
            && Month 0 < month d && month d <  Month 13
            && Day 0   < day d   && day d   <= Day (daysInCurrentMonth d)

checkTime :: Time -> Bool
checkTime t =  hour t < Hour 24 && minute t < Minute 60 && second t < Second 60
   
-- Does the requested integer `mod` i
fmod :: (a -> Int) -> a -> Int -> Int 
fmod f a i = f a `mod` i 

timeToMinutes :: DateTime -> Int
timeToMinutes (DateTime d (Time h m _) _) = daysPast d * 1440 + unHour h * 60 + unMinute m
    
-- subtract dateTimes
minDTTime :: DateTime -> DateTime -> DateTime
minDTTime (DateTime date1 time1 t) 
          (DateTime date2 time2 _) = DateTime (minDate date1 date2) 
                                              (minTime time1 time2)
                                              t

-- subtract dates
minDate :: Date -> Date -> Date
minDate (Date (Year y1) (Month m1) (Day d1)) 
        (Date (Year y2) (Month m2) (Day d2)) = Date (Year (y1 - y2))
                                                    (Month (m1 - m2))
                                                    (Day (d1 - d2))

-- subtract times
minTime :: Time -> Time -> Time
minTime (Time (Hour h1) (Minute m1) (Second s1)) 
        (Time (Hour h2) (Minute m2) (Second s2)) = Time (Hour (h1 - h2))
                                                        (Minute (m1 - m2))
                                                        (Second (s1 - s2))            
                      
-- Calculates total days past since 0001-01-01 by adding the current day in the month, 
-- the days past in previous month, and the days past in previous years                                                     
daysPast :: Date -> Int
daysPast (Date (Year y) (Month m) (Day d)) = 
    d + sum ( map (\i -> daysInCurrentMonth (Date (Year y) (Month i) (Day 0 ))) [1..(m - 1)]) 
      + sum ( map (\i -> daysInCurrentYear (Date (Year i) (Month 12) (Day 31))) [1..(y - 1)])

-- Calculates total days in the current month
daysInCurrentMonth :: Date -> Int
daysInCurrentMonth d  | month d == Month 2 = 28 + leapDay d
                      |  (fmod (unMonth . month) d 2 == 0 && month d < Month 7)  
                      || (fmod (unMonth . month) d 2 /= 0 && month d > Month 7) = 30
                      | otherwise = 31

leapDay :: Date -> Int
leapDay d | fmod (unYear . year) d 400 == 0 = 1  -- leap year
          | fmod (unYear . year) d 100 == 0 = 0  -- not a leap year
          | fmod (unYear . year) d 4   == 0 = 1  -- leap year
          | otherwise = 0                        -- not a leap year
         
-- Calculates total days in the current year
daysInCurrentYear :: Date -> Int
daysInCurrentYear (Date (Year y) (Month m) (Day d)) = 
    d + sum (map (\i -> daysInCurrentMonth (Date (Year y) (Month i) (Day 31))) [1..m])


-- Exercise 6
data Calendar = Calendar [Calprop] [Event]
    deriving (Eq, Ord, Show)

data Calprop = Prodid String | Version20
    deriving (Eq, Ord, Show)

newtype Event = Event [EventProp]
    deriving (Eq, Ord, Show)

data EventProp =  DtStamp DateTime
                | Uid String
                | DtStart DateTime
                | DtEnd DateTime
                | Description String
                | Summary String
                | Location String
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token =  TBegin | TEnd |TVCalendar | TVEvent
            | TProdid String | TVersion
            | TBeginEvent | TDescription String | TSummary String | TLocation String | TUID String
            | TDTStamp DateTime  | TDTStart DateTime | TDTend DateTime 
            | TEndEvent |TEndCalendar 
    deriving (Eq, Ord, Show)

-- Helper function for parsing the tokens to a calendar    
isProdid, isDTStamp, isUID, isDTStart, isDTEnd, isTDescription, isTSummary, isTLocation :: Token -> Bool
isProdid        (TProdid _)      = True
isProdid        _                = False 
isDTStamp       (TDTStamp _)     = True
isDTStamp       _                = False 
isUID           (TUID _)         = True
isUID           _                = False 
isDTStart       (TDTStart _)     = True
isDTStart       _                = False 
isDTEnd         (TDTend _)       = True
isDTEnd         _                = False 
isTDescription  (TDescription _) = True
isTDescription  _                = False 
isTSummary      (TSummary _)     = True
isTSummary      _                = False 
isTLocation     (TLocation _)    = True
isTLocation     _                = False 

scanCalendar :: Parser Char [Token]
scanCalendar = (\ cp e -> [TBegin, TVCalendar] ++ sort cp ++ e ++ [TEnd, TVCalendar ])
                  <$ token "BEGIN:VCALENDAR\r\n" 
                  <*> many scanCalProp 
                  <*> (concat <$> many scanEvent)
                  <* token "END:VCALENDAR\r\n"

scanCalProp :: Parser Char Token
scanCalProp = TProdid <$ token "PRODID:" <*> many (satisfy isPrint) <* token "\r\n"
                <|> TVersion <$ token "VERSION:2.0\r\n"

scanEvent :: Parser Char [Token]
scanEvent = (\ e -> [TBegin, TVEvent] ++ sort e ++ [TEnd, TVEvent])
                <$ token "BEGIN:VEVENT\r\n" 
                <*> many scanToken 
                <* token "END:VEVENT\r\n"

scanToken :: Parser Char Token
scanToken = TDTStamp <$ token "DTSTAMP:" <*> parseDateTime <* token "\r\n"
        <|> TUID <$ token "UID:" <*> many (satisfy isPrint) <* token "\r\n"
        <|> TDTStart <$ token "DTSTART:" <*> parseDateTime <* token "\r\n"
        <|> TDTend <$ token "DTEND:" <*> parseDateTime <* token "\r\n"
        <|> TDescription <$ token "DESCRIPTION:" <*> many (satisfy isPrint) <* token "\r\n"
        <|> TSummary <$ token "SUMMARY:" <*> many (satisfy isPrint) <* token "\r\n"
        <|> TLocation <$ token "LOCATION:" <*> many (satisfy isPrint) <* token "\r\n"
            

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ symbol TBegin <* symbol TVCalendar
                         <*> parseCalProp 
                         <*> many parseEvent 
                         <* symbol TEnd <* symbol TVCalendar

parseCalProp :: Parser Token [Calprop]
parseCalProp =  (\(TProdid s) -> [Prodid s, Version20] ) 
                    <$> satisfy isProdid 
                    <* symbol TVersion

parseEvent :: Parser Token Event
parseEvent = (\descr summ loc uid stamp start end -> Event (descr ++ summ ++ loc ++ uid ++ stamp ++ start ++ end)) 
                    <$  symbol TBegin <* symbol TVEvent
                    <*> option ((\(TDescription s) -> [Description s] ) <$> satisfy isTDescription) []
                    <*> option ((\(TSummary s) -> [Summary s] ) <$> satisfy isTSummary) []
                    <*> option ((\(TLocation s) -> [Location s] ) <$> satisfy isTLocation) []
                    <*> ((\(TUID s) -> [Uid s] ) <$> satisfy isUID)
                    <*> ((\(TDTStamp dt) -> [DtStamp dt] ) <$> satisfy isDTStamp)
                    <*> ((\(TDTStart dt) -> [DtStart dt] ) <$> satisfy isDTStart)
                    <*> ((\(TDTend dt) -> [DtEnd dt] ) <$> satisfy isDTEnd)
                    <*  symbol TEnd <* symbol TVEvent

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
    handle <- openFile fp ReadMode
    hSetNewlineMode handle noNewlineTranslation
    content <- hGetContents handle
    return $ recognizeCalendar content

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar cp e) = "BEGIN:VCALENDAR \r\n" ++ 
                                concatMap printCalProp cp ++ 
                                concatMap printEvent e ++
                                "END:VCALENDAR \r\n" 

printCalProp :: Calprop -> String
printCalProp (Prodid s) = "PRODID:" ++ s ++ "\r\n"
printCalProp Version20 = "VERSION:2.0 \r\n"

printEvent :: Event -> String
printEvent (Event eps) = "BEGIN:VEVENT \r\n" ++
                         concatMap printEventProp eps ++
                         "END:VEVENT \r\n" 

printEventProp :: EventProp -> String
printEventProp (DtStamp     v) = "DTSTAMP:" ++ printDateTime v ++ "\r\n"
printEventProp (Uid         v) = "UID:" ++ v ++ "\r\n"
printEventProp (DtStart     v) = "DTSTART:" ++ printDateTime v ++ "\r\n"
printEventProp (DtEnd       v) = "DTEND:" ++ printDateTime v ++ "\r\n"
printEventProp (Description v) = "DESCRIPTION:" ++ v ++ "\r\n"
printEventProp (Summary     v) = "SUMMARY:" ++ v ++ "\r\n"
printEventProp (Location    v) = "LOCATION:" ++ v ++ "\r\n"

-- Exercise 10

isDtStart, isDtEnd, isSummary :: EventProp -> Bool
isDtStart (DtStart _ ) = True
isDtStart _            = False
isDtEnd (DtEnd _ ) = True
isDtEnd _               = False
isSummary (Summary _) = True
isSummary _ = False

-- return the amount of events in the calendar
countEvents :: Calendar -> Int
countEvents (Calendar _ e) = length e

-- returns the amount of events that happen at a given time
findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ e) = filter (happensAtTime dt) e

-- returns whether the event happens at the given time
happensAtTime :: DateTime -> Event -> Bool
happensAtTime dt event = let (startTime, endTime) = getStartEndTime event 
                         in dt >= startTime && dt < endTime

-- returns whether there are any overlapping events in the calendar                         
checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ evs) =  any (\i -> let (ev, rest) = getEvents i evs
                                                in any (doOverlap ev) rest ) [0 .. length evs - 1]

-- returns the event at place i together with the rest of the events in the list                                                 
getEvents :: Int -> [Event] -> (Event, [Event])
getEvents 0 (e:events) = (e, events)
getEvents i events = getEvents (i - 1) (tail events)

-- checks whether 2 events overlap
doOverlap :: Event -> Event -> Bool 
doOverlap event1 event2 |  (s1 >= s2 && e2 >= s1) 
                        || (s2 >= s1 && s2 <= e1) = True
                        | otherwise = False
                where (s1, e1) = getStartEndTime event1
                      (s2, e2) = getStartEndTime event2

-- returns how much time was spent at a give summary
timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar _ es) = sum $ map eventToMinutes $ filter (hasSummary summary) es

-- checks whether the event has a given summary
hasSummary :: String -> Event -> Bool
hasSummary summary (Event eps) = s == summary
    where [Summary s] = filter isSummary eps

-- returns the amount of minutes spent on a given event
eventToMinutes :: Event -> Int
eventToMinutes event = let (st, et) = getStartEndTime event 
                       in timeToMinutes $ minDTTime et st

-- returns the start dateTime and end dateTime 
getStartEndTime :: Event -> (DateTime, DateTime)
getStartEndTime (Event eps) = (startTime, endTime)
    where [DtStart startTime] = filter isDtStart eps
          [DtEnd   endTime]   = filter isDtEnd   eps

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = render $ createGrid c 7 y m

-- returns a list of events that happens on the current date
getEventsOnDate :: Date -> Calendar -> [Event]
getEventsOnDate dt (Calendar _ evs) = filter (isOnDate dt) evs

-- Checks whether the event happens on a given date
isOnDate :: Date -> Event -> Bool
isOnDate dt ev = let (DateTime sdt _ _, DateTime edt _ _) = getStartEndTime ev
                 in sdt <= dt && dt <= edt

-- concatenates the weeks vertically with a punctuation in between                 
createGrid :: Calendar -> Int -> Year -> Month -> Box
createGrid c gw y m = punctuateV top horizontalDivider $ createColumns 1 allDates
  where
    createColumns :: Int -> [Date] -> [Box]
    createColumns _ [] = []
    createColumns i xs = createRow c 16 (take gw xs) : createColumns (i+gw) (drop gw xs)
    horizontalDivider = text (intercalate "-+-" (replicate gw a))
    a = replicate 16 '-'
    days = daysInCurrentMonth $ Date y m (Day 0)
    allDates = [Date y m (Day d) | d <- [1..days]]

-- concatenates the days horizontally with the punctuation in between    
createRow :: Calendar -> Int -> [Date] -> Box
createRow c bw ds = punctuateH left verticalDivider boxes
  where
    boxes = map (createDay c bw) ds
    bh = maximum (map rows boxes)
    verticalDivider =  foldl' (//) nullBox $ replicate bh (text " | ")

-- concatenates the time of the events vertically
createDay :: Calendar -> Int -> Date -> Box
createDay c bw dt@(Date _ _ d) = text dayLine // vcat top (map (para left bw) ss)
  where
    ts = map (extractTime . getStartEndTime) (getEventsOnDate dt c)

    ss = map timeRangeToString ts
    
    extractTime (DateTime _ t1 _, DateTime _ t2 _) = (t1,t2)
    dayLine = printWith0 unDay d 2 ++ replicate (bw - 2) ' '

-- given a start and end time returns the string   
timeRangeToString :: (Time, Time) -> String
timeRangeToString (t1,t2) = tToString t1 ++ " - " ++ tToString t2
  where 
    tToString (Time h m _) = printWith0 unHour h 2 ++ ":" ++ printWith0 unMinute m 2 


