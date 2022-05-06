module WeatherStats where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import qualified System.Exit as Exit

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Observation = 
   Observation 
     { day :: Text
     , hourOfDay :: Text
     , temperature :: Float
     , precipitation :: Float
     , windspeed :: Float
     } 
   deriving (Eq, Show)

temp (Observation _ _ t _ _) = t
hour (Observation _ h _ _ _) = h

instance FromNamedRecord Observation where    
   parseNamedRecord m = 
     Observation 
       <$> m.: "Date"
       <*> m.: "Time"
       <*> m.: "Temp" 
       <*> m.: "Precip"
       <*> m.: "Wind"

decodeItems :: ByteString -> Either String (Vector Observation)
decodeItems = fmap snd . Cassava.decodeByName

decodeItemsFromFile :: FilePath -> IO (Either String (Vector Observation))

decodeItemsFromFile filePath = catchShowIO (ByteString.readFile filePath) >>= return . either Left decodeItems

catchShowIO :: IO a -> IO (Either String a)

catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where 
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

-- Function to simply read the data. Note that any errors during reading of the csv file will stop the program. To debug file I/O, call dcodeItemsFromFile directly
readData filename = do 
  x <- decodeItemsFromFile filename
  case x of 
     Left reason -> return []
     Right obsData -> return (Vector.toList obsData)

--------------------------------------------
-- DO NOT CHANGE ANYTHING ABOVE THIS LINE --
--------------------------------------------
-- Name: Samuel Morse
-- Partner: Matthew Brown
--------------------------------------------


-- Question 1a

myaverage :: [Float] -> Float
myaverage a = foldl1 (+) a / (mylength a)

-- Question 1b
maxDiff :: [Float] -> Float
maxDiff a = (maxval a) - (minval a)

-- Question 1c
daySummary day = do
  obsData <- readData "2069-2021.csv"
  let averagetemp = myaverage (map temp (take 24 (drop (day*24 - 24) obsData)))
  let maxtempdiff = maxDiff (map temp (take 24 (drop (day*24 - 24) obsData)))
  let averagewind = myaverage (map wind (take 24 (drop (day*24 - 24) obsData)))
  let maxwindspeeddiff = maxDiff (map wind (take 24 (drop (day*24 - 24) obsData)))
  let result = (day, averagetemp, maxtempdiff, averagewind, maxwindspeeddiff)
  print result

--- any other functions you need for 1a-1c go here

wind(Observation _ _ _ _ w) = w

mylength :: [Float] -> Float
mylength [] = 0
mylength l = 1 + (mylength (tail l))

maxval :: [Float] -> Float
maxval [] = 0
maxval a = max (head a) (maxval(tail a))

minval :: [Float] -> Float
minval [] = 1/0
minval a = min (head a) (minval(tail a))
  
-- Question 2a
chunkby :: Int -> [a] -> [[a]]
chunkby n [] = []
chunkby n l = ((take n l) : chunkby n (drop n l))

-- Question 2b
chunkByDays :: [a] -> [[a]]
chunkByDays = chunkby 24

-- 3a: ([Float] -> a) -> Int -> [Observation] -> a
-- 3a: This function takes a function (something like maxval or minval), 
--  which is a list of floats to a value. It also takes an Int (day) and a
--  list of observations. The function produces a value for the stat (a)
dailyTemperatureStat :: ([Float] -> a) -> Int -> [Observation] -> a
dailyTemperatureStat f day obsData = f (map temp dayList)  
  where 
    h = 24*(day-1)
    dayList = (take 24 (drop h obsData))


-- Question 3b
allMinimumTemp filename = do 
  obsData <- readData filename
  let result = minTempList ((length obsData) `div` 24) obsData
  putStr "Minimum temperature for each day of the year:"
  print (result)

minTempList :: Int -> [Observation] -> [(Int, Float)]
minTempList 1 obsData = [(1, dailyTemperatureStat minval 1 obsData)]
minTempList n obsData = (n, dailyTemperatureStat minval n obsData) : (minTempList (n-1) obsData)

-- Question 3c
-- bonus - I added a filter to throw out temperature differences of anything over a threshold
highDifferentialDays filename = do
  obsData <- readData filename
  let result = highDiffList ((length obsData) `div` 24) obsData
  -- using filter and lamda functions to get second element of 3 element tuple
  let difflist = filter (\(_,a,_) -> a > 15) result
  let cleaneddata = filter (\(_,a,_) -> a < 80) difflist
  putStr "high differential days:"
  return cleaneddata

highDiffList :: Int -> [Observation] -> [(Int, Float, Float)]
highDiffList 1 obsData = [(1, (dailyTemperatureStat maxDiff 1 obsData), (dailyTemperatureStat myaverage 1 obsData))]
highDiffList n obsData = (n, (dailyTemperatureStat maxDiff n obsData), (dailyTemperatureStat myaverage n obsData)) : (highDiffList (n-1) obsData)