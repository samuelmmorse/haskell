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

-- -- Question 2b
-- chunkByDays :: [a] -> [[a]]
chunkByDays = do
  obsData <- readData "2069-2021.csv"
  print $ chunkby 24 obsData


-- -- 3a: add type declaration here 
-- -- 3a: add an explanation
-- dailyTemperatureStat :: ...
-- dailyTemperatureStat f day obsData = f (map temp dayList)  
--   where 
--     h = 24*(day-1)
--     dayList = (take 24 (drop h obsData))



-- -- Example function for 3b: Computes the minimum temperature for Jan 3 based on the 24 hourly measurements
-- jan3Minimum filename = do 
--   obsData <- readData filename
--   let result = minimum (map temp (take 24 (drop 48 obsData)))
--   putStr "minimum temperature on January 3 = "
--   print (result)

-- -- Question 3b
-- allMinimumTemp filename = do 
--   obsData <- readData filename
--   let result = ...
--   putStr "Minimum temperature for each day of the year:"
--   print (result)

-- -- Question 3c
-- highDifferentialDays filename = ...
  