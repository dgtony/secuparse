module LogParser where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Maybe (mapMaybe, listToMaybe)

import Data.Time.Format (defaultTimeLocale, readPTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Data.List (intercalate)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map

newtype IPAddress = IPAddress (Int, Int, Int, Int) deriving (Eq, Ord)

instance Show IPAddress where
    show (IPAddress (a, b, c, d)) = intercalate "." $ map show [a, b, c, d]


newtype LogDate = LogDate Integer deriving (Show, Eq)


type ConnAttempt = (LogDate, IPAddress)


-------- parser combinators --------

digit :: ReadP Char
digit = satisfy isDigit


numbers :: Int -> ReadP Int
numbers n = read <$> count n digit


octet :: ReadP Int
octet = do
    n <- numbers 3 <++ numbers 2 <++ numbers 1
    if n > 255 then pfail
               else return n


ipAddressStraight :: ReadP IPAddress
ipAddressStraight = do
    a <- octet
    string "."
    b <- octet
    string "."
    c <- octet
    string "."
    d <- octet
    return $ IPAddress (a, b, c, d)


ipAddressBracket :: ReadP IPAddress
ipAddressBracket = do
    char '['
    a <- ipAddressStraight
    char ']'
    return a


ipAddress :: ReadP IPAddress
ipAddress = ipAddressStraight <++ ipAddressBracket


-------- parsing --------

logDateFormat :: String
logDateFormat = "%b %e %H:%M:%S"


parseDateFields :: ReadP (LogDate, String)
parseDateFields = do
    d <- readPTime True defaultTimeLocale logDateFormat
    rest <- look
    return (LogDate $ round (utcTimeToPOSIXSeconds d), rest)


runParserMaybe :: ReadP a -> String -> Maybe a
runParserMaybe p s = case readP_to_S p s of
        []           -> Nothing
        ((res, _):_) -> Just res


parseLogLine :: String -> Maybe ConnAttempt
parseLogLine s = do
    (date, rest) <- runParserMaybe parseDateFields s
    ip <- listToMaybe $ ipList rest
    return (date, ip)
    where
        ipList l = mapMaybe (runParserMaybe ipAddress) $ words l


uniqueAttempts :: [ConnAttempt] -> Map.Map IPAddress [LogDate]
uniqueAttempts = foldl (\m (d, ip) -> Map.insertWith (++) ip [d] m) Map.empty


parseLogUnique :: BS.ByteString -> Map.Map IPAddress [LogDate]
parseLogUnique content =
    uniqueAttempts $ getConnAttempts logLines
    where
        logLines = C8.lines content
        getConnAttempts = mapMaybe (parseLogLine . C8.unpack)

