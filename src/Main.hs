module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as BS

import Control.Monad (forM_)

import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import LogParser (parseLogUnique, LogDate(..), IPAddress(..))
import TablePrinter (printTable, traverseTransform, SortOrder(..))
import ArgParser



printNumAttempts :: Map.Map IPAddress [LogDate] -> Int -> IO ()
printNumAttempts m maxAddr = printTable tBorder tTitle tContent
    where
        tBorder  = "+-------------------+------------+"
        tTitle   = "|         IP        |  Attempts  |"
        tContent = forM_ (take maxAddr sortedAssList) (uncurry printRes)
        sortedAssList :: [(IPAddress, Int)]
        sortedAssList = traverseTransform m length Desc
        printRes ip attemptTime = printf "|%17s  |%8s    |\n" (show ip) (show attemptTime)


printStats logfile attempts = putStrLn $
    "Ok, file " ++ logfile ++ " parsed: " ++ show attempts ++ " unique IPs detected"



main :: IO ()
main = do
    -- parse CLI arguments
    (Opts logfile showAll maxOutput) <- getOpts

    -- read and parse log file
    parseResult <- parseLogUnique <$> BS.readFile logfile

    -- output stats
    let totalAttempts = length parseResult
    printStats logfile totalAttempts

    -- print results
    if showAll
        then printNumAttempts parseResult totalAttempts
        else printNumAttempts parseResult maxOutput

