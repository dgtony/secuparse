module ArgParser where

import Options.Applicative
import Data.Semigroup ((<>))


data Opts = Opts
    { logfile :: String
    , showAll :: Bool
    , numAddr :: Int } deriving (Eq, Show)


argOpts :: Parser Opts
argOpts = Opts
    <$> strOption
        ( long "logfile"
        <> short 'f'
        <> metavar "FILE"
        <> help "Log file" )
    <*> switch
        ( long "all"
        <> short 'a'
        <> help "Show all IP-addresses" )
    <*> option auto
        ( long "number"
        <> short 'n'
        <> help "Number of IP-addresses in the table"
        <> showDefault
        <> value 20
        <> metavar "INT" )


getOpts :: IO Opts
getOpts = execParser opts
    where opts = info (argOpts <**> helper)
                      ( fullDesc
                        <> progDesc ( "Print table with number of auth attempts for each " ++
                                      "detected IP-address in the descending order" )
                        <> header "Utility for parsing standard security logs in CentOS" )
