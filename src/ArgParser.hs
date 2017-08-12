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
        ( long "showall"
        <> short 'a'
        <> help "Show all the addresses" )
    <*> option auto
        ( long "number"
        <> short 'n'
        <> help "How many addresses to show"
        <> showDefault
        <> value 20
        <> metavar "INT" )


getOpts :: IO Opts
getOpts = execParser opts
    where opts = info (argOpts <**> helper)
                      ( fullDesc
                        <> progDesc "Parse standard security logs in CentOS"
                        <> header "Print table with number of auth attempts for each IP-address" )
