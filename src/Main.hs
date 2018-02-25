module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Lib


data Opts = Opts
  { cpu           :: Bool
  , delay         :: Int
  , numOfDecimals :: Int
  , mem           :: Bool }

usage :: Parser Opts
usage = Opts
  <$> switch
      ( long "cpu"
     <> short 'c'
     <> help "whether to show cpu uasge." )
  <*> option auto
      ( long "delay"
     <> value 2000000
     <> help "cpu delay."
     <> showDefault
     <> metavar "INT" )
  <*> option auto
      ( long "decimals"
     <> value 1
     <> help "number of decimals."
     <> showDefault
     <> metavar "INT" )
  <*> switch
      ( long "mem"
     <> short 'm'
     <> help "whether to show memory uasge." )

pprefs :: ParserPrefs
pprefs = prefs showHelpOnError

opts :: ParserInfo Opts
opts = info (helper <*> usage)
      ( fullDesc
     <> header "hs-cpu-mem - simple program that show your cpu && memory uasge" )

showHelpText :: IO ()
showHelpText = handleParseResult . Failure $
  parserFailure pprefs opts ShowHelpText mempty

main :: IO ()
main = run =<< customExecParser pprefs opts

run :: Opts -> IO ()
run (Opts True d n False) = putStr =<< cpuUsage d n
run (Opts False _ _ True) = putStr =<< memUsage
run (Opts True d n True) = do
  m <- memUsage
  c <- cpuUsage d n
  putStr $ m ++ " " ++ c
run _ = showHelpText
