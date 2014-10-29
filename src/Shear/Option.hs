{-# LANGUAGE OverloadedStrings #-}
module Shear.Option (
      showVersion
    , showHelp
    , usage
    , options
    , defaultOptions
    , Options(..)
    ) where

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import qualified Data.Text as T


usage :: String
usage = usageInfo header options
  where
    header = "Usage: git shear [OPTION...] REFNAME\n" ++
             "Delete remote branches that have been merged into a given ref.\n"


showVersion :: Options -> IO Options
showVersion _ = do
    hPutStrLn stderr "0.1.0"
    exitSuccess


showHelp :: Options -> IO Options
showHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess


data Options = Options { optLimit  :: Maybe Int
                       , optRemote :: T.Text
                       , optDryRun :: Bool}
               deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optRemote = "origin", optDryRun  = False, optLimit = Nothing}


options :: [OptDescr (Options -> IO Options)]
options = [ Option "h" ["help"] (NoArg showHelp) "Display help message."
          , Option "v" ["version"] (NoArg showVersion) "Show the version."

          , Option "l" ["limit"]
            (ReqArg (\l opt -> return opt {optLimit = Just $ read l}) "LIMIT")
            "Limit the number of branches that will be deleted"
          , Option "n" ["dry-run"]
            (NoArg (\opt -> return opt {optDryRun = True} ))
            "Do not actually delete stale branches"
          ]
