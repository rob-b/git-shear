{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Exit
import System.Process
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import System.Console.GetOpt


data Options = Options { optBranch  :: Maybe String}
               deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optBranch  = Nothing }

options :: [OptDescr (Options -> IO Options)]
options = [ Option "h" ["help"]
            (NoArg (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
            "Display help message."

          , Option "v" ["version"]
            (NoArg (\_ -> do
               hPutStrLn stderr "0.1.0"
               exitWith ExitSuccess))
            "Show the version."

          , Option "b" ["integration-branch"]
            (ReqArg (\b opt -> return opt {optBranch = Just b}) "BRANCH")
            "The branch into which branches must be merged to be considered for shearing."
          ]



isProtectedBranch :: T.Text -> Bool
isProtectedBranch t = any isInfixOf ["origin/develop", "origin/master"]
    where isInfixOf = flip T.isInfixOf t

isNotProtectedBranch :: T.Text -> Bool
isNotProtectedBranch t = not (isProtectedBranch t)


filterBranches :: [T.Text] -> [T.Text]
filterBranches = filter isNotProtectedBranch


extractBranches :: T.Text -> [T.Text]
extractBranches s = map T.strip (T.lines s)


mergedRemotes :: String -> IO String
mergedRemotes refname = readProcess "git" ["branch", "-r", "--merged", refname] ""


main :: IO ()
main = do
    output <- mergedRemotes "origin/develop"
    let candidates = sort . filterBranches . extractBranches $ T.pack output
    let candidatesCount = "Would delete the following " ++ show (length candidates) ++ " branch(es):"
    putStrLn candidatesCount
    putStrLn ""
    -- TIO.putStr . T.unlines $ candidates
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
    opts <- foldl (>>=) (return defaultOptions) actions

    let Options { optBranch = branch } = opts
    case branch of
        Nothing -> putStrLn "Branch is needed"
        Just s  -> do
            let n = "Integration branch is: " ++ show s
            putStrLn n
    -- print nonOptions
    -- print errors
