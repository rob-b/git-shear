{-# LANGUAGE OverloadedStrings #-}

import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)


-- TODO: Rename to shear?


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
    TIO.putStr . T.unlines $ candidates
