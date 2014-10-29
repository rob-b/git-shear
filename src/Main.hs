{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shear.Option (usage , options, defaultOptions, Options(..))

import System.IO
import System.Exit
import System.Process
import System.Environment     (getArgs)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.List              (sort)
import Data.Char              (isSpace)
import Data.Either            (lefts, rights)
import System.Console.GetOpt  (getOpt, ArgOrder(..))


exitIfErrors :: [String] -> IO String
exitIfErrors [] = return ""
exitIfErrors es = do
    u <- usage
    let msg = unlines(map rstrip es) ++ "\n\n" ++ u
    hPutStrLn stderr msg
    exitWith $ ExitFailure 10

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse


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


data ShellError = ShellError String Int

refExists :: String -> IO (Either ShellError T.Text)
refExists refname = do
    (code, stdo, stde) <- readProcessWithExitCode "git" ["rev-parse", refname, "--"] ""
    case code of
        ExitFailure c -> return . Left $ ShellError stde c
        ExitSuccess   -> return . Right $ T.replace "\n--" "" (T.pack stdo)


getBranchNames :: String -> [T.Text]
getBranchNames s = sort . filterBranches . extractBranches $ T.pack s


stripRemoteFromName :: T.Text -> [T.Text] -> [T.Text]
stripRemoteFromName r xs = map (T.replace (r `T.append` "/") "") xs


branchCount :: [T.Text] -> T.Text
branchCount bs = "Would delete the following " `T.append` amount `T.append` " branch(es):"
    where amount = T.pack . show $ length bs


branchDeleteCmds :: [T.Text] -> [T.Text]
branchDeleteCmds bs = [T.append "git push origin --delete " x | x <- bs]


shear :: Bool -> [T.Text] -> IO T.Text
shear doCommand cmds = case (doCommand, cmds) of
    (_, []) -> do
        error "No command to execute"
    (False, xs) -> do
        TIO.putStrLn $ branchCount xs
        exec $ getAllCmds $ map (flip T.append " --dry-run") xs
        return ""
    (True, xs) -> do
        exec $ getAllCmds xs
        return ""
    where separate (_:xx) = ("git", (map T.unpack xx))
          getAllCmds = map (separate .T.words)
          exec cs = do
            x <- mapM (\t -> runCmds (fst t) (snd t)) cs
            TIO.putStrLn $ T.unlines (rights x)
            putStrLn . unlines $ map ss (lefts x)


runCmds :: String -> [String] -> IO (Either ShellError T.Text)
runCmds fname args = do
    (code, _, stde) <- readProcessWithExitCode fname args ""
    case code of
        ExitFailure c -> return . Left $ ShellError stde c
        ExitSuccess   -> return . Right $ T.pack stde


takeBranches :: Maybe Int -> [T.Text] -> [T.Text]
takeBranches _ []            = []
takeBranches Nothing  (x:xs) = x:xs
takeBranches (Just i) (x:xs) = take i (x:xs)


validateNonOptions :: [String] -> Either String String
validateNonOptions xs = case xs of
    []     -> Left "You must specify a refname"
    [i]    -> Right i
    (_:is) -> Left $ unlines ["Unsupported option: " ++ x | x <- is]


ss :: ShellError -> String
ss (ShellError stde code) = "Result: " ++ stde


exitWithShellError :: ShellError -> IO b
exitWithShellError sherror = do
    hPutStrLn stderr $ ss sherror
    exitWith $ ExitFailure 128


doNothing :: T.Text -> IO String
doNothing _ = return ""


main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
    opts <- foldl (>>=) (return defaultOptions) actions

    _ <- exitIfErrors errors
    _ <- case validateNonOptions nonOptions of
        Left err      -> exitIfErrors [err]
        Right refname -> do
            ref <- refExists refname
            either exitWithShellError doNothing ref

    let refname = either (error "mismatch") id (validateNonOptions nonOptions)
    output <- mergedRemotes refname
    let Options {optLimit = limit, optDryRun = dryRun, optRemote = remote} = opts

    -- ideally, takeBranches should be part of getBranchNames so that we
    -- reduce the list of branches _before_ filtering and sorting it
    let bs = (stripRemoteFromName remote) . (takeBranches limit) . getBranchNames $ output
    _ <- shear (not dryRun) $ branchDeleteCmds bs
    return ()
