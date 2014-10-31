{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import System.Process
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.List              (sort)
import Data.Char              (isSpace)
import Data.Either            (lefts, rights)
import Options.Applicative


rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse


isProtectedBranch :: T.Text -> Bool
isProtectedBranch t = any isInfixOf ["origin/develop", "origin/master"]
    where isInfixOf = flip T.isInfixOf t


filterBranches :: [T.Text] -> [T.Text]
filterBranches = filter isNotProtectedBranch
    where isNotProtectedBranch t = not (isProtectedBranch t)


extractBranches :: T.Text -> [T.Text]
extractBranches s = map T.strip (T.lines s)


mergedRemotes :: String -> IO String
mergedRemotes refname' = readProcess "git" ["branch", "-r", "--merged", refname'] ""


data ShellError = ShellError { shellErrorMsg :: String
                             , shellErrorCode :: Int }

refExists :: String -> IO (Either ShellError T.Text)
refExists refname' = do
    (code, stdo, stde) <- readProcessWithExitCode "git" ["rev-parse", refname', "--"] ""
    case code of
        ExitFailure c -> return . Left $ ShellError stde c

        -- FIXME: a crazy amount of String -> Text -> String transformations
        ExitSuccess   -> return . Right .T.pack $ tidyName stdo
          where tidyName s = rstrip . T.unpack $ T.replace "--" "" (T.pack s)


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
            putStrLn . unlines $ map shellErrorMsg (lefts x)


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


data App = App { refname :: String
               , dryRun :: Bool }

app :: Parser App
app = App
    <$> argument str (metavar "REFNAME")
    <*> switch (long "dry-run" <> help "Actually delete the stale branches")


-- ideally, takeBranches should be part of getBranchNames so that we
-- reduce the list of branches _before_ filtering and sorting it
names ref limit remote = fmap pipeline $ mergedRemotes ref
    where pipeline = (stripRemoteFromName remote) . (takeBranches $ Just limit ). getBranchNames


run (App refname dryRun) = do
    ref <- refExists refname
    case ref of
         Left err   -> error $ shellErrorMsg err
         Right hash -> do
             branches <- names (T.unpack hash) 4 "origin"
             _ <- shear (not dryRun) $ branchDeleteCmds branches
             return ()


main :: IO ()
main = do
    execParser opts >>= run
      where
          opts = info (helper <*> app)
            (fullDesc
            <> progDesc "Delete branches that have been merged into REFNAME"
            <> header "git-shear - delete stale remote branches")
