module Lichen.Plagiarism.Concatenate where

import System.IO
import System.Process
import System.Directory
import System.FilePath

import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Reader
import Control.Arrow (second)

import Lichen.Util
import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.AssignmentSettings

-- Given a student submission directory, parse the students
-- user_assignment_settings.json file and return the path to the directory
-- containing that student's active submission.
findActive :: FilePath -> Plagiarism FilePath
findActive p = (p </>) . show . activeVersion <$> getAssignmentSettings (p </> "user_assignment_settings.json")

-- Given a destination file and a list of source files, call UNIX cat to
-- concatenate the source files and output the result in the destination
-- file.
runCat :: FilePath -> [FilePath] -> IO ()
runCat dst srcs | null srcs = return ()
                | otherwise = do
                    out <- openFile dst WriteMode
                    (_, _, _, ph) <- createProcess (proc "/bin/cat" srcs) { std_out = UseHandle out }
                    void $ waitForProcess ph

concatenate :: FilePath -> Plagiarism ()
concatenate p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        students <- mapM (clean . second findActive) $ zip studentDirs studentDirs
        dstSrc <- liftIO $ mapM (toDstSrc config) students
        let toCreate = Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (\x -> removeIfDoesntExist x >> createDirectoryIfMissing True x) toCreate
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = dataDir c </> concatDir c
          clean (s, m) = (,) <$> pure s <*> m
          toDstSrc c (s, active) = (,) <$> ((wd c ++) <$> canonicalizePath s)
                                       <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> active </> x) <$> listDirectory active)
