module Lichen.Plagiarism.Concatenate where

import System.IO
import System.Process
import System.Directory
import System.FilePath

import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Reader

import Lichen.Config
import Lichen.Util
import Lichen.Plagiarism.AssignmentSettings

-- Given a student submission directory, parse the students
-- user_assignment_settings.json file and return the path to the directory
-- containing that student's active submission.
findActive :: FilePath -> IO (Maybe FilePath)
findActive p = do
        pas <- getAssignmentSettings (p </> "user_assignment_settings.json")
        case pas of Left err -> putStrLn err >> return Nothing
                    Right as -> return $ Just (p </> show (activeVersion as))

-- Given a destination file and a list of source files, call UNIX cat to
-- concatenate the source files and output the result in the destination
-- file.
runCat :: FilePath -> [FilePath] -> IO ()
runCat dst srcs | null srcs = return ()
                | otherwise = do
                    out <- openFile dst WriteMode
                    (_, _, _, ph) <- createProcess (proc "/bin/cat" srcs) { std_out = UseHandle out }
                    void $ waitForProcess ph

concatenate :: FilePath -> FilePath -> Plagiarism ()
concatenate base p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        studentActiveDirs <- liftIO $ mapM findActive studentDirs
        let students = purifySnd $ zip studentDirs studentActiveDirs
        dstSrc <- liftIO $ mapM (toDstSrc config) students
        let toCreate = Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (\x -> removeDirectoryRecursive x >> createDirectoryIfMissing True x) toCreate
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = base </> dataDir c </> concatDir c
          toDstSrc c (s, active) = (,) <$> ((wd c ++) <$> canonicalizePath s)
                                       <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> active </> x) <$> listDirectory active)
