module Lichen.Plagiarism.Concatenate where

import System.IO
import System.Process
import System.Directory
import System.FilePath

import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Reader
import Control.Arrow (second)

import Text.Read

import Lichen.Util
import Lichen.Languages
import Lichen.Plagiarism.Config
import Lichen.Plagiarism.AssignmentSettings

-- Given a student submission directory, parse the students
-- user_assignment_settings.json file and return the path to the directory
-- containing that student's active submission.
findActive :: FilePath -> Plagiarism FilePath
findActive p = do
        active <- activeVersion <$> getAssignmentSettings (p </> "user_assignment_settings.json")
        if active == 0 then findLatest p else return $ p </> show active

findLatest :: FilePath -> Plagiarism FilePath
findLatest p = liftIO $ (p </>) . show . maximum . purify . fmap rint <$> listDirectory p
    where rint :: String -> Maybe Integer
          rint = readMaybe

findAll :: FilePath -> Plagiarism [FilePath]
findAll p = liftIO $ listDirectory p

-- Given a destination file and a list of source files, call UNIX cat to
-- concatenate the source files and output the result in the destination
-- file.
runCat :: FilePath -> [FilePath] -> IO ()
runCat dst srcs | null srcs = return ()
                | otherwise = do
                    out <- openFile dst WriteMode
                    (_, _, _, ph) <- createProcess (proc "/bin/cat" srcs) { std_out = UseHandle out }
                    void $ waitForProcess ph

concatenateActive :: FilePath -> Plagiarism ()
concatenateActive p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        students <- mapM (clean . second findActive) $ zip studentDirs studentDirs
        dstSrc <- liftIO $ mapM (toDstSrc config) students
        liftIO . mapM_ (\x -> removeDir x >> createDirectoryIfMissing True x) . Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = outputDir c </> concatDir c
          clean (s, m) = (,) <$> pure s <*> m
          toDstSrc c (s, active) = (,) <$> ((wd c ++) <$> canonicalizePath s)
                                       <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> active </> x) <$> listDirectory active)

concatenateAll :: FilePath -> Plagiarism ()
concatenateAll p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        students <- mapM (clean . second findAll) $ zip studentDirs studentDirs
        dstSrc <- liftIO $ mconcat <$> mapM (toDstSrc config) students
        liftIO . mapM_ (\x -> removeDir x >> createDirectoryIfMissing True x) . Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = outputDir c </> concatDir c
          clean (s, m) = (,) <$> pure s <*> m
          toDstSrc :: Config -> (FilePath, [FilePath]) -> IO [(FilePath, [FilePath])]
          toDstSrc c (s, active) = mapM (\a -> (,) <$> ((++ a) . (++ "_") . (wd c ++) <$> canonicalizePath s)
                                                  <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> a </> x) <$> listDirectory a))
                                        active
