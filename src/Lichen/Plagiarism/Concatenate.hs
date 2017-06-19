module Lichen.Plagiarism.Concatenate where

import System.Directory
import System.Process
import System.IO

import Control.Monad

import Lichen.Config
import Lichen.Util
import Lichen.Plagiarism.AssignmentSettings

findActive :: FilePath -> IO (Maybe FilePath)
findActive p = do
        pas <- getAssignmentSettings (p ++ "/user_assignment_settings.json")
        case pas of Left err -> putStrLn err >> return Nothing
                    Right as -> return $ Just (p ++ "/" ++ show (activeVersion as))

runCat :: FilePath -> [FilePath] -> IO ()
runCat dst srcs = do
        out <- openFile dst WriteMode
        (_, _, _, ph) <- createProcess (proc "/bin/cat" srcs) { std_out = UseHandle out }
        void $ waitForProcess ph

concatenate :: FilePath -> IO ()
concatenate p = do
        studentDirs <- fmap (\x -> dir ++ x) <$> listDirectory dir
        studentActiveDirs <- mapM findActive studentDirs
        let students = purifySnd $ zip studentDirs studentActiveDirs
        dstSrc <- mapM toDstSrc students
        mapM_ (createDirectoryIfMissing True . containingDir . fst) dstSrc
        mapM_ (uncurry runCat) dstSrc
    where dir = if last p == '/' then p else p ++ "/"
          toDstSrc (s, active) = (,) <$> (("plagiarism_data/concatenated"++) <$> canonicalizePath s)
                                     <*> (fmap (\x -> active ++ "/" ++ x) <$> listDirectory active)
