{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Main where

import System.Directory
import System.FilePath

import Data.Aeson
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Util
import Lichen.Error
import Lichen.Config
import Lichen.Languages
import Lichen.Plagiarism.Config
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Plagiarism.Report
import Lichen.Plagiarism.Walk
import Lichen.Plagiarism.Shared

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "config-file" <> short 'c' <> metavar "PATH" <> showDefault <> value (configFile dc) <> help "Configuration file")
               <*> strOption (long "output-dir" <> short 'o' <> metavar "DIR" <> showDefault <> value (outputDir dc) <> help "Directory to store generated output")
               <*> strOption (long "concat-dir" <> short 'c' <> metavar "DIR" <> showDefault <> value (concatDir dc) <> help "Subdirectory of output directory storing concatenated student code")
               <*> strOption (long "highlight-dir" <> short 'i' <> metavar "DIR" <> showDefault <> value (highlightDir dc) <> help "Subdirectory of output directory storing syntax-highlighted student code")
               <*> strOption (long "report-dir" <> short 'r' <> metavar "DIR" <> showDefault <> value (reportDir dc) <> help "Subdirectory of output directory storing the HTML report")
               <*> (T.pack <$> strOption (long "report-title" <> metavar "TITLE" <> showDefault <> value (T.unpack $ reportTitle dc) <> help "Title of pages in the HTML report"))
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> option auto (long "top-matches" <> short 't' <> metavar "N" <> showDefault <> value (topMatches dc) <> help "Number of top matches to report")
               <*> (generatePathChoice (pathGenerator dc) <$> (optional . strOption $ long "path-generator" <> metavar "GENERATOR" <> help "Path generation method for reports"))
               <*> strOption (long "semester" <> metavar "SEMESTER" <> value (submittySemester dc) <> help "Semester for Submitty path generation")
               <*> strOption (long "course" <> metavar "COURSE" <> value (submittyCourse dc) <> help "Course for Submitty path generation")
               <*> strOption (long "assignment" <> metavar "ASSIGNMENT" <> value (submittyAssignment dc) <> help "Assignment for Submitty path generation")
               <*> switch (long "all-versions" <> help "Should all submission versions be checked?")
               <*> option auto (long "shared-threshold" <> short 's' <> metavar "N" <> showDefault <> value (sharedThreshold dc) <> help "Threshold for commmon code to be considered shared code.")
               <*> optional (argument str (metavar "SOURCE_DIR"))
               <*> many (argument str (metavar "PAST_DIRS"))

realMain :: Config -> IO ()
realMain ic = do
        iopts <- liftIO . execParser $ opts ic
        mcsrc <- readSafe BS.readFile Nothing $ configFile iopts
        options <- case mcsrc of Just csrc -> do
                                     c <- case eitherDecode csrc of Left e -> (printError . JSONDecodingError $ T.pack e) >> pure ic
                                                                    Right t -> pure t
                                     liftIO . execParser $ opts c
                                 Nothing -> pure iopts
        flip runConfigured options $ do
            config <- ask
            p <- case sourceDir config of Just d -> return d; Nothing -> throwError $ InvocationError "No directory specified"
            dir <- liftIO $ canonicalizePath p
            pdirs <- liftIO . mapM canonicalizePath $ pastDirs config
            let concatenate = if allVersions config then concatenateAll else concatenateActive
            progress "Concatenating submissions" $ do
                concatenate dir
                mapM_ concatenate pdirs
            progress "Highlighting concatenated files" $ do
                highlight dir
                mapM_ highlight pdirs
            (prints, past) <- progress "Fingerprinting submissions" $ do
                prints <- fingerprintDir (language config) (outputDir config </> concatDir config ++ dir)
                past <- concat <$> mapM (\x -> fingerprintDir (language config) (outputDir config </> concatDir config ++ x)) pdirs
                return (prints, past)
            (sprints, spast) <- progress "Detecting shared code" $ do
                let shared = findShared config (fst <$> prints) (fst <$> past)
                    sprints = (\(x, t) -> (Set.toList $ Set.difference (Set.fromList x) shared, t)) <$> prints
                    spast = (\(x, t) -> (Set.toList $ Set.difference (Set.fromList x) shared, t)) <$> past
                return (sprints, spast)
            progress "Generating plagiarism reports" $ report dir sprints spast
    where opts c = info (helper <*> parseOptions c)
                        (fullDesc <> progDesc "Run plagiarism detection" <> header "plagiarism - plagiarism detection")
