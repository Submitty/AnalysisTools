{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Main where

import System.Directory
import System.FilePath

import Data.Aeson
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Util
import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Plagiarism.Report
import Lichen.Plagiarism.Walk

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> strOption (long "concat-dir" <> short 'c' <> metavar "DIR" <> showDefault <> value (concatDir dc) <> help "Subdirectory of data directory storing concatenated student code")
               <*> strOption (long "highlight-dir" <> short 'i' <> metavar "DIR" <> showDefault <> value (highlightDir dc) <> help "Subdirectory of data directory storing syntax-highlighted student code")
               <*> strOption (long "report-dir" <> short 'r' <> metavar "DIR" <> showDefault <> value (reportDir dc) <> help "Subdirectory of data directory storing the HTML report")
               <*> (T.pack <$> strOption (long "report-title" <> metavar "TITLE" <> showDefault <> value (T.unpack $ reportTitle dc) <> help "Title of pages in the HTML report"))
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> option auto (long "top-matches" <> short 't' <> metavar "N" <> showDefault <> value (topMatches dc) <> help "Number of top matches to report")
               <*> (generatePathChoice (pathGenerator dc) <$> (optional . strOption $ long "path-generator" <> metavar "GENERATOR" <> help "Path generation method for reports"))
               <*> strOption (long "semester" <> metavar "SEMESTER" <> value (submittySemester dc) <> help "Semester for Submitty path generation")
               <*> strOption (long "course" <> metavar "COURSE" <> value (submittyCourse dc) <> help "Course for Submitty path generation")
               <*> strOption (long "assignment" <> metavar "ASSIGNMENT" <> value (submittyAssignment dc) <> help "Assignment for Submitty path generation")
               <*> optional (argument str (metavar "SOURCE_DIR"))
               <*> many (argument str (metavar "PAST_DIRS"))

realMain :: Config -> IO ()
realMain ic = do
        iopts <- liftIO . execParser $ opts ic
        mcsrc <- readSafe BS.readFile Nothing (dataDir iopts </> "config_plagiarism.json")
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
            concatenate dir
            mapM_ concatenate pdirs
            highlight dir
            mapM_ highlight pdirs
            prints <- fingerprintDir (language config) (dataDir config </> concatDir config ++ dir)
            past <- concat <$> mapM (\x -> fingerprintDir (language config) (dataDir config </> concatDir config ++ x)) pdirs
            report dir prints past
    where opts c = info (helper <*> parseOptions c) (fullDesc <> progDesc "Run plagiarism detection" <> header "plagiarism - plagiarism detection")
