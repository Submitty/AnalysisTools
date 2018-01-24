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

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "config-file" <> short 'c' <> metavar "PATH" <> showDefault <> value (configFile dc) <> help "Configuration file")
               <*> strOption (long "output-dir" <> short 'o' <> metavar "DIR" <> showDefault <> value (outputDir dc) <> help "Directory to store generated output")
               <*> strOption (long "concat-dir" <> short 'c' <> metavar "DIR" <> showDefault <> value (concatDir dc) <> help "Subdirectory of output directory storing concatenated student code")
               <*> strOption (long "highlight-dir" <> short 'i' <> metavar "DIR" <> showDefault <> value (highlightDir dc) <> help "Subdirectory of output directory storing syntax-highlighted student code")
               <*> strOption (long "report-dir" <> short 'r' <> metavar "DIR" <> showDefault <> value (reportDir dc) <> help "Subdirectory of output directory storing the HTML report")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> option auto (long "top-matches" <> short 't' <> metavar "N" <> showDefault <> value (topMatches dc) <> help "Number of top matches to report")
               <*> option auto (long "semester" <> metavar "SEMESTER" <> value (semester dc) <> help "Semester within Submitty system")
               <*> option auto (long "course" <> metavar "COURSE" <> value (course dc) <> help "Course within Submitty system")
               <*> option auto (long "assignment" <> metavar "ASSIGNMENT" <> value (assignment dc) <> help "Assignment within Submitty system")

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
            --p <- case sourceDir config of Just d -> return d; Nothing -> throwError $ InvocationError "No directory specified"
            dir <- liftIO $ canonicalizePath undefined
            --let concatenate = if allVersions config then concatenateAll else concatenateActive
            let concatenate = concatenateActive
            progress "Concatenating submissions" $ concatenate dir
            progress "Highlighting concatenated files" $ highlight dir
            prints <- progress "Fingerprinting submissions" $ fingerprintDir (language config) (outputDir config </> concatDir config ++ dir)
            progress "Generating plagiarism reports" $ report dir prints
    where opts c = info (helper <*> parseOptions c)
                        (fullDesc <> progDesc "Run plagiarism detection" <> header "plagiarism - plagiarism detection")
