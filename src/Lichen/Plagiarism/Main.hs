{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Main where

import System.Directory
import System.FilePath

import Data.Semigroup ((<>))
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Plagiarism.Report
import Lichen.Plagiarism.Walk

import Lichen.Plagiarism.Render.Compare
import Lichen.Lexer
import Lichen.Plagiarism.Winnow
import qualified Lichen.Lexer.C as C
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T.IO
import Data.List

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> strOption (long "concat-dir" <> short 'c' <> metavar "DIR" <> showDefault <> value (concatDir dc) <> help "Subdirectory of data directory storing concatenated student code")
               <*> strOption (long "highlight-dir" <> short 'i' <> metavar "DIR" <> showDefault <> value (highlightDir dc) <> help "Subdirectory of data directory storing syntax-highlighted student code")
               <*> strOption (long "report-dir" <> short 'r' <> metavar "DIR" <> showDefault <> value (reportDir dc) <> help "Subdirectory of data directory storing the HTML report")
               <*> (T.pack <$> strOption (long "report-title" <> metavar "TITLE" <> showDefault <> value (T.unpack $ reportTitle dc) <> help "Title of pages in the HTML report"))
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> optional (argument str (metavar "SOURCE"))

highlightSource :: FilePath -> Plagiarism [Colored]
highlightSource p = do
        src <- liftIO $ BS.readFile p
        tsrc <- liftIO (expandTabs <$> T.IO.readFile p)
        t <- lift $ C.lex p src
        let pt = processTokens (winnowConfig langC) t
        liftIO . print $ t
        liftIO $ putStrLn "======================================================== SEP =========================================================="
        liftIO . print . sortBy (\a b -> compare (tpos a) (tpos b)) $ pt
        liftIO . print . sort $ (convertPos tsrc . tpos <$> pt)
        liftIO . print . deoverlap . sort $ (convertPos tsrc . tpos <$> pt)
        return . splitInto tsrc . deoverlap . sort $ (convertPos tsrc . tpos <$> pt)

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            p <- case sourceDir config of Just d -> return d; Nothing -> throwError $ InvocationError "No directory specified"
            dir <- liftIO $ canonicalizePath p
            concatenate dir
            highlight dir
            prints <- fingerprintDir (language config) (dataDir config </> concatDir config ++ dir)
            report dir prints
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Run plagiarism detection" <> header "lichen-plagiarism - plagiarism detection")
