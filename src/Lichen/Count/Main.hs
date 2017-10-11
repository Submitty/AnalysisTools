{-# LANGUAGE OverloadedStrings #-}

module Lichen.Count.Main where

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
import Lichen.Languages
import Lichen.Count.Config
import Lichen.Count.Counters

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> (counterChoice (counter dc) <$> optional (argument str (metavar "COUNTER")))
               <*> optional (argument str (metavar "ELEMENT"))
               <*> many (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain ic = do
        iopts <- liftIO . execParser $ opts ic
        mcsrc <- readSafe BS.readFile Nothing (dataDir iopts </> "config_count.json")
        options <- case mcsrc of Just csrc -> do
                                     c <- case eitherDecode csrc of Left e -> (printError . JSONDecodingError $ T.pack e) >> pure ic
                                                                    Right t -> pure t
                                     liftIO . execParser $ opts c
                                 Nothing -> pure iopts
        flip runConfigured options $ do
            config <- ask
            t <- case toCount config of Just t -> return t; Nothing -> throwError $ InvocationError "No countable element specified"
            ps <- liftIO . mapM canonicalizePath $ sourceFiles config
            counts <- lift $ mapM (runCounter (counter config) (language config) t) ps
            liftIO . print $ sum counts
    where opts c = info (helper <*> parseOptions c) (fullDesc <> progDesc "Count occurences of a specific language feature" <> header "count - feature counting")
