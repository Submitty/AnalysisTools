{-# LANGUAGE OverloadedStrings #-}

module Lichen.CountNode.Main where

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import qualified Config.Dyre as Dyre

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.CountNode
import qualified Lichen.Parser as P

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> optional (argument str (metavar "NODE"))
               <*> optional (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            base <- liftIO $ getEnv "LICHEN_CWD"
            n <- case node config of Just t -> return t; Nothing -> throwError $ InvocationError "No node specified"
            relsrc <- case sourceFile config of Just s -> return s; Nothing -> throwError $ InvocationError "No source file specified"
            src <- liftIO $ canonicalizePath (base </> relsrc)
            dat <- liftIO $ BS.readFile src
            tree <- lift $ (parser $ language config) src dat
            liftIO $ print $ P.countTag (T.pack n) tree
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Count occurences of a specific AST node" <> header "lichen-count-node - token counting")

run :: Config -> IO ()
run = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "lichen-count-node"
    , Dyre.realMain = realMain
    , Dyre.statusOut = hPutStrLn stderr
    , Dyre.configDir = Just $ getEnv "LICHEN_CWD"
    , Dyre.cacheDir = Just $ (</>) <$> getEnv "LICHEN_CWD" <*> pure ".lichen/cache"
    }
