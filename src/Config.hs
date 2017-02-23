module Config where

data Config = Config
            { hashBound :: Int
            , signalThreshold :: Int
            , noiseThreshold :: Int
            }
