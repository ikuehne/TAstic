{-|
Module      : Config
Description : Configuration options compiled into the server
Copyright   : (c) Ian Kuehne, 2016
License     : GPL-3
Maintainer  : ikuehne@caltech.edu

Global configuration options that cannot be changed at compile time are kept
in this module.
-}
module Config ( Config(..)
              , config ) where

-- | The global configuration type.
data Config = Config { dbName    :: String
                     , port      :: Int
                     , nHashes   :: Int
                     , threshold :: Double }

-- | The default configuration.
defaultConfig :: Config
defaultConfig = Config { dbName    = "tastic_db"
                       , port      = 3000
                       , nHashes   = 256
                       , threshold = 0.0 }

-- | The configuration @tastic-server@ actually uses.
config :: Config
config = defaultConfig
