module Config ( Config(..)
              , defaultConfig ) where

data Config = Config { dbName    :: String
                     , port      :: Int
                     , nHashes   :: Int
                     , threshold :: Double }

defaultConfig :: Config
defaultConfig = Config { dbName    = "tastic_db"
                       , port      = 3000
                       , nHashes   = 256
                       , threshold = 0.0 }
