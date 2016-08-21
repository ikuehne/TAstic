{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main ( main ) where

import qualified Data.Either as Either

import qualified Control.Monad.Except as Except
import Control.Monad.Trans (lift)
import Data.Binary (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Network.Wai as Wai
import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Status as Status

import Config
import Hashes
import SQL

------------------------------------------------------------------------------
-- Defining the server.
------------------------------------------------------------------------------

main :: IO ()
main = do
    let p = port config
    putStrLn $ "Listening on port " ++ show p
    createTable
    run p app

app :: Application
app req f | method == Method.methodPost = handlePost req >>= f
          | otherwise                   = f invalidRequestType
  where method = Wai.requestMethod req
        invalidRequestType = badRequest "Invalid request type"

handlePost :: Wai.Request -> IO Wai.Response
handlePost req = handleErrors $
  do (ast, sub) <- Except.ExceptT . return $ extractPath req
     body       <- lift $ Wai.strictRequestBody req
     insertHashes ast sub body
     getMatches   ast sub body
     return $ responseLBS Status.status201 [] "Created resource"


------------------------------------------------------------------------------
-- Handline errors.
------------------------------------------------------------------------------

handleErrors :: Except.ExceptT BS.ByteString IO Wai.Response
             -> IO Wai.Response
handleErrors exc = Either.either badRequest id <$> Except.runExceptT exc

badRequest :: BS.ByteString -> Wai.Response
badRequest str = responseLBS Status.badRequest400
                             [("Content-Length", bodyLen)]
                             str
  where bodyLen :: ByteString
        bodyLen = BS.toStrict . encode . show $ BS.length str


------------------------------------------------------------------------------
-- Utilities.
------------------------------------------------------------------------------

extractPath :: Wai.Request -> Either BS.ByteString (Text, Text)
extractPath req = case Wai.pathInfo req of
  [ast, sub] -> Right (ast, sub)
  _          -> Left "Invalid URI: must be assignment/submission"
