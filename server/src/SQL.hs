{-# LANGUAGE OverloadedStrings #-}

module SQL ( insertHashes
           , createTable 
           , foldSubmissions ) where

import qualified Control.Exception.Base as Exception

import qualified Control.Monad.Except as Except
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.ToField ()

import Config

type Assignment = Text
type Submission = Text

insertRowCommand :: SQL.Query
insertRowCommand = "INSERT INTO minhashes VALUES (?, ?, ?)"

createTableCommand :: SQL.Query
createTableCommand = "CREATE TABLE IF NOT EXISTS minhashes \
                                               \(assignment TEXT, \
                                               \ submission TEXT, \
                                               \ hashes BLOB)"

createIndexCommand :: SQL.Query
createIndexCommand = "CREATE UNIQUE INDEX \
                         \IF NOT EXISTS ix_minhashes ON \
                         \minhashes (assignment, submission)"

getAssignmentCommand :: SQL.Query
getAssignmentCommand = "SELECT submission, hashes FROM minhashes \
                                                 \WHERE assignment = ?\
                                                 \AND submission <> ?"

foldSubmissions :: Text
                -> Text
                -> ((Text, BS.ByteString)
                 -> Except.ExceptT BS.ByteString IO ())
                -> Except.ExceptT BS.ByteString IO ()
foldSubmissions assignment submission handleRow = withConnectionExcept $
    \conn ->
      let main = SQL.fold conn
                          getAssignmentCommand
                          (assignment, submission)
                          unit
                          (const $ Except.runExceptT . handleRow )
       in Except.ExceptT main
  where unit = Right ()

createTable :: IO ()
createTable = SQL.withConnection (dbName defaultConfig) $ \conn ->
    do SQL.execute_ conn createTableCommand
       SQL.execute_ conn createIndexCommand

withConnectionExcept :: (SQL.Connection -> Except.ExceptT BS.ByteString IO ())
                     -> Except.ExceptT BS.ByteString IO ()
withConnectionExcept f = Except.ExceptT $
  SQL.withConnection (dbName defaultConfig) (Except.runExceptT . f)

insertHashes :: Assignment
             -> Submission
             -> BS.ByteString
             -> Except.ExceptT BS.ByteString IO ()
insertHashes assignment submission bytes =
    withConnectionExcept $ \conn -> handleSQL $
      SQL.execute conn insertRowCommand (assignment,
                                         submission,
                                         SQL.SQLBlob $ BS.toStrict bytes)

handleSQL :: IO () -> Except.ExceptT BS.ByteString IO ()
handleSQL sql = let exc :: Except.ExceptT SQL.SQLError IO ()
                    exc = Except.ExceptT $ Exception.try sql
                 in Except.withExceptT sqlExceptionToMessage exc

sqlExceptionToMessage :: SQL.SQLError -> BS.ByteString
sqlExceptionToMessage e = 
  case SQL.sqlError e of
    SQL.ErrorConstraint -> "Resource already present"
    _                   -> "Internal database error"
