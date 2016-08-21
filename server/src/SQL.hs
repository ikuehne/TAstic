{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SQL
Description : Interfacing with SQLite
Copyright   : (c) Ian Kuehne, 2016
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
-}
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

------------------------------------------------------------------------------
-- SQL queries.
------------------------------------------------------------------------------

-- | Insert a row into the @minhashes@.
--
-- Contains three paramaters: the assignment, the submission, and the hashes.
insertRowCommand :: SQL.Query
insertRowCommand = "INSERT INTO minhashes VALUES (?, ?, ?)"

-- | Create the @minhashes@ table, or do nothing.
--
-- Contains no parameters.
createTableCommand :: SQL.Query
createTableCommand = "CREATE TABLE IF NOT EXISTS minhashes \
                                               \(assignment TEXT, \
                                               \ submission TEXT, \
                                               \ hashes BLOB)"

-- | Create an index on the @minhashes@ table for assignment/submission pairs.
createIndexCommand :: SQL.Query
createIndexCommand = "CREATE UNIQUE INDEX \
                         \IF NOT EXISTS ix_minhashes ON \
                         \minhashes (assignment, submission)"

-- | Get all other submissions and hashes for a given assignment.
--
-- Does not return the provided submission.  Takes two parameters: the
-- assignment and the submissions.
getAssignmentCommand :: SQL.Query
getAssignmentCommand = "SELECT submission, hashes FROM minhashes \
                                                 \WHERE assignment = ?\
                                                 \AND submission <> ?"

------------------------------------------------------------------------------
-- Performing operations on the @minhashes@ table.
------------------------------------------------------------------------------

-- | Create the @minhashes@ table.
createTable :: IO ()
createTable = SQL.withConnection (dbName config) $ \conn ->
    do SQL.execute_ conn createTableCommand
       SQL.execute_ conn createIndexCommand

-- | Insert a row into the @minhashes@ table.
insertHashes :: Text
             -> Text
             -> BS.ByteString
             -> Except.ExceptT BS.ByteString IO ()
insertHashes assignment submission bytes =
    withConnectionExcept $ \conn -> handleSQL $
      SQL.execute conn insertRowCommand (assignment,
                                         submission,
                                         SQL.SQLBlob $ BS.toStrict bytes)

-- | Perform an action on each row in an assignment
--
-- @foldSubmissions assignment submission handler@ applies @handler@ to each
-- row @(submission, hashes)@ in @assignment@ (apart from the provided
-- @submission@.
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

------------------------------------------------------------------------------
-- Wrappers over @sqlite-simple@ functionality.
------------------------------------------------------------------------------

-- | A version of @withConnection@ in the @ExceptT@ monad.
withConnectionExcept :: (SQL.Connection -> Except.ExceptT BS.ByteString IO ())
                     -> Except.ExceptT BS.ByteString IO ()
withConnectionExcept f = Except.ExceptT $
  SQL.withConnection (dbName config) (Except.runExceptT . f)

-- | Handle any errors in the IO monad by putting them in the @ExceptT@ monad.
handleSQL :: IO () -> Except.ExceptT BS.ByteString IO ()
handleSQL sql = let exc :: Except.ExceptT SQL.SQLError IO ()
                    exc = Except.ExceptT $ Exception.try sql
                 in Except.withExceptT sqlExceptionToMessage exc

-- | Translate a @sqlite-simple@ error to a string to put in the response.
sqlExceptionToMessage :: SQL.SQLError -> BS.ByteString
sqlExceptionToMessage e = 
  case SQL.sqlError e of
    SQL.ErrorConstraint -> "Resource already present"
    _                   -> "Internal database error"
