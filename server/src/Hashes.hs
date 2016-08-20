{-# LANGUAGE OverloadedStrings #-}

module Hashes (getMatches) where

import qualified Data.List as List
import Data.Int (Int32, Int64)

import Control.Monad.Except as Except
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text, unpack)

import Config
import qualified SQL

getMatches :: Text
           -> Text
           -> BS.ByteString 
           -> Except.ExceptT BS.ByteString IO ()
getMatches ast sub bytes =
  do chunked <- Except.ExceptT . return $ chunk bytes
     SQL.foldSubmissions ast sub (uncurry $ handleRow ast sub chunked)

maybeSplit :: Int64 -> BS.ByteString -> Maybe (Int32, BS.ByteString)
maybeSplit i str | BS.length x == i = Just (decode x, xs)
                 | otherwise        = Nothing
  where (x, xs) = BS.splitAt i str

chunk :: BS.ByteString -> Either BS.ByteString [Int32]
chunk str | len /= nHashes defaultConfig = Left message
          | otherwise = Right $ List.unfoldr (maybeSplit 4) str
  where message = "Incorrect number of hashes"
        len = fromIntegral (BS.length str) `quot` 4

jaccard :: [Int32] -> [Int32] -> Double
jaccard l1 l2 = let count = length . filter id $ zipWith (==) l1 l2
                 in fromIntegral count / fromIntegral (nHashes defaultConfig)

handleRow :: Text
          -> Text
          -> [Int32]
          -> Text
          -> BS.ByteString
          -> Except.ExceptT BS.ByteString IO ()
handleRow ast thisSub thisHashes otherSub bytes =
  do otherHashes <- Except.ExceptT . return $ chunk bytes
     let similarity = jaccard thisHashes otherHashes
     liftIO $ handleSimilarity similarity thisSub ast otherSub

handleSimilarity :: Double -> Text -> Text -> Text -> IO ()
handleSimilarity similarity thisSub ast sub
  | similarity >= threshold defaultConfig = putStrLn msg
  | otherwise                             = return () 
  where msg = unpack ast ++ "/" ++ unpack sub
           ++ " has similarity " ++ show (similarity * 100.0) ++ "% with "
           ++ unpack ast ++ "/" ++ unpack thisSub
           ++ "."
