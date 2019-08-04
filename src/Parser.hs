module Parser (replaceTableNames, roughlyExtractTableNames, replaceBackTableNames, extractTableNames, errorString, TableNameMap) where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isNumber, isSpace, toUpper)
import Data.Generics (everything, mkQ)
import Data.List (unfoldr)
import qualified Data.Map as Map
import Data.Tuple (swap)
import qualified Language.SQL.SimpleSQL.Parse as Parse
import qualified Language.SQL.SimpleSQL.Syntax as Syntax

type TableNameMap = Map.Map String String

-- | Replace all the occurrence of table names (file names, in many cases) into
-- valid table names in SQL. The table names are calculated based on the file
-- names, keeping its length so that error position stays at the same position
-- after we replace them back to the file names. I take `max 5` to avoid conflict.
replaceTableNames :: String -> (String, TableNameMap)
replaceTableNames qs = (replaceQueryWithTableMap tableMap qs, tableMap)
  where genTableName xs = take (max 5 (length xs)) $ dropWhile isNumber $ concat $ tail $ iterate sha1Encode xs
        tableMap = Map.fromList [ (name, genTableName name) | name <- roughlyExtractTableNames qs ]

sha1Encode :: String -> String
sha1Encode = show . (Crypto.hash :: Char8.ByteString -> Crypto.Digest Crypto.SHA1) . Char8.pack

-- | This function roughly extract the table names. We need this function because
-- the given query contains the file names so the SQL parser cannot parse.
roughlyExtractTableNames :: String -> [String]
roughlyExtractTableNames qs = [ ys | (xs, ys) <- zip qss (drop 2 qss), isTableNamePrefix xs ]
  where qss = splitQuery qs

-- | The words after these words are possibly table names.
isTableNamePrefix :: String -> Bool
isTableNamePrefix xs = map toUpper xs `elem` ["FROM", "JOIN"]

-- | Replace the table names using the tableMap.
replaceQueryWithTableMap :: TableNameMap -> String -> String
replaceQueryWithTableMap tableMap qs = query
  where qss = splitQuery qs
        query = concat [ if isTableNamePrefix xs then Map.findWithDefault ys ys tableMap else ys | (xs, ys) <- zip ("" : "" : qss) qss ]

-- | Split the query string with spaces, taking the quotes into consideration.
splitQuery :: String -> [String]
splitQuery = unfoldr split'
  where split' :: String -> Maybe (String, String)
        split' ccs@(c:cs) | c `elem` "\"'`" = Just $ splitAt (1 + countUntil c cs) ccs
                          | isSpace c = Just $ span isSpace ccs
                          | otherwise = Just $ break (\d -> isSpace d || d `elem` "\"'`") ccs
        split' [] = Nothing
        countUntil c ('\\':c':cs) | c == c' = 2 + countUntil c cs
        countUntil c (c':cs) | c == c' = 1
                             | otherwise = 1 + countUntil c cs
        countUntil _ [] = 0

-- | Replace the generated table names back into the original file names.
replaceBackTableNames :: TableNameMap -> String -> String
replaceBackTableNames tableMap = replaceQueryWithTableMap reverseMap
  where reverseMap = Map.fromList $ map swap $ Map.toList tableMap

-- | Extracts the table names using the rigid SQL parser.
extractTableNames :: String -> FilePath -> Either Parse.ParseError [String]
extractTableNames query path = everything (++) ([] `mkQ` tableNames)
                            <$> Parse.parseQueryExpr Syntax.mysql path Nothing query
  where tableNames (Syntax.TRSimple (name:_)) = fromName name
        tableNames _ = []
        fromName (Syntax.Name _ name) = [name]

errorString :: Parse.ParseError -> String
errorString = Parse.peFormattedError
