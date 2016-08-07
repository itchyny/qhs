module Parser (replaceTableNames, roughlyExtractTableNames, replaceBackTableNames, extractTableNames, errorString, TableNameMap) where

import Data.Char (isSpace, isAlphaNum, ord, toUpper)
import qualified Data.Digest.SHA1 as SHA1
import Data.Generics (everything, mkQ)
import Data.List.Split
import qualified Data.Map as Map
import Data.Tuple (swap)
import qualified Language.SQL.SimpleSQL.Parser as Parser
import qualified Language.SQL.SimpleSQL.Syntax as Syntax

type TableNameMap = Map.Map String String

-- | Replace all the occurrence of table names (file names, in many cases) into
-- valid table names in SQL. The table names are calculated based on the file
-- names, keeping its length so that error position stays at the same position
-- after we replace them back to the file names. I take `max 3` to avoid conflict.
replaceTableNames :: String -> (String, TableNameMap)
replaceTableNames qs = (replaceQueryWithTableMap tableMap qs, tableMap)
  where tableNames = roughlyExtractTableNames qs
        sha1Encode = show . SHA1.hash . map (fromIntegral . ord)
        withAlias xs = (xs, take (max 3 (length xs)) $ 't' : filter isAlphaNum (xs ++ sha1Encode xs) ++ repeat '_')
        tableMap = Map.fromList $ map withAlias tableNames

-- | This function roughly extract the table names. We need this function because
-- the given query contains the file names so the SQL parser cannot parse.
roughlyExtractTableNames :: String -> [String]
roughlyExtractTableNames qs = let qss = words qs in [ ys | (xs, ys) <- zip qss (tail qss), isTableNamePrefix xs ]

-- | The words after these words are possibly table names.
isTableNamePrefix :: String -> Bool
isTableNamePrefix xs = map toUpper xs `elem` ["FROM", "JOIN"]

-- | Replace the table names using the tableMap.
replaceQueryWithTableMap :: TableNameMap -> String -> String
replaceQueryWithTableMap tableMap qs = query
  where qss = split (whenElt isSpace) qs
        query = concat [ if isTableNamePrefix xs then Map.findWithDefault ys ys tableMap else ys | (xs, ys) <- zip ("" : "" : qss) qss ]

-- | Replace the generated table names back into the original file names.
replaceBackTableNames :: TableNameMap -> String -> String
replaceBackTableNames tableMap = replaceQueryWithTableMap reverseMap
  where reverseMap = Map.fromList $ map swap $ Map.toList tableMap

-- | Extracts the table names using the rigid SQL parser.
extractTableNames :: String -> FilePath -> Either Parser.ParseError [String]
extractTableNames query path = everything (++) ([] `mkQ` tableNames)
                            <$> Parser.parseQueryExpr Syntax.MySQL path Nothing query
  where tableNames (Syntax.TRSimple (name:_)) = fromName name
        tableNames _ = []
        fromName (Syntax.Name name) = [name]
        fromName (Syntax.QName name) = [name]
        fromName (Syntax.UQName name) = [name]
        fromName _ = []

errorString :: Parser.ParseError -> String
errorString = Parser.peFormattedError
