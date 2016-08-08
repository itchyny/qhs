module Parser (replaceTableNames, roughlyExtractTableNames, replaceBackTableNames, extractTableNames, errorString, TableNameMap) where

import Control.Monad (mfilter)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isNumber, isSpace, toUpper)
import Data.Generics (everything, mkQ)
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
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
        withAlias xs = let char = maybe ('t':) (const id) $ mfilter isAlpha' $ listToMaybe $ filter isAlphaNum' xs
                           in (xs, take (max 3 (length xs)) $ char $ filter isAlphaNum' (xs ++ sha1Encode xs) ++ repeat '_')
        tableMap = Map.fromList $ map withAlias tableNames

sha1Encode :: String -> String
sha1Encode = Char8.unpack . Builder.toLazyByteString . Builder.byteStringHex . SHA1.hashlazy . Char8.pack

isAlpha' :: Char -> Bool
isAlpha' c = let c' = toUpper c in 'A' <= c' && c' <= 'Z'

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isNumber c || isAlpha' c

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
