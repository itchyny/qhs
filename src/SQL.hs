module SQL (open, close, createTable, insertRow, execute) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.List (intersperse)
import Data.String (fromString)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQLite

import SQLType

-- | Open a new database connection.
open :: String -> IO SQLite.Connection
open = SQLite.open

-- | Close a database connection.
close :: SQLite.Connection -> IO ()
close = SQLite.close

-- | Creates a new table.
createTable :: SQLite.Connection -> String -> [String] -> [SQLType] -> IO (Maybe String)
createTable conn name columns types = do
  let stmt = "CREATE TABLE " ++ sqlQuote name ++ " "
           ++ tupled (map quote (zip types columns)) ++ ";"
  e <- try $ SQLite.execute_ conn (fromString stmt)
  return $ either (Just . (show :: SomeException -> String)) (const Nothing) e
    where quote (t, c) = sqlQuote c ++ " " ++ showType t

-- | Inserts a row into a table.
insertRow :: SQLite.Connection -> String -> [String] -> [SQLType] -> [String] -> IO (Maybe String)
insertRow conn name columns types entry = do
  let stmt = "INSERT INTO " ++ sqlQuote name ++ tupled (map sqlQuote columns)
           ++ " VALUES " ++ tupled (map quote (zip types entry)) ++ ";"
  e <- try $ SQLite.execute_ conn (fromString stmt)
  return $ either (Just . (show :: SomeException -> String)) (const Nothing) e
    where quote (t, "") | isDigitType t = "NULL"
          quote (t, cs) | isDigitType t = cs
                        | otherwise = "'" ++ toSQLString cs ++ "'"
          isDigitType SQLInt = True
          isDigitType _ = False

toSQLString :: String -> String
toSQLString "" = ""
toSQLString ('\'':xs) = '\'':'\'':toSQLString xs
toSQLString (x:xs) = x : toSQLString xs

-- | Executes a SQL statement.
execute :: SQLite.Connection -> String -> IO (Either String ([String], [[Any]]))
execute conn query = do
  e <- try $ SQLite.query_ conn (fromString query)
  columns <- SQLite.withStatement conn (fromString query) \stmt -> do
    cnt <- toInteger <$> SQLite.columnCount stmt
    forM [0..cnt-1] \i ->
      Text.unpack <$> SQLite.columnName stmt (fromInteger i)
  return $ either (Left . (show :: SomeException -> String)) (Right . (,) columns) $ e

sqlQuote :: String -> String
sqlQuote xs = "`" ++ xs ++ "`"

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"
