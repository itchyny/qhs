module SQL (open, close, createTable, insertRow, execute) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.List (intercalate)
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
createTable :: SQLite.Connection -> String -> [String] -> [SQLType] -> IO (Either SomeException ())
createTable conn name columns types = do
  let stmt = "CREATE TABLE " ++ sqlQuote name ++ " "
           ++ tupled (zipWith quote types columns) ++ ";"
  try $ SQLite.execute_ conn (fromString stmt)
    where quote t c = sqlQuote c ++ " " ++ show t

-- | Inserts a row into a table.
insertRow :: SQLite.Connection -> String -> [String] -> [SQLType] -> [String] -> IO (Either SomeException ())
insertRow conn name columns types entry = do
  let stmt = "INSERT INTO " ++ sqlQuote name ++ tupled (map sqlQuote columns)
           ++ " VALUES " ++ tupled (replicate (length columns) "?") ++ ";"
  try $ SQLite.execute conn (fromString stmt) (zip types entry)

-- | Executes a SQL statement.
execute :: SQLite.Connection -> String -> IO (Either SomeException ([String], [[Any]]))
execute conn query = do
  e <- try $ SQLite.query_ conn (fromString query)
  columns <- SQLite.withStatement conn (fromString query) \stmt -> do
    cnt <- toInteger <$> SQLite.columnCount stmt
    forM [0..cnt-1] \i ->
      Text.unpack <$> SQLite.columnName stmt (fromInteger i)
  return $ (columns,) <$> e

sqlQuote :: String -> String
sqlQuote xs = "`" ++ xs ++ "`"

tupled :: [String] -> String
tupled xs = "(" ++ intercalate ", " xs ++ ")"
