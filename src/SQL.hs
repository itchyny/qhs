module SQL (open, close, createTable, insertRow, execute) where

import Data.List (intersperse)

import qualified Database.SQLite as SQLite

-- | Open a new database connection.
open :: String -> IO SQLite.SQLiteHandle
open = SQLite.openConnection

-- | Close a database connection.
close :: SQLite.SQLiteHandle -> IO ()
close = SQLite.closeConnection

-- | Creates a new table.
createTable :: SQLite.SQLiteHandle -> String -> [String] -> [SQLite.SQLType] -> IO (Maybe String)
createTable conn name columns types = do
  let stmt = "CREATE TABLE " ++ sqlQuote name ++ " "
           ++ tupled (map quote (zip types columns)) ++ ";"
  SQLite.execStatement_ conn stmt
    where quote (t, c) = sqlQuote c ++ " " ++ SQLite.showType t

-- | Inserts a row into a table.
insertRow :: SQLite.SQLiteHandle -> String -> [String] -> [SQLite.SQLType] -> [String] -> IO (Maybe String)
insertRow conn name columns types entry = do
  let stmt = "INSERT INTO " ++ sqlQuote name ++ tupled (map sqlQuote columns)
           ++ " VALUES " ++ tupled (map quote (zip types entry)) ++ ";"
  SQLite.execStatement_ conn stmt
    where quote (t, "") | isDigitType t = "NULL"
          quote (t, cs) | isDigitType t = cs
                        | otherwise = "'" ++ SQLite.toSQLString cs ++ "'"
          isDigitType (SQLite.SQLInt _ _ _) = True
          isDigitType (SQLite.SQLDecimal _ _) = True
          isDigitType (SQLite.SQLFloat _ _) = True
          isDigitType _ = False

-- | Executes a SQL statement.
execute :: SQLite.SQLiteResult a => SQLite.SQLiteHandle -> String -> IO (Either String [[SQLite.Row a]])
execute = SQLite.execStatement

sqlQuote :: String -> String
sqlQuote xs = "`" ++ xs ++ "`"

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"
