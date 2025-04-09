module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_, guard, when, unless)
import Data.Char (isSpace)
import Data.List (isSuffixOf, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set as Set ((\\), fromList, union)
import Database.SQLite.Simple qualified as SQLite
import Options.Applicative (execParser, helper, info, fullDesc, header)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import File qualified
import Option
import Parser qualified
import SQL qualified
import SQLType qualified

main :: IO ()
main = runCommand =<< execParser opts
  where opts = info (helper <*> Option.version <*> Option.options)
                    (fullDesc <> header "qhs - SQL queries on CSV and TSV files")

runCommand :: Option -> IO ()
runCommand opts = do
  queryTableMap <- parseQuery =<< fetchQuery opts
  conn <- SQL.open ":memory:"
  runQuery opts conn queryTableMap
  SQL.close conn

runQuery :: Option -> SQLite.Connection -> (String, Parser.TableNameMap) -> IO ()
runQuery opts conn (query, tableMap) = do
  readFilesCreateTables opts conn tableMap
  ret <- SQL.execute conn query
  case ret of
       Right (cs, rs) -> do
         let outputDelimiter =
               fromMaybe " " $ guard opts.tabDelimitedOutput *> Just "\t"
                            <|> opts.outputDelimiter
                            <|> guard opts.tabDelimited *> Just "\t"
                            <|> opts.delimiter
         when opts.outputHeader $
           putStrLn $ intercalate outputDelimiter cs
         mapM_ (putStrLn . intercalate outputDelimiter . map show) rs
       Left err -> do
         hPutStrLn stderr err
         exitFailure

fetchQuery :: Option -> IO String
fetchQuery opts = do
  when (isJust opts.query && isJust opts.queryFile) $ do
    hPutStrLn stderr "Can't provide both a query file and a query on the command line."
    exitFailure
  query <- fromMaybe "" <$> case opts.query of
                                 Just q -> return (Just q)
                                 Nothing -> mapM readFile opts.queryFile
  when (all isSpace query) $ do
    hPutStrLn stderr "Query cannot be empty."
    hPutStrLn stderr "For basic information, try the `--help' option."
    exitFailure
  return query

parseQuery :: String -> IO (String, Parser.TableNameMap)
parseQuery qs = do
  let (query, tableMap) = Parser.replaceTableNames qs
  case Parser.extractTableNames query "<<query>>" of
       Left err -> do
         hPutStrLn stderr $ Parser.replaceBackTableNames tableMap $ Parser.errorString err
         exitFailure
       Right tableNames -> do
         let xs = Set.fromList tableNames
         let ys = Set.fromList (Map.elems tableMap)
         if xs == ys
            then return (query, tableMap)
            else do
              hPutStrLn stderr "Invalid table name:"
              hPutStrLn stderr $ "  " ++ show (Set.union (xs \\ ys) (ys \\ xs))
              hPutStrLn stderr "Probably a bug of qhs. Please submit a issue report."
              exitFailure

readFilesCreateTables :: Option -> SQLite.Connection -> Parser.TableNameMap -> IO ()
readFilesCreateTables opts conn tableMap =
  forM_ (Map.toList tableMap) \(path, name) -> do
    let path' = unquote path
    handle <- if path' == "-" then return stdin else openFile path' ReadMode
    let opts' = opts { gzipped = opts.gzipped || ".gz" `isSuffixOf` path' }
    (columns, body) <- File.readFromFile opts' handle
    when (null columns) $ do
      hPutStrLn stderr $ if opts.skipHeader
                            then "Header line is expected but missing in file " ++ path
                            else "Warning - data is empty"
      exitFailure
    when (any (elem ',') columns) $ do
      hPutStrLn stderr "Column name cannot contain commas"
      exitFailure
    unless (null columns) $
      createTable conn name path columns body
    hClose handle
  where unquote (x:xs@(_:_)) | x `elem` "\"'`" && x == last xs = init xs
        unquote xs = xs

createTable :: SQLite.Connection -> String -> String -> [String] -> [[String]] -> IO ()
createTable conn name path columns body = do
  let probablyNumberColumn =
        [ all isJust [ readMaybe x :: Maybe Float | x <- xs, not (all isSpace x) ]
                                                  | xs <- transpose body ]
  let types = [ if b then SQLType.SQLInt else SQLType.SQLChar | b <- probablyNumberColumn ]
  ret <- SQL.createTable conn name columns types
  case ret of
       Just err -> do
         putStrLn "Error on creating a new table:"
         putStrLn $ "  " ++ path ++ " (" ++ name ++ ") " ++ show columns
         putStrLn err
       Nothing ->
         forM_ body \entry -> do
           mapM_ (hPutStrLn stderr) =<< SQL.insertRow conn name columns types entry
