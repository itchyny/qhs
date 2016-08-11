module Main where

import Control.Applicative
import Control.Monad (forM_, guard, when)
import Data.Char (isSpace)
import Data.List (isSuffixOf, intercalate, transpose)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Database.SQLite as SQLite
import Options.Applicative (execParser, helper, info, fullDesc, header, (<>))
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import qualified File as File
import qualified Option as Option
import qualified Parser as Parser
import qualified SQL as SQL

main :: IO ()
main = runCommand =<< execParser opts
  where opts = info (helper <*> Option.version <*> Option.options)
                    (fullDesc <> header "qhs - SQL queries on CSV and TSV files")

runCommand :: Option.Option -> IO ()
runCommand opts = do
  conn <- SQL.open ":memory:"
  maybeQueryTableMap <- parseQuery =<< fetchQuery opts
  maybe (return ()) (runQuery opts conn) maybeQueryTableMap
  SQL.close conn

runQuery :: Option.Option -> SQLite.SQLiteHandle -> (String, Parser.TableNameMap) -> IO ()
runQuery opts conn (query, tableMap) = do
  readFilesCreateTables opts conn tableMap
  ret <- fmap head <$> SQL.execute conn query
  case ret of
      Right r -> do
        let outputDelimiter =
              fromMaybe " " $ guard (Option.tabDelimitedOutput opts) *> Just "\t"
                           <|> Option.outputDelimiter opts
                           <|> guard (Option.tabDelimited opts) *> Just "\t"
                           <|> Option.delimiter opts
        when (Option.outputHeader opts) $
          case listToMaybe r of
                Just xs -> putStrLn $ intercalate outputDelimiter $ map fst xs
                Nothing -> return ()
        mapM_ (mapM_ (putStrLn . intercalate outputDelimiter . map snd)) ret
      Left err -> putStrLn err

fetchQuery :: Option.Option -> IO String
fetchQuery opts = do
  when (isJust (Option.query opts) && isJust (Option.queryFile opts)) $ do
    hPutStrLn stderr "Can't provide both a query file and a query on the command line."
    exitFailure
  query <- fromMaybe "" <$> case Option.query opts of
                                 Just q -> return (Just q)
                                 Nothing -> mapM readFile (Option.queryFile opts)
  when (all isSpace query) $ do
    hPutStrLn stderr "Query cannot be empty."
    hPutStrLn stderr "For basic information, try the `--help' option."
    exitFailure
  return query

parseQuery :: String -> IO (Maybe (String, Parser.TableNameMap))
parseQuery qs = do
  let (query, tableMap) = Parser.replaceTableNames qs
  case Parser.extractTableNames query "<<query>>" of
       Left err -> do
         putStrLn $ Parser.replaceBackTableNames tableMap $ Parser.errorString err
         return Nothing
       Right tableNames -> do
         let xs = Set.fromList tableNames
         let ys = Set.fromList (Map.elems tableMap)
         if xs == ys
            then return $ Just (query, tableMap)
            else do
              putStrLn "Invalid table name:"
              putStrLn $ "  " ++ show (Set.union (xs \\ ys) (ys \\ xs))
              putStrLn "Probably a bug of qhs. Please submit a issue report."
              return Nothing

readFilesCreateTables :: Option.Option -> SQLite.SQLiteHandle -> Parser.TableNameMap -> IO ()
readFilesCreateTables opts conn tableMap =
  forM_ (Map.toList tableMap) $ \(path, name) -> do
    let path' = unquote path
    handle <- openFile (if path' == "-" then "/dev/stdin" else path') ReadMode
    let opts' = opts { Option.gzipped = Option.gzipped opts || ".gz" `isSuffixOf` path' }
    (columns, body) <- File.readFromFile opts' handle
    when (length columns == 0) $ do
      hPutStrLn stderr $ if Option.skipHeader opts
                            then "Header line is expected but missing in file " ++ path
                            else "Warning - data is empty"
      exitFailure
    when (any (elem ',') columns) $ do
      hPutStrLn stderr "Column name cannot contain commas"
      exitFailure
    when (length columns >= 1) $
      createTable conn name path columns body
    hClose handle
  where unquote (x:xs@(_:_)) | x `elem` "\"'`" && x == last xs = init xs
        unquote xs = xs

createTable :: SQLite.SQLiteHandle -> String -> String -> [String] -> [[String]] -> IO ()
createTable conn name path columns body = do
  let probablyNumberColumn =
        [ all isJust [ readMaybe x :: Maybe Float | x <- xs, not (all isSpace x) ]
                                                  | xs <- transpose body ]
  let types = [ if b then SQLite.SQLInt SQLite.NORMAL False False
                     else SQLite.SQLChar Nothing | b <- probablyNumberColumn ]
  ret <- SQL.createTable conn name columns types
  case ret of
       Just err -> do
         putStrLn "Error on creating a new table:"
         putStrLn $ "  " ++ path ++ " (" ++ name ++ ") " ++ show columns
         putStrLn err
       Nothing ->
         forM_ body $ \entry -> do
           mapM_ (hPutStrLn stderr) =<< SQL.insertRow conn name columns types entry
