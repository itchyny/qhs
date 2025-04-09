module File where

import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Char (isSpace)
import System.Exit (exitFailure)
import System.IO

import Option qualified

readFromFile :: Option.Option -> Handle -> IO ([String], [[String]])
readFromFile opts handle = do
  contents <- joinMultiLines <$> lines <$>
    if Option.gzipped opts
       then Char8.unpack <$> GZip.decompress <$> ByteString.hGetContents handle
       else hGetContents handle
  let (headLine : secondLine : _) = contents ++ [ "", "" ]
  let delimiter = guard (Option.tabDelimited opts) *> Just "\t" <|> Option.delimiter opts
  when (maybe False ((/=1) . length) delimiter) $ do
    hPutStrLn stderr "Invalid delimiter."
    exitFailure
  let splitter = case delimiter of
                      Just [c] -> (==) c
                      _ -> detectSplitter headLine secondLine
  let headColumns = splitFixedSize splitter 0 headLine
  let size = length headColumns
  let columns = if Option.skipHeader opts then headColumns else [ 'c' : show i | i <- [1..size] ]
  let skipLine = if Option.skipHeader opts then tail else id
  let stripSpaces = if Option.keepLeadingWhiteSpace opts then id else dropWhile isSpace
  let body = filter (not . null) $ map (map stripSpaces . splitFixedSize splitter size) (skipLine contents)
  return (columns, body)
  where joinMultiLines (cs:ds:css) | valid True cs = cs : joinMultiLines (ds:css)
                                   | otherwise = joinMultiLines $ (cs ++ "\n" ++ ds) : css
          where valid False ('"':'"':xs) = valid False xs
                valid False ('\\':'"':xs) = valid False xs
                valid b ('"':xs) = valid (not b) xs
                valid b (_:xs) = valid b xs
                valid b "" = b
        joinMultiLines css = css

detectSplitter :: String -> String -> Char -> Bool
detectSplitter xs ys = head $ [ splitter | (x, y, splitter) <- map splitLines splitters
                                         , 1 < length x && length x <= length y ] ++ splitters
  where splitLines f = (splitFixedSize f 0 xs, splitFixedSize f 0 ys, f)
        splitters = [ (==','), isSpace ]

splitFixedSize :: (Char -> Bool) -> Int -> String -> [String]
splitFixedSize f n = fill . go n
  where go _ "" = []
        go k (c:cs@(c':_)) | f c && f c' && not (f ' ' && isSpace c') = "" : go (k - 1) cs
                           | f c = go k cs
        go k ('"':cs) = let (ys, xs) = takeQuotedString cs in xs : go (k - 1) ys
          where takeQuotedString ('"':'"':xs) = fmap ('"':) (takeQuotedString xs)
                takeQuotedString ('\\':'"':xs) = fmap ('"':) (takeQuotedString xs)
                takeQuotedString ('"':xs) = (xs, "")
                takeQuotedString (x:xs) = fmap (x:) (takeQuotedString xs)
                takeQuotedString "" = ("", "")
        go k (c:cs) | f c = go k cs
        go 1 cs = [cs]
        go k cs = let (xs, ys) = break f cs in xs : go (k - 1) ys
        fill [] = []
        fill xs = xs ++ replicate (n - length xs) ""
