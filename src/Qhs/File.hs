module Qhs.File (readFromFile, detectSplitter, splitFixedSize) where

import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)), head, prependList, tail, toList,
                           uncons, (<|))
import Data.Tuple.Extra (second)
import System.Exit (exitFailure)
import System.IO
import Prelude hiding (head, tail)

import Qhs.Option

readFromFile :: Option -> Handle -> IO ([String], [[String]])
readFromFile opts handle = do
  contents <- joinMultiLines . lines <$>
    if opts.gzipped
       then Char8.unpack . GZip.decompress <$> ByteString.hGetContents handle
       else hGetContents handle
  let delimiter = guard opts.tabDelimited *> Just "\t" <|>
                  guard opts.pipeDelimited *> Just "|" <|>
                  opts.delimiter
  when (maybe False ((/=1) . length) delimiter) do
    hPutStrLn stderr "Invalid delimiter."
    exitFailure
  let splitter = case delimiter of
                      Just [c] -> (==) c
                      _ -> detectSplitter headLine secondLine
                        where (headLine, secondLine) = second (maybe "" head) (uncons contents)
  let headColumns = splitFixedSize splitter 0 $ head contents
  let size = length headColumns
  let columns = if opts.skipHeader then headColumns else [ 'c' : show i | i <- [1..size] ]
  let skipLine = if opts.skipHeader then tail else toList
  let stripSpaces = if opts.keepLeadingWhiteSpace then id else dropWhile isSpace
  let body = filter (not . null) $ map (map stripSpaces . splitFixedSize splitter size) (skipLine contents)
  return (columns, body)
  where joinMultiLines (cs:ds:css) | valid True cs = cs <| joinMultiLines (ds:css)
                                   | otherwise = joinMultiLines $ (cs ++ "\n" ++ ds) : css
          where valid False ('"':'"':xs)  = valid False xs
                valid False ('\\':'"':xs) = valid False xs
                valid b ('"':xs)          = valid (not b) xs
                valid b (_:xs)            = valid b xs
                valid b ""                = b
        joinMultiLines (cs:css) = cs :| css
        joinMultiLines [] = "" :| []

detectSplitter :: String -> String -> Char -> Bool
detectSplitter xs ys = head $ [ s | (x, y, s) <- map splitLines $ toList splitters
                                  , 1 < length x && length x <= length y ] `prependList` splitters
  where splitLines f = (splitFixedSize f 0 xs, splitFixedSize f 0 ys, f)
        splitters = (==',') :| [isSpace]

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
