module Option where

import Data.Version (showVersion)
import Options.Applicative

import Paths_qhs qualified as QHS

-- | Command options
data Option = Option { skipHeader :: Bool,
                       outputHeader :: Bool,
                       delimiter :: Maybe String,
                       tabDelimited :: Bool,
                       outputDelimiter :: Maybe String,
                       tabDelimitedOutput :: Bool,
                       keepLeadingWhiteSpace :: Bool,
                       gzipped :: Bool,
                       queryFile :: Maybe String,
                       query :: Maybe String }

-- | Option parser
options :: Parser Option
options = Option
  <$> switch (long "skip-header"
             <> short 'H'
             <> help "Skip the header row for row input and use it for column names instead.")
  <*> switch (long "output-header"
             <> short 'O'
             <> help "Output the header line.")
  <*> optional (strOption (long "delimiter"
                          <> short 'd'
                          <> metavar "DELIMITER"
                          <> help "Field delimiter. If not specified, automatically detected."))
  <*> switch (long "tab-delimited"
             <> short 't'
             <> help "Same as -d $'\\t'.")
  <*> optional (strOption (long "output-delimiter"
                          <> short 'D'
                          <> metavar "OUTPUT_DELIMITER"
                          <> help "Field delimiter for output. If not specified, the argument of -d DELIMITER is used."))
  <*> switch (long "tab-delimited-output"
             <> short 'T'
             <> help "Same as -D $'\\t'.")
  <*> switch (long "keep-leading-whitespace"
             <> short 'k'
             <> help "Keep leading whitespace in values. The leading whitespaces are stripped off by default.")
  <*> switch (long "gzipped"
             <> short 'z'
             <> help "Assuming the gzipped input.")
  <*> optional (strOption (long "query-filename"
                          <> short 'q'
                          <> metavar "QUERY_FILENAME"
                          <> help "Read query from the provided filename."))
  <*> optional (argument str (metavar "QUERY"))

-- | Parser for --version/-v
version :: Parser (a -> a)
version = abortOption (InfoMsg ("qhs " ++ showVersion QHS.version)) $
    long "version" <>
    short 'v' <>
    help "Show the version of the command." <>
    hidden
