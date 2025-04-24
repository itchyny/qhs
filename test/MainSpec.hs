module MainSpec (spec) where

import Control.Monad
import System.IO
import System.Process
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = qhsSpec

qhsSpec :: Spec
qhsSpec =

  describe "qhs" do

    let tests = [
          "basic", "columns", "stdin", "header", "where", "tab", "tab2",
          "count", "is_null", "not_null", "output_header", "spaces",
          "output_delimiter", "tab_delimited_output", "multiline",
          "pipe_delimited", "pipe_delimited_output", "delimiters",
          "query_file", "empty_query", "empty_query_file", "file_spaces",
          "gzip", "gzip_stdin", "avg", "sum", "avg_sum", "seq",
          "group", "group_sum", "concat", "join", "invalid"
          ]

    forM_ tests \test -> do

      it ("should be executed correctly: " ++ test) do
        let cp = (shell ("bash " ++ test ++ ".sh")) {
              cwd = Just "test/tests",
              std_out = CreatePipe,
              std_err = CreatePipe
            }
        (_, Just out, Just err, _) <- createProcess cp
        hSetBuffering out NoBuffering
        hSetBuffering err NoBuffering
        outExpected <- readFile $ "test/tests/" ++ test ++ ".out"
        liftA2 (++) (hGetContents out) (hGetContents err) `shouldReturn` outExpected
