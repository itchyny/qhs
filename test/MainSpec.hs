module MainSpec (spec) where

import Control.Applicative
import Control.Monad
import System.IO
import System.Process
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = qhsSpec

qhsSpec :: Spec
qhsSpec =

  describe "qhs" $ do

    let tests = [
          "basic", "columns", "stdin", "stdin_hyphen", "header", "where",
          "count", "is_null", "not_null", "output_header", "tab", "tab2",
          "spaces", "output_delimiter", "tab_delimited_output", "multiline",
          "query_file", "gzip", "gzip_stdin", "avg", "sum", "avg_sum", "seq",
          "group", "group_sum", "empty_query", "empty_query_file", "concat",
          "join", "invalid", "version"
          ]

    forM_ tests $ \test -> do

      it ("should be executed correctly: " ++ test) $ do
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
