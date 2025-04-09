module SQLSpec (spec) where

import Control.Monad
import Test.Hspec (Spec, describe, it, shouldBe)

import SQL
import SQLType

spec :: Spec
spec = do
  openCloseSpec
  tableSpec

openCloseSpec :: Spec
openCloseSpec =
  describe "open, close" $ do

    it "should not throw exception" $ do
      conn <- SQL.open ":memory:"
      SQL.close conn

tableSpec :: Spec
tableSpec =
  describe "createTable, insertRow, execute" $ do

    it "should create a table" $ do
      conn <- SQL.open ":memory:"
      let columns = ["foo", "bar", "baz", "qux"]
      let types = repeat SQLChar
      _ <- SQL.createTable conn "test_table" columns types
      let entries = [ ["c0", "c1", "c2", "c3"],
                      ["d0", "d1", "d2", "d3"],
                      ["f0", "f1", "f2", "f3"] ]
      forM_ entries $ SQL.insertRow conn "test_table" columns types
      ret <- SQL.execute conn "SELECT * FROM test_table"
      ret `shouldBe` Right (fromColumnsAndEntries columns entries)
      SQL.close conn

    it "should take care of null values in a number column" $ do
      conn <- SQL.open ":memory:"
      let columns = ["foo", "num", "bar", "baz"]
      let types = cycle [SQLChar, SQLInt]
      _ <- SQL.createTable conn "test_table" columns types
      let entries = [ ["c0", "2", "c2", "3"],
                      ["d0", "", "d2", "2"],
                      ["f0", "3", "f2", ""],
                      ["e0", "", "e2", "4.3"] ]
      forM_ entries $ SQL.insertRow conn "test_table" columns types
      ret0 <- SQL.execute conn "SELECT * FROM test_table"
      ret0 `shouldBe` Right (fromColumnsAndEntries columns entries)
      ret1 <- SQL.execute conn "SELECT * FROM test_table WHERE num IS NOT NULL"
      ret1 `shouldBe` Right (fromColumnsAndEntries columns [e | e <- entries, e !! 1 /= ""])
      ret2 <- SQL.execute conn "SELECT avg(num) FROM test_table"
      ret2 `shouldBe` Right (fromColumnsAndEntries ["avg(num)"] [["2.5"]])
      ret3 <- SQL.execute conn "SELECT avg(baz) FROM test_table"
      ret3 `shouldBe` Right (fromColumnsAndEntries ["avg(baz)"] [["3.1"]])
      SQL.close conn

    it "can create a table when name and column names contain spaces" $ do
      conn <- SQL.open ":memory:"
      let columns = ["foo bar", "baz qux"]
      let types = repeat SQLChar
      _ <- SQL.createTable conn "test table" columns types
      let entries = [ ["c0 c1", "c2 c3"],
                      ["d0 d1", "d2 d3"],
                      ["f0 f1", "f2 f3"] ]
      forM_ entries $ SQL.insertRow conn "test table" columns types
      ret0 <- SQL.execute conn "SELECT * FROM `test table`"
      ret0 `shouldBe` Right (fromColumnsAndEntries columns entries)
      ret1 <- SQL.execute conn "SELECT `foo bar` FROM `test table`"
      ret1 `shouldBe` Right (fromColumnsAndEntries ["foo bar"] (map (take 1) entries))
      SQL.close conn
