module ParserSpec (spec) where

import Data.Either (isLeft)
import Data.Either.Extra (fromRight')
import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Parser

spec :: Spec
spec = do
  replaceTableNamesSpec
  roughlyExtractTableNamesSpec
  replaceBackTableNamesSpec
  extractTableNamesSpec

replaceTableNamesSpec :: Spec
replaceTableNamesSpec =
  describe "replaceTableNames" $ do

    it "should replace the file names with table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 > 0"
      let expected = ("SELECT * FROM d8dc7ec0 WHERE c0 > 0",
                      Map.fromList [("./table0", "d8dc7ec0")])
      replaceTableNames query `shouldBe` expected

    it "should replace multiple file names with table names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let expected = ("SELECT * FROM d0bf242ceb32ba2b\nJOIN bb239869bdfc764\tON c1 = c2 WHERE c0 > 0",
                      Map.fromList [("./src/table0.csv", "d0bf242ceb32ba2b"),("/tmp/table1.csv", "bb239869bdfc764")])
      replaceTableNames query `shouldBe` expected

    it "should replace only the table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 LIKE '%foo ./table0 bar%'"
      let expected = ("SELECT * FROM d8dc7ec0 WHERE c0 LIKE '%foo ./table0 bar%'",
                      Map.fromList [("./table0", "d8dc7ec0")])
      replaceTableNames query `shouldBe` expected

    it "should take care of multi-byte file names correctly" $ do
      let query = "SELECT * FROM ./テスト"
      let expected = ("SELECT * FROM e4c1e",
                      Map.fromList [("./テスト", "e4c1e")])
      replaceTableNames query `shouldBe` expected

    it "should take care of file name whose sha1 hash has only numbers" $ do
      let query = "SELECT * FROM vdbdc.csv"
      let expected = ("SELECT * FROM f8ce01ceb",
                      Map.fromList [("vdbdc.csv", "f8ce01ceb")])
      replaceTableNames query `shouldBe` expected

    it "should not replace inside single quotes" $ do
      let query = "SELECT * FROM table0.csv WHERE c0 LIKE '%foo FROM table1.csv bar%'"
      let expected = ("SELECT * FROM ec0f6d989c WHERE c0 LIKE '%foo FROM table1.csv bar%'",
                      Map.fromList [("table0.csv", "ec0f6d989c")])
      replaceTableNames query `shouldBe` expected

    it "should not replace inside double quotes" $ do
      let query = "SELECT * FROM table0.csv WHERE c0 LIKE \"%foo FROM table0.csv bar%\""
      let expected = ("SELECT * FROM ec0f6d989c WHERE c0 LIKE \"%foo FROM table0.csv bar%\"",
                      Map.fromList [("table0.csv", "ec0f6d989c")])
      replaceTableNames query `shouldBe` expected

    it "should replace the file name containing spaces" $ do
      let query = "SELECT * FROM `foo/bar baz qux/quux.csv`"
      let expected = ("SELECT * FROM a3ecf94bae7d224b52e9ad3df3",
                      Map.fromList [("`foo/bar baz qux/quux.csv`", "a3ecf94bae7d224b52e9ad3df3")])
      replaceTableNames query `shouldBe` expected

roughlyExtractTableNamesSpec :: Spec
roughlyExtractTableNamesSpec =
  describe "roughlyExtractTableNames" $ do

    it "should roughly extract table name" $ do
      roughlyExtractTableNames "SELECT * FROM table0 WHERE c0 > 0" `shouldBe` [ "table0" ]
      roughlyExtractTableNames "select * from table0 where c0 > 0" `shouldBe` [ "table0" ]

    it "should roughly extract multiple table names" $ do
      roughlyExtractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c0 > 0" `shouldBe` [ "table0", "table1" ]
      roughlyExtractTableNames "select * from table0 join table1 on c1 = c2 where c0 > 0" `shouldBe` [ "table0", "table1" ]

    it "should roughly extract table names but ignore inside quotes" $ do
      roughlyExtractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c3 LIKE 'A FROM table3 '" `shouldBe` [ "table0", "table1" ]
      roughlyExtractTableNames "select * from table0 join table1 on c1 = c2 where c3 like 'a from table3 '" `shouldBe` [ "table0", "table1" ]

    it "should roughly extract quoted table names" $ do
      roughlyExtractTableNames "SELECT * FROM `src/table 0 .csv` JOIN '/tmp/table 1.csv'" `shouldBe` [ "`src/table 0 .csv`", "'/tmp/table 1.csv'" ]

replaceBackTableNamesSpec :: Spec
replaceBackTableNamesSpec =
  describe "replaceBackTableNames" $ do

    it "should replace back table names to the file names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back multiple table names with the file names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back only the table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 LIKE '%foo d8dc7ec0 bar%'"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back the table name to the file name containing spaces" $ do
      let query = "SELECT * FROM `foo/bar baz qux/quux.csv`"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

extractTableNamesSpec :: Spec
extractTableNamesSpec =
  describe "extractTableNames" $ do

    it "should extract table name" $ do
      fromRight' (extractTableNames "SELECT * FROM table0 WHERE c0 > 0" "") `shouldBe` [ "table0" ]
      fromRight' (extractTableNames "select * from table0 where c0 > 0" "") `shouldBe` [ "table0" ]

    it "should extract multiple table names" $ do
      fromRight' (extractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c0 > 0" "") `shouldBe` [ "table0", "table1" ]
      fromRight' (extractTableNames "select * from table0 join table1 on c1 = c2 where c0 > 0" "") `shouldBe` [ "table0", "table1" ]

    it "should return a parse error" $ do
      isLeft (extractTableNames "SELECT ** FROM table0 WHERE c0 > 0" "") `shouldBe` True
      isLeft (extractTableNames "SELECT * FROM table0 WHERE > 0" "") `shouldBe` True
