module ParserSpec (spec) where

import Data.Either
import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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
      let query = "SELECT * FROM table0 WHERE c0 > 0"
      let expected = ("SELECT * FROM ttable WHERE c0 > 0",
                      Map.fromList [("table0", "ttable")])
      replaceTableNames query `shouldBe` expected

    it "should replace multiple file names with table names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let expected = ("SELECT * FROM tsrctable0csvWor\nJOIN ttmptable1csvWo\tON c1 = c2 WHERE c0 > 0",
                      Map.fromList [("./src/table0.csv", "tsrctable0csvWor"),("/tmp/table1.csv", "ttmptable1csvWo")])
      replaceTableNames query `shouldBe` expected

    it "should replace only the table names" $ do
      let query = "SELECT * FROM stdin WHERE c0 LIKE '%foo stdin bar%'"
      let expected = ("SELECT * FROM tstdi WHERE c0 LIKE '%foo stdin bar%'",
                      Map.fromList [("stdin", "tstdi")])
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

replaceBackTableNamesSpec :: Spec
replaceBackTableNamesSpec =
  describe "replaceBackTableNames" $ do

    it "should replace back table names to the file names" $ do
      let query = "SELECT * FROM table0 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back multiple table names with the file names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back only the table names" $ do
      let query = "SELECT * FROM stdin WHERE c0 LIKE '%foo tstdi bar%'"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

extractTableNamesSpec :: Spec
extractTableNamesSpec =
  describe "extractTableNames" $ do

    it "should extract table name" $ do
      extractTableNames "SELECT * FROM table0 WHERE c0 > 0" "" `shouldBe` Right [ "table0" ]
      extractTableNames "select * from table0 where c0 > 0" "" `shouldBe` Right [ "table0" ]

    it "should extract multiple table names" $ do
      extractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c0 > 0" "" `shouldBe` Right [ "table0", "table1" ]
      extractTableNames "select * from table0 join table1 on c1 = c2 where c0 > 0" "" `shouldBe` Right [ "table0", "table1" ]

    it "should return a parse error" $ do
      extractTableNames "SELECT ** FROM table0 WHERE c0 > 0" "" `shouldSatisfy` isLeft
      extractTableNames "SELECT * FROM table0 WHERE > 0" "" `shouldSatisfy` isLeft
