module SQLType (SQLType(..), showType, Any, fromColumnsAndEntries) where

import Data.String (IsString(..))
import Data.Text qualified as Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok
import Text.Read (readMaybe)

data SQLType = SQLChar | SQLInt

showType :: SQLType -> String
showType t =
  case t of
    SQLChar -> "CHAR"
    SQLInt -> "INTEGER"

data Any = AnyDouble Double | AnyInt Int | AnyString String | AnyNull
         deriving (Eq)

instance FromField Any where
  fromField (Field (SQLFloat d) _) = Ok . AnyDouble $ d
  fromField (Field (SQLInteger i) _) = Ok . AnyInt . fromIntegral $ i
  fromField (Field (SQLText s) _) = Ok . AnyString . Text.unpack $ s
  fromField (Field SQLNull _) = Ok AnyNull
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance IsString Any where
  fromString s =
    case readMaybe s of
         Just i -> AnyInt i
         Nothing -> case readMaybe s of
                         Just d -> AnyDouble d
                         Nothing -> if s == "" then AnyNull else AnyString s

instance Show Any where
  show (AnyDouble d) = show d
  show (AnyInt i) = show i
  show (AnyString s) = s
  show AnyNull = ""

fromColumnsAndEntries :: [String] -> [[String]] -> ([String], [[Any]])
fromColumnsAndEntries columns xs = (columns, map (map fromString) xs)
