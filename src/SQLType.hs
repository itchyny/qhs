module SQLType (SQLType(..), Any, fromColumnsAndEntries) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text qualified as Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Text.Read (readMaybe)

data SQLType = SQLChar | SQLInt
             deriving Eq

instance Show SQLType where
  show SQLChar = "CHAR"
  show SQLInt  = "INTEGER"

instance ToField (SQLType, String) where
  toField (SQLChar, cs) = SQLText $ Text.pack cs
  toField (SQLInt, cs)  = maybe SQLNull SQLFloat $ readMaybe cs

data Any = AnyDouble Double | AnyInt Int | AnyString String | AnyNull
         deriving Eq

instance FromField Any where
  fromField (Field (SQLFloat d) _) = Ok . AnyDouble $ d
  fromField (Field (SQLInteger i) _) = Ok . AnyInt . fromIntegral $ i
  fromField (Field (SQLText s) _) = Ok . AnyString . Text.unpack $ s
  fromField (Field SQLNull _) = Ok AnyNull
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance IsString Any where
  fromString "" = AnyNull
  fromString s = fromMaybe (AnyString s) $
                   AnyInt <$> readMaybe s <|>
                   AnyDouble <$> readMaybe s

instance Show Any where
  show (AnyDouble d) = show d
  show (AnyInt i)    = show i
  show (AnyString s) = s
  show AnyNull       = ""

fromColumnsAndEntries :: [String] -> [[String]] -> ([String], [[Any]])
fromColumnsAndEntries columns xs = (columns, map (map fromString) xs)
