module Data.Intl.DateTimeFormat.Class where

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prelude (Unit)

newtype FormatComponentRecord = FormatComponentRecord
  { era :: Maybe String
  , weekday :: Maybe String
  , year :: Maybe String
  , month :: Maybe String
  , day :: Maybe String
  , hour :: Maybe String
  , minute :: Maybe String
  , second :: Maybe String
  , timeZoneName :: Maybe String
  }

derive instance newtypeFormatComponentRecord :: Newtype FormatComponentRecord _

defaultComponentRecord :: FormatComponentRecord
defaultComponentRecord = FormatComponentRecord
  { era: Nothing
  , weekday: Nothing
  , year: Nothing
  , month: Nothing
  , day: Nothing
  , hour: Nothing
  , minute: Nothing
  , second: Nothing
  , timeZoneName: Nothing
  }


class FormatComponent a where
  formatComponent :: a -> FormatComponentRecord

instance unitFormatComponent :: FormatComponent Unit where
  formatComponent _ = defaultComponentRecord
