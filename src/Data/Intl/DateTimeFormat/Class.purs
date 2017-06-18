module Data.Intl.DateTimeFormat.Class where

import Data.Maybe (Maybe(..))
import Prelude (Unit)

type FormatComponentRecord =
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

defaultComponentRecord :: FormatComponentRecord
defaultComponentRecord =
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
