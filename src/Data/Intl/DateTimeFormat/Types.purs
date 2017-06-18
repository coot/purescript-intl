module Data.Intl.DateTimeFormat.Types 
  ( StringRep(..)
  , NumericRep(..)
  , MonthRep(..)
  , TimeZoneNameRep(..)
  , FormatParts(..)
  , readFormatParts
  , ResolvedOptions(..)
  ) where

import Data.Foreign (F, Foreign, ForeignError(..), fail, readString)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude (class Show, bind, pure, ($), (<>), (>>=))

data StringRep = Narrow | Short | Long

instance showStringRep :: Show StringRep where
  show Narrow = "narrow"
  show Short = "short"
  show Long = "long"

data NumericRep = Numeric | TwoDigit

instance showNumericRep :: Show NumericRep where
  show Numeric = "numeric"
  show TwoDigit = "2-digit"

data MonthRep = MonthNumeric | MonthTwoDigit | MonthNarrow | MonthShort | MonthLong

instance showMonthRep :: Show MonthRep where
  show MonthNumeric = "numeric"
  show MonthTwoDigit = "2-digit"
  show MonthNarrow = "narrow"
  show MonthShort = "short"
  show MonthLong = "long"

data TimeZoneNameRep = TimeZoneNameShort | TimeZoneNameLong

instance showTimeZoneRep :: Show TimeZoneNameRep where
  show TimeZoneNameShort = "short"
  show TimeZoneNameLong = "long"

data FormatParts
  = Era String
  | Year String
  | Month String
  | Weekday String
  | Day String
  | Hour String
  | Minute String
  | DayPeriod String
  | Literal String

readFormatParts :: Foreign -> F FormatParts
readFormatParts val = do
  type_ <- val ! "type" >>= readString
  value <- val ! "value" >>= readString
  case type_ of
    "era" -> pure $ Era value
    "year" -> pure $ Year value
    "month" -> pure $ Month value
    "weekday" -> pure $ Weekday value
    "month" -> pure $ Month value
    "day" -> pure $ Day value
    "hour" -> pure $ Hour value
    "minute" -> pure $ Minute value
    "dayperiod" -> pure $ DayPeriod value
    "literal" -> pure $ Literal value
    _ -> fail $ ForeignError ("type " <> type_ <> " is not supported by purescript-intl")

newtype ResolvedOptions = ResolvedOptions
  { locale :: String
  , numberingSystem :: String
  , calendar :: String
  , timeZone :: String
  , era :: Maybe String
  , weekday :: Maybe String
  , year :: Maybe String
  , day :: Maybe String
  , hour :: Maybe String
  , minute :: Maybe String
  , second :: Maybe String
  , timeZoneName :: Maybe String
  }

derive instance newtypeResolvedOptions :: Newtype ResolvedOptions _
