module Data.Intl.DateTimeFormat.Types 
  ( StringRep(..)
  , NumericRep(..)
  , MonthRep(..)
  , TimeZoneNameRep(..)
  , FormatParts(..)
  , readFormatParts
  , ResolvedOptions(..)
  , WeekdayYearMonthDayHourMinuteSecond(..)
  , WeekdayYearMonthDay(..)
  , YearMonthDay(..)
  , YearMonth(..)
  , MonthDay(..)
  , HourMinuteSecond(..)
  , HourMinute(..)
  , Undefined
  , undefined
  ) where

import Data.Foreign (F, Foreign, ForeignError(..), fail, readString)
import Data.Foreign.Index ((!))
import Data.Generic (class Generic)
import Data.Intl.DateTimeFormat.Class (class FormatComponent)
import Data.Intl.DateTimeFormat.Generic (genericFormatComponent)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude (class Show, bind, pure, ($), (<>), (>>=))

foreign import data Undefined :: Type
foreign import undefined :: Undefined

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

newtype WeekdayYearMonthDayHourMinuteSecond = WeekdayYearMonthDayHourMinuteSecond
  { weekday :: StringRep
  , year :: NumericRep
  , month :: MonthRep
  , day :: NumericRep
  , hour :: NumericRep
  , minute :: NumericRep
  , second :: NumericRep
  }

derive instance genericWeekdayYearMonthDayHourMinuteSecond :: Generic WeekdayYearMonthDayHourMinuteSecond _

instance formatComponentWeekdayYearMonthDayHourMinuteSecond :: FormatComponent WeekdayYearMonthDayHourMinuteSecond where
  formatComponent = genericFormatComponent

newtype WeekdayYearMonthDay = WeekdayYearMonthDay
    { weekday :: StringRep
    , year :: NumericRep
    , month :: MonthRep
    , day :: NumericRep
    }

derive instance genericWeekdayYearMonthDay :: Generic WeekdayYearMonthDay _

instance formatComponentWeekdayYearMonthDay :: FormatComponent WeekdayYearMonthDay where
  formatComponent = genericFormatComponent

newtype YearMonthDay = YearMonthDay
  { year :: NumericRep
  , month :: MonthRep
  , day :: NumericRep
  }

derive instance genericYearMonthDay :: Generic YearMonthDay _

instance formatComponentYearMonthDay :: FormatComponent YearMonthDay where
  formatComponent = genericFormatComponent

newtype YearMonth = YearMonth
  { year :: NumericRep
  , month :: MonthRep
  }

derive instance genericYearMonth :: Generic YearMonth _

instance formatComponentYearMonth :: FormatComponent YearMonth where
  formatComponent = genericFormatComponent

newtype MonthDay = MonthDay
  { month :: MonthRep
  , day :: NumericRep
  }

derive instance genericMonthDay :: Generic MonthDay _

instance formatComponentMonthDay :: FormatComponent MonthDay where
  formatComponent = genericFormatComponent

newtype HourMinuteSecond = HourMinuteSecond
  { hour :: NumericRep
  , minute :: NumericRep
  , second :: NumericRep
  }

derive instance genericHourMinuteSecond :: Generic HourMinuteSecond _

instance formatComponentHourMinuteSecond :: FormatComponent HourMinuteSecond where
  formatComponent = genericFormatComponent

newtype HourMinute = HourMinute
  { hour :: NumericRep
  , minute :: NumericRep
  }

derive instance genericHourMinute :: Generic HourMinute _

instance formatComponentHourMinute :: FormatComponent HourMinute where
  formatComponent = genericFormatComponent
