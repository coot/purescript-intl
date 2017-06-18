module Data.Intl.DateTimeFormat 
  ( LocalesOption
  , LocaleMatcher(..)
  , TimeZone(..)
  , FormatMatcher(..)
  , WeekdayYearMonthDayHourMinuteSecond(..)
  , WeekdayYearMonthDay(..)
  , YearMonthDay(..)
  , YearMonth(..)
  , MonthDay(..)
  , HourMinuteSecond(..)
  , HourMinute(..)
  , Undefined
  , undefined
  , DateTimeFormatOptions(..)
  , DateTimeFormatOptions'
  , DateTimeComponents(..)
  , DateTimeFormatter
  , createDateTimeFormatter
  , formatJSDate
  , module Data.Intl.DateTimeFormat.Types
  , supportedLocalesOf
  , formatToParts
  , resolvedOptions
  ) where

import Data.Intl.DateTimeFormat.Types

import Data.Either (Either(..))
import Data.Foreign (F, Foreign, isUndefined, readString)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Intl.DateTimeFormat.Generic (class FormatComponent, defaultComponentRecord, formatComponent, genericFormatComponent)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.StrMap (union)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Variant (Variant, default, on)
import Prelude (class Eq, class Show, Unit, bind, id, pure, show, (#), ($), (<$>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

foreign import data LocalesOption' :: Type
foreign import data DateTimeFormat :: Type

foreign import data Undefined :: Type
foreign import undefined :: Undefined

foreign import dateTimeFormatImpl :: LocalesOption' -> DateTimeFormatOptions_ -> DateTimeFormat

newtype DateTimeFormatOptions_ = DateTimeFormatOptions_
  { localeMatcher :: String
  , timeZone :: String
  , hour12 :: Boolean
  , formatMatcher :: String
  , weekday :: String
  , era :: String
  , year :: String
  , month :: String
  , day :: String
  , hour :: String
  , minutes :: String
  , second :: String
  , timeZoneName :: String
  }

type LocalesOption = Variant (locale :: String, locales :: Array String, undefined :: Undefined)

toLocale :: LocalesOption -> LocalesOption'
toLocale = unsafeCoerce $
  default undefined
    # on (SProxy :: SProxy "locale") id
    # on (SProxy :: SProxy "locales") id

data LocaleMatcher
  = LMLookup
  | LMBestFit

instance showLocaleMatcher :: Show LocaleMatcher where
  show LMLookup = "lookup"
  show LMBestFit = "best fit"

newtype TimeZone = TimeZone String

instance showTimeZone :: Show TimeZone where
  show (TimeZone tz) = tz

derive instance newtypeTimeZone :: Newtype TimeZone _

derive instance eqTimeZone :: Eq TimeZone

data FormatMatcher
  = FMBasic
  | FMBestFit

instance showFormatMatcher :: Show FormatMatcher where
  show FMBasic = "basic"
  show FMBestFit = "best fit"

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

type DateTimeComponents a =
  Variant
    ( weekdayYearMonthDayHourMinuteSecond :: WeekdayYearMonthDayHourMinuteSecond
    , weekdayYearMonthDay :: WeekdayYearMonthDay
    , yearMonthDay :: YearMonthDay
    , yearMonth :: YearMonth
    , monthDay :: MonthDay
    , hourMinuteSecond :: HourMinuteSecond
    , hourMinute :: HourMinute
    , custom :: a
    )

data DateTimeFormatOptions a = DateTimeFormatOptions
  { localeMatcher :: Maybe LocaleMatcher
  , timeZone :: Maybe TimeZone
  , hour12 :: Maybe Boolean
  , formatMatcher :: Maybe FormatMatcher
  }
  (DateTimeComponents a)

type DateTimeFormatOptions' = DateTimeFormatOptions Unit

formatDateTimeOptions :: forall a. (FormatComponent a) => DateTimeFormatOptions a -> DateTimeFormatOptions_
formatDateTimeOptions (DateTimeFormatOptions opts comps) = merge opts' (compsCoerce comps')
  where

  undefinedString :: String
  undefinedString = unsafeCoerce undefined

  opts' = { localeMatcher: maybe undefinedString show opts.localeMatcher
          , timeZone: maybe undefinedString show opts.timeZone
          , hour12: fromMaybe (unsafeCoerce undefined) opts.hour12
          , formatMatcher: maybe undefinedString show opts.formatMatcher
          }
  comps' = default (defaultComponentRecord)
    # on (SProxy :: SProxy "weekdayYearMonthDayHourMinuteSecond") formatComponent
    # on (SProxy :: SProxy "weekdayYearMonthDay") formatComponent
    # on (SProxy :: SProxy "yearMonthDay") formatComponent
    # on (SProxy :: SProxy "yearMonth") formatComponent
    # on (SProxy :: SProxy "monthDay") formatComponent
    # on (SProxy :: SProxy "hourMinuteSecond") formatComponent
    # on (SProxy :: SProxy "hourMinute") formatComponent
    # on (SProxy :: SProxy "custom") formatComponent
    $ comps

  toUndefined :: Maybe String -> String
  toUndefined m = fromMaybe (unsafeCoerce undefined) m

  compsCoerce
    :: { weekday :: Maybe String
       , era :: Maybe String
       , year :: Maybe String
       , month :: Maybe String
       , day :: Maybe String
       , hour :: Maybe String
       , minute :: Maybe String
       , second :: Maybe String
       , timeZoneName :: Maybe String
       }
    -> { weekday :: String
       , era :: String
       , year :: String
       , month :: String
       , day :: String
       , hour :: String
       , minute :: String
       , second :: String
       , timeZoneName :: String
       }
  compsCoerce { weekday, era, year, month, day, hour, minute, second, timeZoneName }
    = { weekday: toUndefined weekday
      , era: toUndefined era
      , year: toUndefined year
      , month: toUndefined month
      , day: toUndefined day
      , hour: toUndefined hour
      , minute: toUndefined minute
      , second: toUndefined second
      , timeZoneName: toUndefined timeZoneName
      }

  merge
    :: { localeMatcher :: String
       , timeZone :: String
       , hour12 :: Boolean
       , formatMatcher :: String
       }
    -> { weekday :: String
       , era :: String
       , year :: String
       , month :: String
       , day :: String
       , hour :: String
       , minute :: String
       , second :: String
       , timeZoneName :: String
       }
    -> DateTimeFormatOptions_
  merge a b = unsafeCoerce $ union (unsafeCoerce a) (unsafeCoerce b)

foreign import data DateTimeFormatter :: Type

foreign import createDateTimeFormatterImpl
  :: LocalesOption'
  -> DateTimeFormatOptions_
  -> DateTimeFormatter

createDateTimeFormatter
  :: forall a
   .(FormatComponent a)
  => LocalesOption
  -> DateTimeFormatOptions a
  -> DateTimeFormatter
createDateTimeFormatter ls opts = createDateTimeFormatterImpl (toLocale ls) (formatDateTimeOptions opts)

foreign import formatJSDate :: DateTimeFormatter -> JSDate -> String

foreign import supportedLocalesOfImpl :: Fn3 (String -> Either String (Array String)) (Array String -> Either String (Array String)) (Array String) (Either String (Array String))
 
foreign import formatToPartsImpl :: DateTimeFormatter -> JSDate -> Array Foreign

formatToParts :: DateTimeFormatter -> JSDate -> F (Array FormatParts)
formatToParts formatter date = sequence $ readFormatParts <$> formatToPartsImpl formatter date

supportedLocalesOf :: Array String -> Either String (Array String)
supportedLocalesOf locales = runFn3 supportedLocalesOfImpl Left Right locales

foreign import resolvedOptionsImpl :: DateTimeFormatter -> Foreign

resolvedOptions :: DateTimeFormatter -> F ResolvedOptions
resolvedOptions fmt = do
  locale <- obj ! "locale" >>= readString
  calendar <- obj ! "calendar" >>= readString
  numberingSystem <- obj ! "numberingSystem" >>= readString
  timeZone <- obj ! "timeZone" >>= readString
  era <- obj ! "era" >>= readMaybeString
  weekday <- obj ! "weekday" >>= readMaybeString
  year <- obj ! "year" >>= readMaybeString
  day <- obj ! "day" >>= readMaybeString
  hour <- obj ! "hour" >>= readMaybeString
  minute <- obj ! "minute" >>= readMaybeString
  second <- obj ! "second" >>= readMaybeString
  timeZoneName <- obj ! "timeZoneName" >>= readMaybeString

  pure $ ResolvedOptions { locale, calendar, numberingSystem, timeZone, era, weekday, year, day, hour, minute, second, timeZoneName }

  where
    obj = resolvedOptionsImpl fmt

    readMaybeString :: Foreign -> F (Maybe String)
    readMaybeString f =
      if isUndefined f
        then pure Nothing
        else Just <$> readString f
