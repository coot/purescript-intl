module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DateTime (Date, DateTime(..), Month(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Generic.Rep (class Generic)
import Data.Intl.DateTimeFormat (DateTimeFormatOptions(..), DateTimeFormatOptions', HourMinute(HourMinute), HourMinuteSecond(HourMinuteSecond), LocalesOption, MonthDay(..), MonthRep(..), NumericRep(Numeric, TwoDigit), StringRep(..), TimeZone(TimeZone), WeekdayYearMonthDay(..), WeekdayYearMonthDayHourMinuteSecond(WeekdayYearMonthDayHourMinuteSecond), YearMonth(..), YearMonthDay(..), createDateTimeFormatter, formatJSDate)
import Data.Intl.DateTimeFormat.Class (class FormatComponent, genericFormatComponent)
import Data.JSDate (JSDate, fromDateTime)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time (Time(..))
import Data.Variant (inj)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (ASSERT, assert)

type Tests = Eff (console :: CONSOLE, assert :: ASSERT) Unit

newtype EraYear = EraYear
  { era :: StringRep
  , year :: NumericRep
  }

derive instance genericEraYear :: Generic EraYear _

instance formatComponentEraYear :: FormatComponent EraYear where
  formatComponent = genericFormatComponent

newtype Era = Era StringRep

instance formatComponentEra :: FormatComponent Era where
  formatComponent (Era r) =
    { era: Just (show r)
    , weekday: Nothing
    , year: Nothing
    , month: Nothing
    , day: Nothing
    , hour: Nothing
    , minute: Nothing
    , second: Nothing
    , timeZoneName: Nothing
    }

main :: Tests
main = do
  let

    locales :: LocalesOption
    locales = inj (SProxy :: SProxy "locale") "en-GB"

    fmtHourMinute =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts = 
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just false
              , formatMatcher: Nothing
              }
              (inj (SProxy :: SProxy "hourMinute") (HourMinute {hour: TwoDigit, minute: TwoDigit}))
            )

    fmtHourMinuteSecond =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts = 
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just false
              , formatMatcher: Nothing
              }
              (inj (SProxy :: SProxy "hourMinuteSecond") (HourMinuteSecond {hour: Numeric, minute: Numeric, second: Numeric}))
            )
          
    fmtWeekdayYearMonthDayHourMinuteSecond =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts = 
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "weekdayYearMonthDayHourMinuteSecond")
                (WeekdayYearMonthDayHourMinuteSecond
                  { weekday: Short
                  , year: Numeric
                  , month: MonthLong
                  , day: Numeric
                  , hour: Numeric
                  , minute: Numeric
                  , second: Numeric
                  }
                )
              )
            )

    fmtWeekdayYearMonthDay =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "weekdayYearMonthDay")
                (WeekdayYearMonthDay
                  { weekday: Long
                  , year: TwoDigit
                  , month: MonthShort
                  , day: TwoDigit
                  }
                )
              )
            )

    fmtYearMonthDay =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "yearMonthDay")
                (YearMonthDay
                  { year: TwoDigit
                  , month: MonthNarrow
                  , day: TwoDigit
                  }
                )
              )
            )

    fmtYearMonth =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "yearMonth")
                (YearMonth
                  { year: Numeric
                  , month: MonthNumeric
                  }
                )
              )
            )

    fmtMonthDay =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions'
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "monthDay")
                (MonthDay
                  { month: MonthTwoDigit
                  , day: TwoDigit
                  }
                )
              )
            )

    fmtEraYear =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions EraYear
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "custom")
                (EraYear
                  { era: Long
                  , year: Numeric
                  }
                )
              )
            )

    fmtEra =
      createDateTimeFormatter locales opts
        where
          opts :: DateTimeFormatOptions Era
          opts =
            (DateTimeFormatOptions
              { localeMatcher: Nothing
              , timeZone: (Just (TimeZone "Europe/London"))
              , hour12: Just true
              , formatMatcher: Nothing
              }
              (inj
                (SProxy :: SProxy "custom")
                (Era Short)
              )
            )


    date :: Maybe Date
    date = do
      y <- toEnum 2018
      let m = January
      d <- toEnum 2
      pure $ canonicalDate y m d

    time :: Maybe Time
    time = do
      h <- toEnum 12
      m <- toEnum 0
      s <- toEnum 0
      ms <- toEnum 0
      pure $ Time h m s ms

    dt :: Maybe DateTime
    dt = do
      d <- date
      t <- time
      pure $ DateTime d t

    jsDate :: Maybe JSDate
    jsDate = fromDateTime <$> dt

  case jsDate of
    Nothing -> unsafeCrashWith "jsDate not parsed"
    Just d -> do
      log "fmtWeekdayYearMonthDayHourMinuteSecond"
      assert $ formatJSDate fmtWeekdayYearMonthDayHourMinuteSecond d == "Tue, 2 January 2018, 12:00:00 pm"
      log "fmtWeekdayYearMonthDay"
      assert $ formatJSDate fmtWeekdayYearMonthDay d == "Tuesday, 02 Jan 18"
      log "fmtYearMonthDay"
      assert $ formatJSDate fmtYearMonthDay d == "02 J 18"
      log "fmtYearMonth"
      assert $ formatJSDate fmtYearMonth d == "01/2018"
      log "fmtMonthDay"
      assert $ formatJSDate fmtMonthDay d == "02/01"
      log "fmtHourMinuteSecond"
      assert $ formatJSDate fmtHourMinuteSecond d == "12:00:00"
      log "fmtHourMinute"
      assert $ formatJSDate fmtHourMinute d == "12:00"
      log "fmtEraYear"
      assert $ formatJSDate fmtEraYear d == "2018 Anno Domini"
      log "fmtEra"
      assert $ formatJSDate fmtEra d == "2 1 2018 AD"
