module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
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
import Test.Assert (ASSERT, assert')

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
      let fmtDate1 = formatJSDate fmtWeekdayYearMonthDayHourMinuteSecond d
          fmtDate1Expected = "Tue, 2 January 2018, 12:00:00 pm"
      assert' ("fmtWeekdayYearMonthDayHourMinuteSecond: got: '" <> fmtDate1 <> "' expected: '" <> fmtDate1Expected <> "'")
        $ fmtDate1 == fmtDate1Expected
      let fmtDate2 = formatJSDate fmtWeekdayYearMonthDay d
          fmtDate2Expected = "Tuesday, 02 Jan 18"
      assert' ("fmtWeekdayYearMonthDayString: got '" <> fmtDate2 <> "' expected: '" <> fmtDate2Expected <> "'")
        $ fmtDate2 == fmtDate2Expected
      let fmtDate3 = formatJSDate fmtYearMonthDay d
          fmtDate3Expected = "02 J 18"
      assert' ("fmtYearMonthDayString: got '" <> fmtDate3 <> "' expected: '" <> fmtDate3Expected <> "'")
        $ fmtDate3 == fmtDate3Expected
      let fmtDate4 = formatJSDate fmtYearMonth d
          fmtDate4Expected = "01/2018"
      assert' ("fmtYearMonthString: got '" <> fmtDate4 <> "' expected: '" <> fmtDate4Expected <> "'")
        $ fmtDate4 == fmtDate4Expected
      let fmtDate5 = formatJSDate fmtMonthDay d
          fmtDate5Expected = "02/01"
      assert' ("fmtMonthDayString: got '" <> fmtDate5 <> "' expected: '" <> fmtDate5Expected <> "'")
        $ fmtDate5 == fmtDate5Expected
      let fmtDate6 = formatJSDate fmtHourMinuteSecond d
          fmtDate6Expected = "12:00:00"
      assert' ("fmtHourMinuteSecondString: got '" <> fmtDate6 <> "' expected: '" <> fmtDate6Expected <> "'")
        $ fmtDate6 == fmtDate6Expected
      let fmtDate7 = formatJSDate fmtHourMinute d
          fmtDate7Expected = "12:00"
      assert' ("fmtHourMinuteString: got '" <> fmtDate7 <> "' expected: '" <> fmtDate7Expected <> "'")
        $ fmtDate7 == fmtDate7Expected
      let fmtDate8 = formatJSDate fmtEraYear d
          fmtDate8Expected = "2018 Anno Domini"
      assert' ("fmtEraYear: got '" <> fmtDate8 <> "' expected: '" <> fmtDate8Expected <> "'")
        $ fmtDate8 == fmtDate8Expected
      let fmtDate9 = formatJSDate fmtEra d
          fmtDate9Expected = "2 1 2018 AD"
      assert' ("fmtEra: got '" <> fmtDate9 <> "' expected: '" <> fmtDate9Expected <> "'")
        $ fmtDate9 == fmtDate9Expected
