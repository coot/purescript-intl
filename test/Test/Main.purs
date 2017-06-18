module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.DateTime (Date, DateTime(..), Month(..), canonicalDate)
import Data.Either (Either(..), fromRight, isLeft)
import Data.Enum (toEnum)
import Data.Foldable (intercalate)
import Data.Foreign (renderForeignError)
import Data.Generic.Rep (class Generic)
import Data.Intl.DateTimeFormat (DateTimeFormatOptions(DateTimeFormatOptions), DateTimeFormatOptions', HourMinute(HourMinute), HourMinuteSecond(HourMinuteSecond), LocalesOption, MonthDay(MonthDay), MonthRep(MonthTwoDigit, MonthNumeric, MonthNarrow, MonthShort, MonthLong), NumericRep(Numeric, TwoDigit), ResolvedOptions(..), StringRep(Short, Long), TimeZone(TimeZone), WeekdayYearMonthDay(WeekdayYearMonthDay), WeekdayYearMonthDayHourMinuteSecond(WeekdayYearMonthDayHourMinuteSecond), YearMonth(YearMonth), YearMonthDay(YearMonthDay), createDateTimeFormatter, formatJSDate, resolvedOptions, supportedLocalesOf)
import Data.Intl.DateTimeFormat.Generic (class FormatComponent, genericFormatComponent)
import Data.JSDate (JSDate, fromDateTime)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Symbol (SProxy(..))
import Data.Time (Time(..))
import Data.Variant (inj)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
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
    locales = inj (SProxy :: SProxy "locale") "en-US"

    fmtError =
      createDateTimeFormatter (inj (SProxy :: SProxy "locale") "") opts
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

    fmtHourMinute = unsafePartial $ fromRight $
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

    fmtHourMinuteSecond = unsafePartial $ fromRight $
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
          
    fmtWeekdayYearMonthDayHourMinuteSecond = unsafePartial $ fromRight $
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

    fmtWeekdayYearMonthDay = unsafePartial $ fromRight $
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

    fmtYearMonthDay = unsafePartial $ fromRight $
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

    fmtYearMonth = unsafePartial $ fromRight $
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

    fmtMonthDay = unsafePartial $ fromRight $
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

    fmtEraYear = unsafePartial $ fromRight $
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

    fmtEra = unsafePartial $ fromRight $
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

      assert' "fmtError expected to return error" $ isLeft fmtError

      let fmtDate1 = formatJSDate fmtWeekdayYearMonthDayHourMinuteSecond d
          fmtDate1Expected = "Tue, January 2, 2018, 12:00:00 PM"
      assert' ("fmtWeekdayYearMonthDayHourMinuteSecond: got: '" <> fmtDate1 <> "' expected: '" <> fmtDate1Expected <> "'")
        $ fmtDate1 == fmtDate1Expected
      let fmtDate2 = formatJSDate fmtWeekdayYearMonthDay d
          fmtDate2Expected = "Tuesday, Jan 02, 18"
      assert' ("fmtWeekdayYearMonthDayString: got '" <> fmtDate2 <> "' expected: '" <> fmtDate2Expected <> "'")
        $ fmtDate2 == fmtDate2Expected
      let fmtDate3 = formatJSDate fmtYearMonthDay d
          fmtDate3Expected = "J 02, 18"
      assert' ("fmtYearMonthDayString: got '" <> fmtDate3 <> "' expected: '" <> fmtDate3Expected <> "'")
        $ fmtDate3 == fmtDate3Expected
      let fmtDate4 = formatJSDate fmtYearMonth d
          fmtDate4Expected = "1/2018"
      assert' ("fmtYearMonthString: got '" <> fmtDate4 <> "' expected: '" <> fmtDate4Expected <> "'")
        $ fmtDate4 == fmtDate4Expected
      let fmtDate5 = formatJSDate fmtMonthDay d
          fmtDate5Expected = "01/02"
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
          fmtDate9Expected = "1 2, 2018 AD"
      assert' ("fmtEra: got '" <> fmtDate9 <> "' expected: '" <> fmtDate9Expected <> "'")
        $ fmtDate9 == fmtDate9Expected

      let slocs = supportedLocalesOf ["en-US", "jp-JP"]
      assert' ("supportedLocalesOf: got: " <> show slocs) $ slocs == Right ["en-US"]

      let err = supportedLocalesOf ["en_US"]
      assert' ("supportedLocalesOf got: " <> show err) $ isLeft err

      -- formatToParts is not supported in node
      -- let parts = runExcept $ formatToParts fmtHourMinute d
      -- assert' ("") $ isRight parts 

      case runExcept (resolvedOptions fmtHourMinuteSecond) of
        Left errs-> assert' ("resolvedOptions error: " <> (intercalate "; " $ renderForeignError <$> errs)) false
        Right (ResolvedOptions {locale, year, day, weekday, hour, minute, second}) -> do
          assert' ("resolvedOptions locale: got: " <> locale) $ (locale == "en-US") || (locale == "en")
          assert' ("resolvedOptions year: got Just") $ isNothing year
          assert' ("resolvedOptions day: got Just") $ isNothing day
          assert' ("resolvedOptions weekday: got Just") $ isNothing weekday
          assert' ("resolvedOptions hour: got Nothing") $ isJust hour
          assert' ("resolvedOptions minute: got Nothing") $ isJust minute
          assert' ("resolvedOptions second: got Nothing") $ isJust second

