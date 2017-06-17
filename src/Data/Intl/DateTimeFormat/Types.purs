module Data.Intl.DateTimeFormat.Types where

import Prelude (class Show)

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

