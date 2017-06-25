module Data.Intl.DateTimeFormat.Generic
  ( class GenericFormatComponent
  , genericFormatComponent'
  , genericFormatComponent
  , class GenericFormatComponentFields
  , genericFormatComponentFields
  ) where

import Data.Generic.Rep (class Generic, Constructor(Constructor), Field(..), NoConstructors, Product(..), Rec(..), from)
import Data.Intl.DateTimeFormat.Class (FormatComponentRecord(..))
import Data.Intl.DateTimeFormat.Types (MonthRep, NumericRep, TimeZoneNameRep, StringRep)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, lookup, singleton, union)
import Prelude (show)

class GenericFormatComponent a where
  genericFormatComponent' :: a -> FormatComponentRecord

class GenericFormatComponentFields a where
  genericFormatComponentFields :: a -> StrMap String

instance genericFormatComponentNoConsturctors :: GenericFormatComponent NoConstructors where
  genericFormatComponent' a = FormatComponentRecord
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

instance genericFormatComponentConstructor :: (GenericFormatComponent a) => GenericFormatComponent (Constructor name a) where
  genericFormatComponent' (Constructor a) = genericFormatComponent' a

instance genericFormatComponentFieldsEra
    :: GenericFormatComponentFields (Field "era" StringRep) where
  genericFormatComponentFields (Field a) = singleton "era" (show a)

instance genericFormatComponentFieldsWeekday
    :: GenericFormatComponentFields (Field "weekday" StringRep) where
  genericFormatComponentFields (Field a) = singleton "weekday" (show a)

instance genericFormatComponentFieldsYear
    :: GenericFormatComponentFields (Field "year" NumericRep) where
  genericFormatComponentFields (Field a) = singleton "year" (show a)

instance genericFormatComponentFieldsMonth
    :: GenericFormatComponentFields (Field "month" MonthRep) where
  genericFormatComponentFields (Field a) = singleton "month" (show a)

instance genericFormatComponentFieldsDay
    :: GenericFormatComponentFields (Field "day" NumericRep) where
  genericFormatComponentFields (Field a) = singleton "day" (show a)

instance genericFormatComponentFieldsHour
    :: GenericFormatComponentFields (Field "hour" NumericRep) where
  genericFormatComponentFields (Field a) = singleton "hour" (show a)

instance genericFormatComponentFieldsMinute
    :: GenericFormatComponentFields (Field "minute" NumericRep) where
  genericFormatComponentFields (Field a) = singleton "minute" (show a)

instance genericFormatComponentFieldsSecond
    :: GenericFormatComponentFields (Field "second" NumericRep) where
  genericFormatComponentFields (Field a) = singleton "second" (show a)

instance genericFormatComponentFieldsTimeZoneName
    :: GenericFormatComponentFields (Field "timeZoneName" TimeZoneNameRep) where
  genericFormatComponentFields (Field a) = singleton "timeZoneName" (show a)

instance genericFormatComponentFieldsProduct
    :: (GenericFormatComponentFields a, GenericFormatComponentFields b)
    => GenericFormatComponentFields (Product a b) where
  genericFormatComponentFields (Product a b) = 
      union (genericFormatComponentFields a) (genericFormatComponentFields b)

instance genericFormatComponentRec
    :: (GenericFormatComponentFields a)
    => GenericFormatComponent (Rec a) where
  genericFormatComponent' (Rec a) = FormatComponentRecord
    { era: "era" `lookup` u
    , weekday: "weekday" `lookup` u
    , year: "year" `lookup` u
    , month: "month" `lookup` u
    , day: "day" `lookup` u
    , hour: "hour" `lookup` u
    , minute: "minute" `lookup` u
    , second: "second" `lookup` u
    , timeZoneName: "timeZoneName" `lookup` u
    }
    where
      u = genericFormatComponentFields a

genericFormatComponent :: forall a rep. Generic a rep => GenericFormatComponent rep => a -> FormatComponentRecord
genericFormatComponent a = genericFormatComponent' (from a)
