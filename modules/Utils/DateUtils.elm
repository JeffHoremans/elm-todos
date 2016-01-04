module DateUtils
    ( formatTime, formatDate
    , isBefore, isAfter, isEqual
    , isBeforeDate, isAfterDate, isEqualDate
    , plusDay) where

{-| A library for some additional date utilities -}
import Date exposing ( Date )
import Time exposing ( Time )

{-| Format the given time to a string conforming the ISO 8601 syntax (YYYY-MM-DD) (the preferred JavaScript date format).

    formatTime 1450958489791 == "2015-12-24"
-}
formatTime : Time -> String
formatTime time = formatDate (Date.fromTime time)

{-| Format the given time to a string conforming the ISO 8601 syntax (YYYY-MM-DD) (the preferred JavaScript date format).
-}
formatDate : Date -> String
formatDate date = (toString (Date.year date)) ++ "-" ++ month date ++ "-" ++ day date

month : Date -> String
month date =
  let month = (Date.month date) in
    case month of
      Date.Jan -> "01"
      Date.Feb -> "02"
      Date.Mar -> "03"
      Date.Apr -> "04"
      Date.May -> "05"
      Date.Jun -> "06"
      Date.Jul -> "07"
      Date.Aug -> "08"
      Date.Sep -> "09"
      Date.Oct -> "10"
      Date.Nov -> "11"
      Date.Dec -> "12"

day : Date -> String
day date =
  let day = (Date.day date) in
    if day < 10 then "0"++(toString day)
    else (toString day)

isBefore: Time -> Time -> Bool
isBefore a b =
  case compare a b of
    LT -> True
    _ -> False

isAfter: Time -> Time -> Bool
isAfter a b = isBefore b a

isEqual: Time -> Time -> Bool
isEqual a b =
  case compare a b of
    EQ -> True
    _ -> False

isBeforeDate: Date -> Date -> Bool
isBeforeDate a b = isBefore (Date.toTime a) (Date.toTime b)

isAfterDate: Date -> Date -> Bool
isAfterDate a b = isAfter (Date.toTime a) (Date.toTime b)

isEqualDate: Date -> Date -> Bool
isEqualDate a b = isEqual (Date.toTime a) (Date.toTime b)

plusDay: Time -> Time
plusDay = (+) 86400000
