module Timespan exposing
  ( Timespan
  , fromMilliseconds
  , fromSeconds
  , toSeconds
  , add, zero, scale
  , min
  )

type Timespan = T Float

fromMilliseconds : Float -> Timespan
fromMilliseconds f = T (f / 1000)

fromSeconds : Float -> Timespan
fromSeconds f = T f

toSeconds : Timespan -> Float
toSeconds (T f) = f

add : Timespan -> Timespan -> Timespan
add (T a) (T b) = T (a + b)

zero : Timespan
zero = T 0

scale : Float -> Timespan -> Timespan
scale f (T v) = T (f * v)

min : Timespan -> Timespan -> Timespan
min (T a) (T b) = T (Basics.min a b)
