module Lerp exposing (at)

import Array exposing (Array)
import Vector

at : Vector.Space a -> Array (Float, a) -> Float -> Maybe a
at space points x =
  let
      len = Array.length points

      go li lk lv ri rk rv =
        case compare (ri - li) 1 of
          LT -> Nothing
          EQ ->
            Vector.along space
              ((x - lk) / (rk - lk))
              lv
              rv
            |> Just
          GT ->
            let
                i = li + floor (toFloat (ri - li) / 2)
            in
            Array.get i points
            |> Maybe.andThen (\(k, v) ->
              case compare x k of
                EQ -> Just v
                LT -> go li lk lv i k v
                GT -> go i k v ri rk rv)
  in
  Array.get 0 points
  |> Maybe.andThen (\(lk, lv) ->
    case compare x lk of
      EQ -> Just lv
      LT -> Nothing
      GT ->
        Array.get (len - 1) points
        |> Maybe.andThen (\(rk, rv) ->
          case compare x rk of
            EQ -> Just rv
            GT -> Nothing
            LT ->
              go 0 lk lv (len - 1) rk rv))
