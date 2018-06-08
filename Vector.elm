module Vector exposing (Pt, VectorSpace, float, array, time, weightedAverage)

import Array exposing (Array)
import Time exposing (Time)

type alias Pt = (Int, Int)

type alias VectorSpace vector =
  { add   : vector -> vector -> vector
  , zero  : vector
  , scale : Float  -> vector -> vector
  }

float : VectorSpace Float
float =
  { add = (+)
  , zero = 0
  , scale = (*)
  }

time : VectorSpace Time
time = float

array : VectorSpace (Array Float)
array =
  { add = (\a1 a2 -> Array.indexedMap (\i x -> Maybe.withDefault 0 (Array.get i a2) + x) a1)
  , zero = Array.empty
  , scale = (\f a -> Array.map (\x -> f * x) a)
  }

weightedAverage : VectorSpace v -> List { weight : Float, value : v } -> Maybe v
weightedAverage { add, zero, scale } xs =
  case List.filter (\x -> x.weight > 0) xs of
    [] -> Nothing
    nonZeroes ->
      let
          total =
            List.foldl
              (\ this total ->
                { weight = this.weight + total.weight
                , value  = add (scale this.weight this.value) total.value
                })
              { weight = 0, value = zero }
              nonZeroes
      in
      Just (scale (1 / total.weight) total.value)
