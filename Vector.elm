module Vector exposing (Pt, VectorSpace, float, array, time)

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
