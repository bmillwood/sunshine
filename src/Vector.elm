module Vector exposing
  ( Pt
  , Space, along
  , float, array, time
  , Vec3(..), vec3
  )

import Array exposing (Array)
import Timespan exposing (Timespan)

type alias Pt = (Int, Int)

type alias Space vector =
  { add   : vector -> vector -> vector
  , zero  : vector
  , scale : Float  -> vector -> vector
  }

along : Space vector -> Float -> vector -> vector -> vector
along space amount a b =
  space.add a (space.scale amount (space.add b (space.scale (-1) a)))

float : Space Float
float =
  { add = (+)
  , zero = 0
  , scale = (*)
  }

time : Space Timespan
time = { add = Timespan.add, zero = Timespan.zero, scale = Timespan.scale }

array : Space (Array Float)
array =
  { add = (\a1 a2 -> Array.indexedMap (\i x -> Maybe.withDefault 0 (Array.get i a2) + x) a1)
  , zero = Array.empty
  , scale = (\f a -> Array.map (\x -> f * x) a)
  }

type Vec3 a = Vec3 a a a

vec3 : Space a -> Space (Vec3 a)
vec3 { add, zero, scale } =
  { add =
      (\(Vec3 a1 a2 a3) (Vec3 b1 b2 b3) ->
        Vec3 (add a1 b1) (add a2 b2) (add a3 b3))
  , zero = Vec3 zero zero zero
  , scale = (\f (Vec3 x y z) -> Vec3 (scale f x) (scale f y) (scale f z))
  }
