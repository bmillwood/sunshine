module Weighted exposing (Weighted, map, average, self, adjacent, diagonal)

import Vector exposing (Pt, VectorSpace)

type alias Weighted a = { weight : Float, value : a }

map : (a -> b) -> Weighted a -> Weighted b
map f w = { w | value = f w.value }

average : VectorSpace v -> List (Weighted v) -> Maybe v
average { add, zero, scale } xs =
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

ofPairs : List (Pt, Float) -> List (Weighted Pt)
ofPairs = List.map (\(pt, w) -> { weight = w, value = pt })

self : Float -> List (Weighted Pt)
self f = ofPairs [ ((0, 0), f) ]

adjacent : Float -> List (Weighted Pt)
adjacent f =
  ofPairs
    [ (( 0, -1), f)
    , (( 0,  1), f)
    , ((-1,  0), f)
    , (( 1,  0), f)
    ]

diagonal : Float -> List (Weighted Pt)
diagonal f =
  ofPairs
    [ ((-1, -1), f)
    , ((-1,  1), f)
    , (( 1, -1), f)
    , (( 1,  1), f)
    ]
