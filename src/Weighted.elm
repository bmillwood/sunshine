module Weighted exposing (Weighted, map, average, self, adjacent, diagonal)

import Tiling
import Vector exposing (Pt)

type alias Weighted a = { weight : Float, value : a }

map : (a -> b) -> Weighted a -> Weighted b
map f w = { weight = w.weight, value = f w.value }

average : Vector.Space v -> List (Weighted v) -> Maybe v
average { add, zero, scale } xs =
  case List.filter (\x -> x.weight > 0) xs of
    [] -> Nothing
    nonZeroes ->
      let
          total =
            List.foldl
              (\ this acc ->
                { weight = this.weight + acc.weight
                , value  = add (scale this.weight this.value) acc.value
                })
              { weight = 0, value = zero }
              nonZeroes
      in
      Just (scale (1 / total.weight) total.value)

ofPairs : List (Pt, Float) -> List (Weighted Pt)
ofPairs = List.map (\(pt, w) -> { weight = w, value = pt })

self : Float -> List (Weighted Pt)
self f = ofPairs [ ((0, 0), f) ]

ofTileCoords
  :  (Tiling.Local -> List Pt)
  -> Tiling.Tiling -> Float -> List (Weighted Pt)
ofTileCoords getPairs t weight =
  List.map
    (\p -> (p, weight))
    (getPairs (Tiling.local t))
  |> ofPairs

adjacent : Tiling.Tiling -> Float -> List (Weighted Pt)
adjacent = ofTileCoords Tiling.adjacent

diagonal : Tiling.Tiling -> Float -> List (Weighted Pt)
diagonal = ofTileCoords Tiling.diagonal
