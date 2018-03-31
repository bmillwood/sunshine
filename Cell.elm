module Cell exposing (Cell, init, boost, value, step)

import Array exposing (Array)
import Maybe exposing (Maybe)
import Time exposing (Time)

type Cell = C (Array Float)

init : Int -> Int -> Cell
init i j = C (Array.fromList [0, 0, 0])

boost : Time -> Cell -> Cell
boost timeStep (C ar) =
  Array.set
    0
    (Maybe.withDefault 0 (Array.get 0 ar) + 5 * timeStep)
    ar
  |> C

value : Cell -> Float
value (C ar) = 0.5 * (Maybe.withDefault 0 (Array.get 0 ar) + 1)

weights : List ((Int, Int), Float)
weights =
  [ (( 0,  0), 4)
  , ((-1,  0), 1)
  , (( 1,  0), 1)
  , (( 0, -1), 1)
  , (( 0,  1), 1)
  ]

weightedAverage : List { weight : Float, value : Float } -> Maybe Float
weightedAverage xs =
  case List.filter (\x -> x.weight > 0) xs of
    [] -> Nothing
    nonZeroes ->
      let
          total =
            List.foldl
              (\ this total ->
                { weight = this.weight + total.weight
                , value  = this.weight * this.value + total.value
                })
              { weight = 0, value = 0 }
              nonZeroes
      in
      Just (total.value / total.weight)

step : { timeStep : Time } -> ((Int, Int) -> Maybe Cell) -> Cell -> Cell
step { timeStep } getNeighbour (C ar) =
  let
      clamp x = min 1 (max (-1) x)
  in
  Array.indexedMap (\o x ->
      case Array.get (o + 1) ar of
        Just dx -> clamp (x + timeStep * dx)
        Nothing ->
          List.filterMap
            (\(p, weight) ->
              getNeighbour p
              |> Maybe.andThen (\(C a) -> Array.get 0 a)
              |> Maybe.map (\value -> { weight = weight, value = value })
            )
            weights
          |> weightedAverage
          |> Maybe.withDefault 0
          |> negate
          |> clamp
    )
    ar
  |> C
