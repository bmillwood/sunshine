module Cell.Integrator exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Shared exposing (..)

type Cell = C (Array Float)

type Msg = SetRand Float

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C (Array.fromList [0, 0, 0])
  , Random.generate SetRand (Random.float (-1) 1)
  )

boost : Time -> Cell -> Cell
boost timeStep (C ar) =
  Array.set
    0
    (Maybe.withDefault 0 (Array.get 0 ar) + 5 * timeStep)
    ar
  |> C

value : Cell -> Float
value (C ar) = 0.5 * (Maybe.withDefault 0 (Array.get 0 ar) + 1)

weights : List (Pt, Float)
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

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C ar) =
  ( Array.indexedMap (\o x ->
        case Array.get (o + 1) ar of
          Just dx -> clamp (-1) 1 (x + timeStep * dx)
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
            |> clamp (-1) 1
      )
      ar
    |> C
  , Cmd.none
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (SetRand r) (C ar) = (C (Array.set 0 r ar), Cmd.none)
