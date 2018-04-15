module Cell.PhaseSync exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Shared exposing (..)

type Cell = C { time : Time, phase : Float }

type Msg = SetPhase Float

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C { time = 0, phase = 0 }
  , Random.generate SetPhase (Random.float (negate pi) pi)
  )

boost : Time -> Cell -> Cell
boost timeStep (C cell) =
  C { cell | phase = cell.phase - 5 * timeStep }

value : Cell -> Float
value (C { time, phase }) = 0.5 * (sin (time + phase) + 1)

weights : List (Pt, Float)
weights =
  [ (( 0,  0), 500)
  , ((-1,  0),   1)
  , (( 1,  0),   1)
  , (( 0, -1),   1)
  , (( 0,  1),   1)
  ]

type alias VectorSpace vector =
  { add   : vector -> vector -> vector
  , zero  : vector
  , scale : Float  -> vector -> vector
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

arrayVS : VectorSpace (Array Float)
arrayVS =
  { add = (\a1 a2 -> Array.indexedMap (\i x -> Maybe.withDefault 0 (Array.get i a2) + x) a1)
  , zero = Array.empty
  , scale = (\f a -> Array.map (\x -> f * x) a)
  }

length : Array Float -> Float
length xs = sqrt (Array.foldl (+) 0 (Array.map (\x -> x * x) xs))

normalize : Array Float -> Maybe (Array Float)
normalize xs =
  let
      len = length xs
  in
  if len == 0
    then Nothing
    else Just (arrayVS.scale (1 / len) xs)

unitWithPhase : Float -> Array Float
unitWithPhase p = Array.fromList [cos p, sin p]

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C { time, phase }) =
  ( { time = time + timeStep
    , phase =
        let
            phases =
              List.filterMap
                (\(pt, weight) ->
                  getNeighbour pt
                  |> Maybe.map (\ (C c) ->
                      { weight = weight, value = unitWithPhase c.phase }
                    )
                )
                weights
        in
        weightedAverage arrayVS phases
        |> Maybe.andThen (\v ->
            Maybe.map2 (\x y -> atan2 y x)
              (Array.get 0 v)
              (Array.get 1 v)
          )
        |> Maybe.withDefault phase
    } |> C
  , Cmd.none
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (SetPhase v) (C c) = (C { c | phase = v }, Cmd.none)
