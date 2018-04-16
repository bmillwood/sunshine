module Cell.PhaseSync exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Vector exposing (Pt)

type Cell = C { time : Time, phase : Float }

type Msg = SetPhase Float

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C { time = 0, phase = 0 }
  , Random.generate SetPhase (Random.float (negate pi) pi)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C cell) =
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

length : Array Float -> Float
length xs = sqrt (Array.foldl (+) 0 (Array.map (\x -> x * x) xs))

normalize : Array Float -> Maybe (Array Float)
normalize xs =
  let
      len = length xs
      arrayVS = Vector.array
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
        Vector.weightedAverage Vector.array phases
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
