module Cell.PhaseSync exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Vector exposing (Pt)

type Cell = C { phase : Float }

type Msg = SetPhase Float

timeScale : Float
timeScale = 0.004

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C { phase = 0 }
  , Random.generate SetPhase (Random.float (negate pi) pi)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C cell) =
  C { cell | phase = cell.phase - 5 * timeScale * timeStep }

value : Cell -> Float
value (C { phase }) = 0.5 * (sin phase + 1)

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

advancePhase : { timeStep : Float } -> Float -> Float
advancePhase { timeStep } f =
  let newF = f + timeStep * timeScale
  in
      if newF > pi then newF - 2*pi else newF

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C { phase }) =
  ( { phase =
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
        |> advancePhase { timeStep = timeStep }
    } |> C
  , Cmd.none
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (SetPhase v) (C c) = (C { phase = v }, Cmd.none)
