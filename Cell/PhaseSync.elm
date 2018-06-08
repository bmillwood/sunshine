module Cell.PhaseSync exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Vector exposing (Pt)
import Weighted exposing (Weighted)

type Cell = C { phase : Float }

type Msg = TweakPhase Float

timeScale : Float
timeScale = 0.004

tweakSize : Float
tweakSize = 0.01 * pi

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C { phase = 0 }
  , Random.generate TweakPhase (Random.float (negate pi) pi)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C cell) =
  C { cell | phase = cell.phase - 5 * timeScale * timeStep }

value : Cell -> Float
value (C { phase }) = 0.5 * (sin phase + 1)

weights : List (Weighted Pt)
weights = Weighted.self 500 ++ Weighted.adjacent 1

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
                (\wpt ->
                  getNeighbour wpt.value
                  |> Maybe.map (\ (C c) ->
                      { weight = wpt.weight, value = unitWithPhase c.phase }
                    )
                )
                weights
        in
        Weighted.average Vector.array phases
        |> Maybe.andThen (\v ->
            Maybe.map2 (\x y -> atan2 y x)
              (Array.get 0 v)
              (Array.get 1 v)
          )
        |> Maybe.withDefault phase
        |> advancePhase { timeStep = timeStep }
    } |> C
  , Random.generate TweakPhase (Random.float (negate tweakSize) tweakSize)
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (TweakPhase tw) (C c) = (C { phase = c.phase + tw }, Cmd.none)
