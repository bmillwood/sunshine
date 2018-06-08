module Cell.PhaseSync exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Vector exposing (Pt)
import Weighted exposing (Weighted)

type Cell = C { phase : Float, speed : Float }
type Field = Phase | Speed

type Msg = Tweak Field Float

timeScale : Float
timeScale = 0.001

tweakSize : Field -> Float
tweakSize f =
  case f of
    Phase -> 0.01 * pi
    Speed -> 0.01

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C { phase = 0, speed = 1 }
  , Random.generate (Tweak Phase) (Random.float (negate pi) pi)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C cell) =
  C { cell | phase = cell.phase - 5 * timeScale * timeStep }

value : Cell -> Float
value (C { phase }) = 0.5 * (sin phase + 1)

weights : Field -> List (Weighted Pt)
weights fi =
  case fi of
    Phase -> Weighted.self 500 ++ Weighted.adjacent 1
    Speed -> Weighted.self 10 ++ Weighted.adjacent 1

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

advancePhase : { timeStep : Float, speed : Float } -> Float -> Float
advancePhase { timeStep, speed } f =
  let newF = f + timeStep * timeScale * speed
  in
      if newF > pi then newF - 2*pi else newF

randomTweak : Field -> Cmd Msg
randomTweak fi =
  let
      size = tweakSize fi
  in
  Random.generate (Tweak fi) (Random.float (negate size) size)

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C { phase, speed }) =
  let
      onNeighbours field f =
        List.filterMap
          (\wpt ->
            getNeighbour wpt.value
            |> Maybe.map (\cell -> { wpt | value = f cell })
          )
          (weights field)
  in
  ( { phase =
        onNeighbours Phase (\(C c) -> unitWithPhase c.phase)
        |> Weighted.average Vector.array
        |> Maybe.andThen (\v ->
            Maybe.map2 (\x y -> atan2 y x)
              (Array.get 0 v)
              (Array.get 1 v)
          )
        |> Maybe.withDefault phase
        |> advancePhase { timeStep = timeStep, speed = speed }
    , speed =
        onNeighbours Speed (\(C c) -> c.speed)
        |> Weighted.average Vector.float
        |> Maybe.withDefault speed
    } |> C
  , Cmd.batch (List.map randomTweak [Phase, Speed])
  )

modifyField : Field -> (Float -> Float) -> Cell -> Cell
modifyField fi modify (C cell) =
  case fi of
    Phase -> C { cell | phase = modify cell.phase }
    Speed -> C { cell | speed = modify cell.speed }

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (Tweak field tw) cell = (modifyField field (\v -> v + tw) cell, Cmd.none)
