module Cell.Mean exposing
  ( Cell
  , Msg
  , tiling
  , init
  , boost
  , value
  , step
  , msg
  )

import Tiling exposing (Tiling)
import Timespan exposing (Timespan)
import Vector exposing (Pt)
import Weighted exposing (Weighted)

type Cell = C Float
type Msg = M

tiling : Tiling
tiling = Tiling.HexHex

boostFactor : Float
boostFactor = 1

weights : List (Weighted Pt)
weights = Weighted.self 100 ++ Weighted.adjacent tiling 1

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C 0
  , Cmd.none
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } (C x) =
  C (clamp 0 1 (x + boostFactor * Timespan.toSeconds timeStep))

value : Cell -> Float
value (C x) = x

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C _) =
  let
      newX =
        List.filterMap
          (\wpt ->
            getNeighbour wpt.value
            |> Maybe.map (\(C v) -> { weight = wpt.weight, value = v })
          )
          weights
        |> Weighted.average Vector.float
        |> Maybe.withDefault 0
  in
  ( C newX
  , Cmd.none
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m cell =
  case m of
    M -> (cell, Cmd.none)
