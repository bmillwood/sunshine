module Cell.TestCard exposing
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

type Cell = C Pt
type Msg = M

tiling : Tiling
tiling = Tiling.SquareHex

init : Pt -> (Cell, Cmd Msg)
init pt =
  ( C pt
  , Cmd.none
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } cell = cell

value : Cell -> Float
value (C (cx, cy)) =
  sin (toFloat (cx + cy) / 4) * -0.3 + 0.5

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour cell = (cell, Cmd.none)

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m cell =
  case m of
    M -> (cell, Cmd.none)
