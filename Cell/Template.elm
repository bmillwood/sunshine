module Cell.Template exposing (Cell, Msg, init, boost, value, step, msg)

import Time exposing (Time)

import Shared exposing (..)

type Cell = C
type Msg = M

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C
  , Cmd.none
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } C = C

value : Cell -> Float
value C = 0.5

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour C = (C, Cmd.none)

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m cell =
  case m of
    M -> (cell, Cmd.none)
