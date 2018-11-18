module Cell.TestCard exposing (Cell, Msg, init, boost, value, step, msg)

import Timespan exposing (Timespan)
import Vector exposing (Pt)

type Cell = C Pt
type Msg = M

init : Pt -> (Cell, Cmd Msg)
init pt =
  ( C pt
  , Cmd.none
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } cell = cell

value : Cell -> Float
value (C (cx, cy)) =
  {- I can't remember any PRNGs but I remember that inverting discrete log
     is hard. -}
  let
      m = 107
      loop x y acc =
        if y == 0
        then acc
        else
          loop
            (modBy m (x * x))
            (y // 2)
            (if modBy 2 y == 1 then modBy m (acc * x) else acc)
      positivize n =
        if n >= 0
        then 2 * n
        else 2 * abs n - 1
      notDiv d n =
        n + n // (d - 1) + 1
      tweak n = notDiv m (positivize n + 7)
  in
  toFloat (loop (tweak cx) (tweak cy) 1) / toFloat m

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour cell = (cell, Cmd.none)

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m cell =
  case m of
    M -> (cell, Cmd.none)
