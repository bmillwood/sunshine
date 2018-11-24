module Cell.Integrator exposing
  ( Cell
  , Msg
  , tiling
  , init
  , boost
  , value
  , step
  , msg
  )
{-| Cells are arrays of floats, where index `i+1` is interpreted by `step` as
the derivative of index `i`.

Most of the code is agnostic about how many entries the arrays have, but we
initialize them with three (colour "position", "velocity", "acceleration").

The last entry in the array is computed by `step` as a sign-flipped weighted
average of the cell's own value (index 0) and that of its neighbours, and then
a random nudge is applied. This gives a kind of locally-synchronizing simple
harmonic motion, with a bit of drift over time.
-}

import Array exposing (Array)
import Random

import Tiling exposing (Tiling)
import Timespan exposing (Timespan)
import Vector exposing (Pt)
import Weighted exposing (Weighted)

-- Index i+1 is the derivative of index i
type Cell = C (Array Float)

type Msg
  = SetRand Float
  | TweakRand Float

tiling : Tiling
tiling = Tiling.SquareHex

timeScale : Float
timeScale = 4

boostScale : Float
boostScale = 0.5

tweakSize : Float
tweakSize = 0.3

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C (Array.fromList [0, 0, 0])
  , Cmd.none -- Random.generate SetRand (Random.float (-1) 1)
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } (C ar) =
  let
      seconds = Timespan.toSeconds timeStep
  in
  Array.set
    0
    (Maybe.withDefault 0 (Array.get 0 ar) + boostScale * timeScale * seconds)
    ar
  |> C

value : Cell -> Float
value (C ar) = 0.5 * (Maybe.withDefault 0 (Array.get 0 ar) + 1)

weights : List (Weighted Pt)
weights = Weighted.self 4 ++ Weighted.adjacent tiling 1

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C ar) =
  let
      seconds = Timespan.toSeconds timeStep
  in
  ( Array.indexedMap (\o x ->
        case Array.get (o + 1) ar of
          Just dx -> clamp (-1) 1 (x + timeScale * seconds * dx)
          Nothing ->
            List.filterMap
              (\wpt ->
                getNeighbour wpt.value
                |> Maybe.andThen (\(C a) -> Array.get 0 a)
                |> Maybe.map (\v -> { weight = wpt.weight, value = v })
              )
              weights
            |> Weighted.average Vector.float
            |> Maybe.withDefault 0
            |> negate
            |> clamp (-1) 1
      )
      ar
    |> C
  , Random.generate TweakRand (Random.float (negate tweakSize) tweakSize)
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m (C ar) =
  case m of
    SetRand setTo -> (C (Array.set 0 setTo ar), Cmd.none)
    TweakRand tw ->
      let
          lastIx = Array.length ar - 1
          last = Maybe.withDefault 0 (Array.get lastIx ar)
      in
      (C (Array.set lastIx (last + tw) ar), Cmd.none)
