module Cell.Integrator exposing (Cell, Msg, init, boost, value, step, msg)
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
import Time exposing (Time)

import Vector exposing (Pt)

-- Index i+1 is the derivative of index i
type Cell = C (Array Float)

type Msg
  = SetRand Float
  | TweakRand Float

timeScale : Float
timeScale = 0.004

tweakSize : Float
tweakSize = 0.3

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( C (Array.fromList [0, 0, 0])
  , Cmd.none -- Random.generate SetRand (Random.float (-1) 1)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C ar) =
  Array.set
    0
    (Maybe.withDefault 0 (Array.get 0 ar) + 5 * timeScale * timeStep)
    ar
  |> C

value : Cell -> Float
value (C ar) = 0.5 * (Maybe.withDefault 0 (Array.get 0 ar) + 1)

weights : List (Pt, Float)
weights =
  [ (( 0,  0), 4)
  , ((-1,  0), 1)
  , (( 1,  0), 1)
  , (( 0, -1), 1)
  , (( 0,  1), 1)
  ]

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C ar) =
  ( Array.indexedMap (\o x ->
        case Array.get (o + 1) ar of
          Just dx -> clamp (-1) 1 (x + timeScale * timeStep * dx)
          Nothing ->
            List.filterMap
              (\(p, weight) ->
                getNeighbour p
                |> Maybe.andThen (\(C a) -> Array.get 0 a)
                |> Maybe.map (\value -> { weight = weight, value = value })
              )
              weights
            |> Vector.weightedAverage Vector.float
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
