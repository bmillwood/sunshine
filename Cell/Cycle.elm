module Cell.Cycle exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random
import Time exposing (Time)

import Vector exposing (Pt)
import Weighted exposing (Weighted)

type Cell =
  C
    { phase  : Float
    , speed  : Float
    , centre : Float
    , radius : Float
    }
type Field = Phase | Speed | Centre | Radius

type Msg = Tweak Cell

timeScale : Float
timeScale = 0.001

tweakSize : Field -> Cell -> Float
tweakSize f (C cell) =
  case f of
    Phase -> 0.01 * pi
    Speed -> 0.01
    Centre -> 0
    Radius -> 0

weights : Field -> Cell -> List (Weighted Pt)
weights fi (C cell) =
  case fi of
    Phase -> Weighted.self 500 ++ Weighted.adjacent 1
    Speed -> Weighted.self 10 ++ Weighted.adjacent 1
    Centre -> Weighted.self 1
    Radius -> Weighted.self 1

clampCell : Cell -> Cell
clampCell (C cell) =
  { phase  = cell.phase
  , speed  = max 0 cell.speed
  , centre = clamp 0 1 cell.centre
  , radius = clamp 0 (max 0 (min cell.centre (1 - cell.centre))) cell.radius
  } |> C

initCell : (Field -> Float) -> Cell
initCell f =
  C { phase = f Phase, speed = f Speed, centre = f Centre, radius = f Radius }
  |> clampCell

zero : Cell
zero = initCell (\_ -> 0)

unif01Cell : Random.Generator Cell
unif01Cell =
  let
      unif01 = Random.float 0 1
  in
  Random.map4
    (\phase speed centre radius ->
      { phase = phase
      , speed = speed
      , centre = centre
      , radius = radius
      } |> C
    )
    unif01
    unif01
    unif01
    unif01

get : Field -> Cell -> Float
get field (C cell) =
  case field of
    Phase ->  cell.phase
    Speed ->  cell.speed
    Centre -> cell.centre
    Radius -> cell.radius

mapi : (Field -> Float -> Float) -> Cell -> Cell
mapi f cell = initCell (\field -> f field (get field cell))

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  ( zero
  , unif01Cell
    |> Random.map
      (\(C random) ->
        { phase = pi * (2 * random.phase - 1)
        , speed = 1
        , centre = 0.2
        , radius = 0.2
        } |> C
      )
    |> Random.generate Tweak
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C cell) =
  C { cell | phase = cell.phase - 5 * timeScale * timeStep }

value : Cell -> Float
value (C cell) = cell.radius * sin cell.phase + cell.centre

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

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C cell) =
  let
      onNeighbours field f =
        List.filterMap
          (\wpt ->
            getNeighbour wpt.value
            |> Maybe.map (\cell -> { wpt | value = f cell })
          )
          (weights field (C cell))
  in
  ( { cell
    | phase =
        onNeighbours Phase (\(C c) -> unitWithPhase c.phase)
        |> Weighted.average Vector.array
        |> Maybe.andThen (\v ->
            Maybe.map2 (\x y -> atan2 y x)
              (Array.get 0 v)
              (Array.get 1 v)
          )
        |> Maybe.withDefault cell.phase
        |> advancePhase { timeStep = timeStep, speed = cell.speed }
    , speed =
        onNeighbours Speed (\(C c) -> c.speed)
        |> Weighted.average Vector.float
        |> Maybe.withDefault cell.speed
    } |> C
  , unif01Cell
    |> Random.map (mapi (\field v -> (2 * v - 1) * tweakSize field (C cell)))
    |> Random.generate Tweak
  )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg (Tweak add) cell =
  ( initCell (\field -> get field cell + get field add)
  , Cmd.none
  )
