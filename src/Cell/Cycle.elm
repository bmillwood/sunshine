module Cell.Cycle exposing (Cell, Msg, init, boost, value, step, msg)

import Array exposing (Array)
import Random

import Lerp
import Sample
import Timespan exposing (Timespan)
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
timeScale = 1

value : Cell -> Float
value (C cell) = cell.radius * sin cell.phase + cell.centre

tweakSize : Field -> Cell -> Float
tweakSize f cell =
  case f of
    Phase -> 0.01 * pi
    Speed -> 0.05
    Centre -> 0
    Radius -> 0

weights : Field -> Cell -> List (Weighted Pt)
weights fi cell =
  let
      v = value cell

      centreWeights =
        [ (0.00, 160)
        , (0.50, 160)
        , (0.60,  40)
        , (0.70,   5)
        , (1.00,   0)
        ] |> Array.fromList

      csw =
        Lerp.at Vector.float centreWeights v
        |> Maybe.withDefault 160
  in
  case fi of
    Phase  -> Weighted.self 500 ++ Weighted.adjacent (v^2)
    Speed  -> Weighted.self 40  ++ Weighted.adjacent (v^2)
    Centre ->
      Weighted.self csw
      ++ Weighted.adjacent 1
      ++ Weighted.diagonal 1
    Radius -> Weighted.self 1

clampCell : Cell -> Cell
clampCell (C cell) =
  let
      radius = clamp 0 0.5 cell.radius
  in
  { phase  = cell.phase
  , speed  = cell.speed
  , centre = clamp radius (1 - radius) cell.centre
  , radius = radius
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
        , speed =
          Sample.ofUniform 0 1 random.speed
          |> Sample.toExponential { mean = 1 }
        , centre = 0.1
        , radius = 0.1
        } |> C
      )
    |> Random.generate Tweak
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } (C cell) =
  let
      seconds = Timespan.toSeconds timeStep
  in
  { cell
  | centre = cell.centre + seconds * timeScale * 2
  , speed  = cell.speed  + seconds * timeScale * 1
  , phase  = cell.phase  + cell.speed * seconds * timeScale
  } |> C

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

advancePhase : { timeStep : Timespan, speed : Float } -> Float -> Float
advancePhase { timeStep, speed } f =
  let newF = f + Timespan.toSeconds timeStep * timeScale * speed
  in
  if newF > pi then newF - 2*pi else newF

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C cell) =
  let
      onNeighbours field f =
        List.filterMap
          (\wpt ->
            getNeighbour wpt.value
            |> Maybe.map (\c -> { weight = wpt.weight, value = f c })
          )
          (weights field (C cell))

      straightAverage field =
        onNeighbours field (get field)
        |> Weighted.average Vector.float
        |> Maybe.withDefault (get field (C cell))
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
    , speed  = straightAverage Speed
    , centre = straightAverage Centre
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
