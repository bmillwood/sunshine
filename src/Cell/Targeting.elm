module Cell.Targeting exposing (Cell, Msg, init, boost, value, step, msg)

import Random

import Timespan exposing (Timespan)
import Vector exposing (Pt)
import Weighted exposing (Weighted)

type alias Happiness =
  { baseHappiness : Float
  , happiness : Float
  , target : Float
  , period : Timespan
  , timeTo : Timespan
  }

type Cell = C Happiness

type Msg = Target { target : Float, period : Timespan }

weights : List (Weighted Pt)
weights = Weighted.adjacent 1 ++ Weighted.diagonal 0.5

vectorTarget : Vector.Space { target : Float, period : Timespan }
vectorTarget =
  { zero = { target = 0, period = Timespan.zero }
  , add = (\t1 t2 -> { target = t1.target + t2.target, period = Timespan.add t1.period t2.period })
  , scale = (\f t -> { target = f * t.target, period = Timespan.scale f t.period })
  }

genTarget : Happiness -> (Pt -> Maybe Cell) -> Cmd Msg
genTarget hap getNeighbour =
  let
      randomWeight = 4
      neighbourTargets =
        List.filterMap (\wpt ->
          getNeighbour wpt.value
          |> Maybe.map (\ (C nhap) ->
                { weight = nhap.happiness * wpt.weight
                , value = { target = nhap.target, period = nhap.period }
                }
              )
          )
          weights
      ofRandoms t1 t2 =
        let
            randomTarget = { target = t1, period = Timespan.fromSeconds t2 }
            defaultPeriod = Timespan.fromSeconds 1
        in
        Weighted.average vectorTarget
          ({ weight = randomWeight, value = randomTarget } :: neighbourTargets)
        |> Maybe.withDefault { target = hap.target, period = defaultPeriod }
  in
  Random.generate Target <|
    Random.map2
      ofRandoms
      (Random.float hap.baseHappiness (min 1 (hap.baseHappiness * 2)))
      (Random.float 1 5)

init : Pt -> (Cell, Cmd Msg)
init (_, _) =
  let
      hap =
        { baseHappiness = 0.5
        , happiness = 0.75
        , target = 0.5
        , period = Timespan.fromSeconds 1
        , timeTo = Timespan.fromSeconds 1
        }
  in
  ( C hap
  , genTarget hap (\_ -> Nothing)
  )

boost : { timeStep : Timespan } -> Cell -> Cell
boost { timeStep } (C hap) =
  let
      baseBoostRate = 0.06
      baseBoost     = Timespan.toSeconds timeStep * baseBoostRate
      hapBoostRate  = 0.24
      hapBoost      = baseBoost + Timespan.toSeconds timeStep * hapBoostRate
      clampHap      = clamp 0 1
  in
  { hap
  | baseHappiness = hap.baseHappiness + baseBoost |> clampHap
  , happiness     = hap.happiness     + hapBoost  |> clampHap
  , target        = hap.target        + hapBoost  |> clampHap
  } |> C

value : Cell -> Float
value (C hap) = hap.happiness

step : { timeStep : Timespan } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C hap) =
  let
      timeTo = Timespan.toSeconds hap.timeTo
      timeStepSec = Timespan.toSeconds timeStep
  in
  if timeTo <= 0
  then
    ( C { hap | happiness = hap.target }
    , genTarget hap getNeighbour
    )
  else
    ( { hap
      | happiness = hap.happiness + (timeStepSec / timeTo) * (hap.target - hap.happiness)
      , timeTo = Timespan.fromSeconds (timeTo - timeStepSec)
      } |> C
    , Cmd.none
    )

msg : Msg -> Cell -> (Cell, Cmd Msg)
msg m (C hap) =
  case m of
    Target { target, period } ->
      ( C { hap | target = target, period = period, timeTo = period }
      , Cmd.none
      )
