module Cell.Targeting exposing (Cell, Msg, init, boost, value, step, msg)

import Random
import Time exposing (Time)

import Vector exposing (Pt)
import Weighted exposing (Weighted)

type alias Happiness =
  { baseHappiness : Float
  , happiness : Float
  , target : Float
  , period : Time
  , timeTo : Time
  }

type Cell = C Happiness

type Msg = Target { target : Float, period : Time }

weights : List (Weighted Pt)
weights = Weighted.adjacent 1 ++ Weighted.diagonal 0.5

vectorTarget : Vector.VectorSpace { target : Float, period : Time }
vectorTarget =
  { zero = { target = 0, period = 0 }
  , add = (\t1 t2 -> { target = t1.target + t2.target, period = t1.period + t2.period })
  , scale = (\f t -> { target = f * t.target, period = f * t.period })
  }

genTarget : Happiness -> (Pt -> Maybe Cell) -> Cmd Msg
genTarget hap getNeighbour =
  let
      randomWeight = 4
      neighbourTargets =
        List.filterMap (\wpt ->
          getNeighbour wpt.value
          |> Maybe.map (\ (C hap) ->
                { weight = hap.happiness * wpt.weight
                , value = { target = hap.target, period = hap.period }
                }
              )
          )
          weights
      ofRandoms t1 t2 =
        let
            randomTarget = { target = t1, period = t2 * Time.second }
        in
        Weighted.average vectorTarget
          ({ weight = randomWeight, value = randomTarget } :: neighbourTargets)
        |> Maybe.withDefault { target = hap.target, period = Time.second }
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
        , period = Time.second
        , timeTo = Time.second
        }
  in
  ( C hap
  , genTarget hap (\_ -> Nothing)
  )

boost : { timeStep : Time } -> Cell -> Cell
boost { timeStep } (C hap) =
  let
      baseBoostRate = 0.06
      baseBoost     = Time.inSeconds timeStep * baseBoostRate
      hapBoostRate  = 0.24
      hapBoost      = baseBoost + Time.inSeconds timeStep * hapBoostRate
      clampHap      = clamp 0 1
  in
  { hap
  | baseHappiness = hap.baseHappiness + baseBoost |> clampHap
  , happiness     = hap.happiness     + hapBoost  |> clampHap
  , target        = hap.target        + hapBoost  |> clampHap
  } |> C

value : Cell -> Float
value (C hap) = hap.happiness

step : { timeStep : Time } -> (Pt -> Maybe Cell) -> Cell -> (Cell, Cmd Msg)
step { timeStep } getNeighbour (C hap) =
  if hap.timeTo <= 0
  then
    ( C { hap | happiness = hap.target }
    , genTarget hap getNeighbour
    )
  else
    ( { hap
      | happiness = hap.happiness + (timeStep / hap.timeTo) * (hap.target - hap.happiness)
      , timeTo = hap.timeTo - timeStep
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
