module Draw exposing (draw)

import Array exposing (Array)
import Dict exposing (Dict)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes

import Lerp
import Tiling exposing (Tiling, Local (..))
import Vector exposing (Pt)

scale : Tiling.Local -> number
scale tl =
  case tl of
    Square -> 40
    Hex -> 40

colours : Array (Float, Vector.Vec3 Float)
colours =
  [ (0.00, Vector.Vec3 0.00 0.00 0.00)
  , (0.20, Vector.Vec3 0.25 0.00 0.25)
  , (0.50, Vector.Vec3 0.75 0.00 0.00)
  , (0.75, Vector.Vec3 1.00 1.00 0.00)
  , (0.80, Vector.Vec3 1.00 1.00 1.00)
  , (0.85, Vector.Vec3 1.00 1.00 1.00)
  , (0.90, Vector.Vec3 0.00 1.00 1.00)
  , (1.00, Vector.Vec3 0.00 0.80 1.00)
  ] |> Array.fromList

toColour : Float -> String
toColour value =
  let
      c f = String.fromInt (floor (255 * min (max 0 f) 1))
      make (Vector.Vec3 r g b) =
        "rgb(" ++ c r ++ "," ++ c g ++ "," ++ c b ++ ")"
  in
  -- Linearly interpolating RGB values is incorrect for interpolating
  -- colours. Should be fine for the time being though.
  Lerp.at (Vector.vec3 Vector.float) colours value
  |> Maybe.withDefault (Vector.Vec3 0 0 0)
  |> make

squareFor : { events : Pt -> List (Svg.Attribute msg) }
  -> (Pt, Float) -> Svg.Svg msg
squareFor { events } ((i, j), value) =
  Svg.rect
    (List.append
      [ Svg.Attributes.width  (String.fromInt (scale Square))
      , Svg.Attributes.height (String.fromInt (scale Square))
      , Svg.Attributes.x      (String.fromInt (j * (scale Square)))
      , Svg.Attributes.y      (String.fromInt (i * (scale Square)))
      , Svg.Attributes.fill   (toColour value)
      ]
      (events (i, j))
    )
    []

rt3over2 : Float
rt3over2 = sqrt 3 / 2

hexPoints : List (Float, Float)
hexPoints =
  [ (0.50, 0.00)
  , (1.00, 0.25)
  , (1.00, 0.75)
  , (0.50, 1.00)
  , (0.00, 0.75)
  , (0.00, 0.25)
  ] |> List.map (\(x, y) -> (rt3over2 * x, y))

hexFor : { events : Pt -> List (Svg.Attribute msg) }
  -> (Pt, Float) -> Svg.Svg msg
hexFor { events } ((i, j), value) =
  let
      toPointsString pts =
        List.map
          (\(x,y) ->
            String.fromInt (round x) ++ "," ++ String.fromInt (round y)
          )
          pts
        |> String.join " "

      offsetX = rt3over2 * toFloat j / 2
      offsetY = 0.75 * toFloat i

      points =
        List.map
          (\(x, y) -> (scale Hex * (x + offsetX), scale Hex * (y + offsetY)))
          hexPoints
  in
  Svg.polygon
    (List.append
      [ Svg.Attributes.points (toPointsString points)
      , Svg.Attributes.fill   (toColour value)
      ]
      (events (i, j))
    )
    []

draw : Tiling -> Dict Pt Float
  -> { events : Pt -> List (Svg.Attribute msg) }
  -> Html msg
draw tiling values { events } =
  let
      tilingBounds = Tiling.bounds tiling
      (tminI, tminJ) = tilingBounds.min
      (tmaxI, tmaxJ) = tilingBounds.max
      (width, height, shapeFor) =
        case Tiling.local tiling of
          Square ->
            ( scale Square * toFloat (tmaxJ - tminJ + 1)
            , scale Square * toFloat (tmaxI - tminI + 1)
            , squareFor { events = events }
            )
          Hex ->
            ( scale Hex * toFloat (tmaxJ - tminJ + 1) * rt3over2
            , scale Hex * (toFloat (tmaxI - tminI) * 0.75 + 1)
            , hexFor { events = events }
            )
  in
  Svg.svg
    [ Html.Attributes.width  (round width)
    , Html.Attributes.height (round height)
    ]
    (List.map shapeFor (Dict.toList values))
