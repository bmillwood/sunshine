module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import List
import Maybe exposing (Maybe)
import Time exposing (Time)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Svg.Events

type Msg =
    Tick Time
  | Moused Bool Int Int

type alias Cell = Array Float

type alias Model =
  { cells    : Dict (Int, Int) Cell
  , moused   : Maybe (Int, Int)
  }

squaresWide = 16
squaresHigh = 16

init : (Model, Cmd Msg)
init =
  let
      initCell i j =
        Array.fromList [0, 0, 0]
  in
  ( { cells =
        Dict.fromList (
          List.concatMap (\i ->
              List.concatMap (\j ->
                  [((i,j), initCell i j)]
                )
                (List.range 0 (squaresWide - 1))
            )
            (List.range 0 (squaresHigh - 1))
        )
    , moused = Nothing
    }
  , Cmd.none
  )

boostCell : Time -> Cell -> Cell
boostCell timeStep cell =
  Array.set
    0
    (Maybe.withDefault 0 (Array.get 0 cell) + 5 * timeStep)
    cell

applyMouse : Time -> Model -> Model
applyMouse timeStep model =
  case model.moused of
    Nothing -> model
    Just (i, j) ->
      { model | cells = Dict.update (i, j) (Maybe.map (boostCell timeStep)) model.cells }

average : List Float -> Maybe Float
average floats =
  if List.isEmpty floats
  then Nothing
  else
    let
        (total, length) = List.foldl (\x (t, l) -> (t + x, l + 1)) (0, 0) floats
    in
    Just (total / length)

neighbourCoords : (Int, Int) -> List (Int, Int)
neighbourCoords (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick rawTimeStep ->
      let
          maxSkip = 1
          timeScale = 0.01
          timeStep = timeScale * min rawTimeStep maxSkip
          stepCell (i,j) cell =
            Array.indexedMap (\o x ->
                case Array.get (o + 1) cell of
                  Just dx -> min 1 (max (-1) (x + timeStep * dx))
                  Nothing ->
                    let
                        neighbourValues =
                          List.filterMap
                            (\p ->
                              Dict.get p model.cells
                              |> Maybe.andThen (Array.get 0)
                            )
                            ((i,j) :: (i,j) :: (i,j) :: (i,j) :: neighbourCoords (i,j))
                    in
                    negate (Maybe.withDefault 0 (average neighbourValues))
              )
              cell
      in
      ( applyMouse timeStep { model | cells = Dict.map stepCell model.cells }
      , Cmd.none
      )
    Moused isMoused i j ->
      ( if isMoused
        then { model | moused = Just (i, j) }
        else
          case model.moused of
            Nothing -> model
            Just (oi, oj) ->
              if i == oi && j == oj
              then { model | moused = Nothing }
              else model
      , Cmd.none
      )

view : Model -> Html Msg
view { cells } =
  let
      squareSize = 40

      toColour cell =
        let
            c f = toString (floor (255 * min (max 0 f) 1))
            make r g b =
              "rgb(" ++ c r ++ "," ++ c g ++ "," ++ c b ++ ")"
            value = 0.5 * (Maybe.withDefault 0 (Array.get 0 cell) + 1)
        in
             if value < 0.25 then make value 0 value
        else if value < 0.75 then make value 0 (0.5*(0.75 - value))
        else                      make value (3*(value - 0.75)) 0

      squareFor ((i,j), cell) =
        Svg.rect
          [ Svg.Attributes.width  (toString squareSize)
          , Svg.Attributes.height (toString squareSize)
          , Svg.Attributes.x      (toString (j * squareSize))
          , Svg.Attributes.y      (toString (i * squareSize))
          , Svg.Attributes.fill   (toColour cell)
          , Svg.Events.onMouseOver (Moused True  i j)
          , Svg.Events.onMouseOut  (Moused False i j)
          ]
          []
  in
  Svg.svg
    [ Html.Attributes.width  (squaresWide * squareSize)
    , Html.Attributes.height (squaresHigh * squareSize)
    ]
    (List.map squareFor (Dict.toList cells))

subscriptions : Model -> Sub Msg
subscriptions _ = AnimationFrame.diffs Tick

main =
  Html.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
