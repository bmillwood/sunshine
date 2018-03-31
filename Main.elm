module Main exposing (main)

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

import Cell exposing (Cell)

type Msg =
    Tick Time
  | Moused Bool Int Int

type alias Model =
  { cells    : Dict (Int, Int) Cell
  , moused   : Maybe (Int, Int)
  }

squaresWide = 16
squaresHigh = 16

init : (Model, Cmd Msg)
init =
  ( { cells =
        Dict.fromList (
          List.concatMap (\i ->
              List.concatMap (\j ->
                  [((i,j), Cell.init i j)]
                )
                (List.range 0 (squaresWide - 1))
            )
            (List.range 0 (squaresHigh - 1))
        )
    , moused = Nothing
    }
  , Cmd.none
  )

applyMouse : Time -> Model -> Model
applyMouse timeStep model =
  case model.moused of
    Nothing -> model
    Just (i, j) ->
      { model | cells = Dict.update (i, j) (Maybe.map (Cell.boost timeStep)) model.cells }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick rawTimeStep ->
      let
          maxSkip = 1
          timeScale = 0.01
          timeStep = timeScale * min rawTimeStep maxSkip
          stepCell (i,j) cell =
            Cell.step
              { timeStep = timeStep }
              (\(di, dj) -> Dict.get (i + di, j + dj) model.cells)
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
            value = Cell.value cell
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
