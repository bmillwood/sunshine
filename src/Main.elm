module Main exposing (main)

import Array
import Dict exposing (Dict)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Svg.Events

import Debug

import Cell.Targeting
import Cell.Integrator
import Cell.Cycle
import Cell.TestCard as Cell exposing (Cell)
import Cell.Template
import Draw
import Help
import Lerp
import Tiling exposing (Tiling)
import Timespan exposing (Timespan)
import Vector exposing (Pt)

type Msg
  = Tick Timespan
  | Moused Bool Pt
  | Cell Pt Cell.Msg
  | Help Help.Msg

type alias Model =
  { cells  : Dict Pt Cell
  , moused : Maybe Pt
  , help   : Help.Model
  }

splitCellsCmds : Dict Pt (Cell, Cmd Cell.Msg) -> (Dict Pt Cell, Cmd Msg)
splitCellsCmds cells =
  ( Dict.map (\_ (cell, _) -> cell) cells
  , List.map (\(pt, (_, cmd)) -> Cmd.map (\r -> Cell pt r) cmd) (Dict.toList cells)
    |> Cmd.batch
  )

init : () -> (Model, Cmd Msg)
init () =
  let
      (cells, cellCmd) =
        Tiling.allCells Cell.tiling Cell.init
        |> splitCellsCmds

      (help, helpCmd) = Help.init
  in
  ( { cells  = cells
    , moused = Nothing
    , help   = help
    }
  , Cmd.batch [cellCmd, Cmd.map Help helpCmd]
  )

applyMouse : Timespan -> Model -> Model
applyMouse timeStep model =
  case model.moused of
    Nothing -> model
    Just (i, j) ->
      { model
      | cells = Dict.update (i, j) (Maybe.map (Cell.boost { timeStep = timeStep })) model.cells
      }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick rawTimeStep ->
      let
          maxSkip = Timespan.fromSeconds 0.1
          timeStep = Timespan.min rawTimeStep maxSkip
          stepCell (i,j) cell =
            Cell.step
              { timeStep = timeStep }
              (\(di, dj) -> Dict.get (i + di, j + dj) model.cells)
              cell
          (newCells, cmd) = splitCellsCmds (Dict.map stepCell model.cells)
      in
      ( applyMouse timeStep { model | cells = newCells }
      , cmd
      )
    Moused isMoused (i, j) ->
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
    Cell pt cellMsg ->
      case Dict.get pt model.cells of
        Nothing -> (model, Cmd.none)
        Just cell ->
          let
              (newCell, cmd) = Cell.msg cellMsg cell
          in
          ( { model | cells = Dict.insert pt newCell model.cells }
          , Cmd.map (\r -> Cell pt r) cmd
          )
    Help helpMsg ->
      let
          (help, helpCmd) = Help.update helpMsg model.help
      in
      ( { model | help = help }
      , Cmd.map Help helpCmd
      )

view : Model -> Html Msg
view { cells, help } =
  let
      events (i, j) =
        [ Svg.Events.onMouseOver (Moused True  (i, j))
        , Svg.Events.onMouseOut  (Moused False (i, j))
        ]
  in
  Html.div
    []
    [ Draw.draw
        Cell.tiling
        (Dict.map (\_ c -> Cell.value c) cells)
        { events = events }
    , Html.map Help (Help.view help)
    ]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Browser.Events.onAnimationFrameDelta
    (\millis -> Tick (Timespan.fromMilliseconds millis))

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
