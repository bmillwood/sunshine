module Main exposing (main)

import Array
import Dict exposing (Dict)
import Time exposing (Time)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Svg.Events

import Cell.Targeting
import Cell.Integrator
import Cell.Cycle as Cell exposing (Cell)
import Cell.Template
import Help
import Lerp
import Vector exposing (Pt)

type Msg
  = Tick Time
  | Moused Bool Pt
  | Cell Pt Cell.Msg
  | Help Help.Msg

type alias Model =
  { cells  : Dict Pt Cell
  , moused : Maybe Pt
  , help   : Help.Model
  }

squaresWide = 13
squaresHigh = 13

splitCellsCmds : Dict Pt (Cell, Cmd Cell.Msg) -> (Dict Pt Cell, Cmd Msg)
splitCellsCmds cells =
  ( Dict.map (\_ (cell, _) -> cell) cells
  , List.map (\(pt, (_, cmd)) -> Cmd.map (\r -> Cell pt r) cmd) (Dict.toList cells)
    |> Cmd.batch
  )

init : (Model, Cmd Msg)
init =
  let
      (cells, cellCmd) =
        List.concatMap (\i ->
            List.concatMap (\j ->
                [((i,j), Cell.init (i, j))]
              )
              (List.range 0 (squaresWide - 1))
          )
          (List.range 0 (squaresHigh - 1))
        |> Dict.fromList
        |> splitCellsCmds

      (help, helpCmd) = Help.init
  in
  ( { cells  = cells
    , moused = Nothing
    , help   = help
    }
  , Cmd.batch [cellCmd, Cmd.map Help helpCmd]
  )

applyMouse : Time -> Model -> Model
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
          maxSkip = 0.1 * Time.second
          timeStep = min rawTimeStep maxSkip
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
      squareSize = 40

      colours =
        [ (0.00, Vector.Vec3 0.00 0.00 0.00)
        , (0.20, Vector.Vec3 0.25 0.00 0.25)
        , (0.60, Vector.Vec3 0.75 0.00 0.00)
        , (0.80, Vector.Vec3 1.00 1.00 0.00)
        , (0.85, Vector.Vec3 1.00 1.00 1.00)
        , (0.90, Vector.Vec3 1.00 1.00 1.00)
        , (1.00, Vector.Vec3 0.00 1.00 1.00)
        ] |> Array.fromList

      toColour cell =
        let
            c f = toString (floor (255 * min (max 0 f) 1))
            make (Vector.Vec3 r g b) =
              "rgb(" ++ c r ++ "," ++ c g ++ "," ++ c b ++ ")"
            value = Cell.value cell
        in
        -- Linearly interpolating RGB values is incorrect for interpolating
        -- colours. Should be fine for the time being though.
        Lerp.at (Vector.vec3 Vector.float) colours value
        |> Maybe.withDefault (Vector.Vec3 0 0 0)
        |> make

      squareFor ((i,j), cell) =
        Svg.rect
          [ Svg.Attributes.width  (toString squareSize)
          , Svg.Attributes.height (toString squareSize)
          , Svg.Attributes.x      (toString (j * squareSize))
          , Svg.Attributes.y      (toString (i * squareSize))
          , Svg.Attributes.fill   (toColour cell)
          , Svg.Events.onMouseOver (Moused True  (i, j))
          , Svg.Events.onMouseOut  (Moused False (i, j))
          ]
          []
  in
  Html.div
    []
    [ Svg.svg
        [ Html.Attributes.width  (squaresWide * squareSize)
        , Html.Attributes.height (squaresHigh * squareSize)
        ]
        (List.map squareFor (Dict.toList cells))
    , Html.map Help (Help.view help)
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = AnimationFrame.diffs Tick

main =
  Html.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
