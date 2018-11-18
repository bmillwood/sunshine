module Tiling exposing
  ( Tiling (..)
  , Local (..)
  , local
  , adjacent
  , diagonal
  , allCells
  , bounds
  )

import Dict exposing (Dict)

import Vector exposing (Pt)

type Tiling
  = SquareSquare
  | SquareHex
  | HexHex

type Local
  = Square
  | Hex

local : Tiling -> Local
local t =
  case t of
    SquareSquare -> Square
    SquareHex -> Hex
    HexHex -> Hex

adjacent : Local -> List Pt
adjacent t =
  case t of
    Square ->
      [ ( 0, -1)
      , ( 0,  1)
      , (-1,  0)
      , ( 1,  0)
      ]
    Hex ->
      [ (-2,  0)
      , (-1, -1)
      , (-1,  1)
      , ( 1, -1)
      , ( 1,  1)
      , ( 2,  0)
      ]

diagonal : Local -> List Pt
diagonal t =
  case t of
    Square ->
      [ (-1, -1)
      , (-1,  1)
      , ( 1, -1)
      , ( 1,  1)
      ]
    Hex ->
      []

squareSize = 13

hexSide = 8

forIJ : List Int -> (Int -> List Int) -> (Int -> Int -> a) -> List a
forIJ iRange jRange f =
  List.concatMap (\i ->
      List.map (\j -> f i j) (jRange i)
    )
    iRange

forSquare : (Pt -> Pt) -> Dict Pt ()
forSquare f =
  forIJ
    (List.range 0 (squareSize - 1))
    (\_ -> List.range 0 (squareSize - 1))
    (\i j -> (f (i, j), ()))
  |> Dict.fromList

allCellsUnit : Tiling -> Dict Pt ()
allCellsUnit t =
  case t of
    SquareSquare ->
      forSquare (\pt -> pt)
    SquareHex ->
      forSquare (\(i, j) -> (i, j * 2 + modBy 2 i))
    HexHex ->
      let
          growing =
            forIJ
              (List.range 0 (hexSide - 1))
              (\i -> List.range 0 (hexSide - 1 + i))
              (\i j -> (i, hexSide - 1 - i + 2 * j))
          shrinking =
            forIJ
              (List.range 1 (hexSide - 1))
              (\i -> List.range 0 (2 * (hexSide - 1) - i))
              (\i j -> (hexSide - 1 + i, i + 2 * j))
      in
      List.append growing shrinking
      |> List.map (\pt -> (pt, ()))
      |> Dict.fromList

allCells : Tiling -> (Pt -> cell) -> Dict Pt cell
allCells t f = Dict.map (\k () -> f k) (allCellsUnit t)

bounds : Tiling -> { min : Pt, max : Pt }
bounds t =
  Dict.foldl
    (\ (i, j) () a ->
      case a of
        Nothing -> Just { min = (i, j), max = (i, j) }
        Just old ->
          let
              (lowI, lowJ) = old.min
              (hiI, hiJ) = old.max
          in
          Just
            { min = (min lowI i, min lowJ j)
            , max = (max hiI i, max hiJ j)
            })
    Nothing
    (allCellsUnit t)
  |> Maybe.withDefault { min = (0, 0), max = (0, 0) } {- unreachable -}
