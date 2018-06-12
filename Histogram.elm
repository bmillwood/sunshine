module Histogram exposing (main)
{-| Tools for testing the `Sample` module. -}

import Array exposing (Array)
import Maybe
import Random

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes

import Sample

histogram
  :  { min : Float, max : Float, buckets : Int }
  -> Array Float -> Array Int
histogram { min, max, buckets } samples =
  let
      addSample sample histogram =
        let
            index = floor (toFloat buckets * (sample - min) / (max - min))
        in
        Array.set
          index
          (1 + Maybe.withDefault 0 (Array.get index histogram))
          histogram
  in
  Array.foldl
    addSample
    (Array.repeat buckets 0)
    samples

multogram : Array Int -> Array Float
multogram histo =
  let
      maxCount = toFloat (Array.foldl max 0 histo)
  in
  Array.map (\sample -> toFloat sample / maxCount) histo

expSamples : { mean : Float, count : Int } -> Random.Generator (Array Float)
expSamples { mean, count } =
  if count == 1
  then
    Random.map
      (\u ->
        Sample.ofUniform 0 1 u
        |> Sample.toExponential { mean = mean }
        |> Array.repeat 1
      )
      (Random.float 0 1)
  else
    let
        k = floor (toFloat count / 2)
    in
    Random.map2
      Array.append
      (expSamples { mean = mean, count = k })
      (expSamples { mean = mean, count = count - k })

type alias Model = { histo : Array Int, sampleMean : Float }
type Msg = Set Model

numBuckets : Int
numBuckets = 100
numSamples : Int
numSamples = 20000

randogram : Random.Generator Model
randogram =
  Random.map
    (\samples ->
      { histo = histogram { min = 0, max = 10, buckets = numBuckets } samples
      , sampleMean = Array.foldl (+) 0 samples / toFloat (Array.length samples)
      })
    (expSamples { mean = 2, count = numSamples })

init : (Model, Cmd Msg)
init =
  ( { histo = Array.repeat 1 0, sampleMean = 0 }
  , Random.generate Set randogram
  )

update : Msg -> Model -> (Model, Cmd Msg)
update (Set m) _ = (m, Cmd.none)

view : Model -> Html Msg
view { histo, sampleMean } =
  let
      width = 1000
      height = 700
      bucketWidth = width / toFloat numBuckets
      bucket i x =
        Svg.rect
          [ Svg.Attributes.width  (toString bucketWidth)
          , Svg.Attributes.height (toString (x * height))
          , Svg.Attributes.x      (toString (toFloat i * bucketWidth))
          , Svg.Attributes.y      (toString ((1 - x) * height))
          ]
          []
      buckets =
        Array.indexedMap bucket (multogram histo) |> Array.toList
      sampleMeanText =
        Svg.text_
          [ Svg.Attributes.x (toString (width / 2))
          , Svg.Attributes.y "50"
          , Svg.Attributes.width  "50"
          , Svg.Attributes.height "50"
          ]
          [ Svg.text (toString sampleMean) ]
  in
  Svg.svg
    [ Html.Attributes.width  width
    , Html.Attributes.height height
    ]
    (sampleMeanText :: buckets)

main =
  Html.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = \_ -> Sub.none
    }
