module Help exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type Model
  = NoneYet
  | Help1
  | Help2
  | Dismissed

type Msg = Set Model

init : (Model, Cmd Msg)
init = (NoneYet, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update (Set m) _ = (m, Cmd.none)

view : Model -> Html Msg
view model =
  let
      moreHelp nextHelp text =
        Html.a
          [ Html.Attributes.id "moreHelp"
          , Html.Attributes.href "#"
          , Html.Events.onClick (Set nextHelp)
          ]
          [ Html.text text ]

      paragraphs =
        case model of
          NoneYet ->
            [ moreHelp Help1 "what?" ]
          Help1 ->
            [ Html.text "mouse over people to hang out with them"
            , Html.br [] []
            , moreHelp Help2 "huh?"
            ]
          Help2 ->
            [ Html.text "mouse over people to hang out with them"
            , Html.br [] []
            , Html.text " and make them less gloomy"
            , Html.br [] []
            , moreHelp Dismissed "ok."
            ]
          Dismissed ->
            []
  in
  Html.p
    [ Html.Attributes.id "help" ]
    paragraphs
