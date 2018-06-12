module Help exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type Model = M (List (Html Msg))

type Msg = Set Model

moreHelp : Model -> String -> Html Msg
moreHelp nextHelp text =
  Html.a
  [ Html.Attributes.id "moreHelp"
  , Html.Attributes.href "#"
  , Html.Events.onClick (Set nextHelp)
  ]
  [ Html.text text ]

init : (Model, Cmd Msg)
init =
  let
      br = Html.br [] []
      line1 = Html.text "mouse over people to hang out with them"

      help1 =
        [ line1
        , br, moreHelp help2 "huh?"
        ] |> M

      help2 =
        [ line1
        , br, Html.text " and make them less gloomy"
        , br, moreHelp dismissed "ok."
        ] |> M

      dismissed = M []
  in
  ( M [ moreHelp help1 "what?" ]
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update (Set m) _ = (m, Cmd.none)

view : Model -> Html Msg
view (M lines) =
  Html.p
    [ Html.Attributes.id "help" ]
    lines
