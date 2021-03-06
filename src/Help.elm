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

      help0 =
        [ moreHelp help1 "huh?"
        ] |> M

      help1 =
        [ line1
        , br, moreHelp dismissed "oh ok."
        , br, moreHelp help2 "I don't get it"
        , br, moreHelp tech "tech?"
        ] |> M

      help2 =
        [ line1
        , br, Html.text "and make them less gloomy"
        , br, moreHelp dismissed "ok."
        , br, moreHelp tech "tech?"
        ] |> M

      tech =
        [ Html.text "powered by "
        , Html.a
            [ Html.Attributes.href "https://elm-lang.org" ]
            [ Html.text "elm" ]
        , Html.text "; code available on "
        , Html.a
            [ Html.Attributes.href "https://github.com/bmillwood/sunshine" ]
            [ Html.text "github" ]
        , br, moreHelp dismissed "ok."
        ] |> M

      dismissed = M []
  in
  ( help0
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update (Set m) _ = (m, Cmd.none)

view : Model -> Html Msg
view (M lines) =
  Html.p
    [ Html.Attributes.id "help" ]
    lines
