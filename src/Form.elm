module Form exposing (..)

import Basics.Extra exposing ((=>))
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Html exposing (..)
import Html.Attributes as Attr exposing (placeholder, value, rows, cols, rel, href, type_, class)
import Html.Events exposing (onInput, onClick)
import Time
import Color
import Debug
import CssBasics exposing (Declaration, toStyleAttribute, CssValue(..), UnitType(..))

import Novel exposing (Novel)
import Novel.Parser as Parser

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




type alias Model =
  { novel : Novel ()
  , screen : String
  , input : String
  , error : String
  }


defaultModel : Model
defaultModel =
  { novel = Novel.return
  , screen = ""
  , input = ""
  , error = ""
  }


type Msg
  = Input String
  | Run
  | Feed


init : ( Model, Cmd Msg )
init =
  withNone defaultModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input input ->
      { model | input = input }
        |> withNone

    Run ->
      case Parser.parse model.input of
        Ok novel ->
          { model | novel = Debug.log "novelの中身" novel, screen = "Click Start!▼" }
            |> withNone

        Err error ->
          { model | error = error }
            |> withNone

    Feed ->
      let
        (head, rest) = Novel.untilAt model.novel
      in
        { model
          | screen = if head == Novel.return then "End" else Novel.toString head
          , novel = rest
        } |> withNone



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


stylesheet : String -> Html msg
stylesheet url =
  node "link"
    [ rel "stylesheet"
    , type_ "text/css"
    , href url
    ]
    []


bulma : Html msg
bulma =
  stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.3.0/css/bulma.min.css"


fontAwesome : Html msg
fontAwesome =
  stylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"


classList : List String -> Attribute msg
classList list =
  list
    |> List.map (flip (,) True)
    |> Attr.classList


view : Model -> Html Msg
view model =
  div []
    [ bulma
    , fontAwesome
    , header
    , messageView model
    , div [ classList [ "tile", "is-ancestor" ] ]
      [ novelView model
      , div
        [ classList [ "tile", "is-6", "is-vertical" ] ]
        [ controlView
        , inputForm model
        ]
      ]
    ]


header : Html msg
header =
  section
    [ classList [ "hero", "is-primary" ] ]
    [ div [ classList [ "hero-head" ]]
      [ nav [ classList [ "nav", "has-shadow" ]]
        [ div [ class "nav-right" ]
          [ a [ class "nav-item", href "https://github.com/miyamoen/elm-novel-engine" ]
            [ span [ class "icon"]
              [ i [ classList [ "fa", "fa-github" ] ] [] ]
            ]
          ]
        ]
      ]
    , div [ class "hero-body has-text-centered" ]
      [ h1 [ class "title" ] [ text "Novel Engine" ] ]
    ]


inputForm : Model -> Html Msg
inputForm { input } =
  div
    [ classList
      [ "tile"
      , "is-parent"
      , "is-vertical"
      , "container"
      , "is-fluid"
      ]
    ]
    [ label [ classList [ "label", "tile", "is-child" ] ] [ text "Your Novel" ]
    , p [ classList [ "control", "tile", "is-child" ] ]
      [ textarea
        [ classList [ "textarea", "is-large", "is-primary" ]
        , placeholder "Input Your Novel!"
        , value input
        ,onInput Input
        ] []
      ]
    ]


controlView : Html Msg
controlView =
  div
    [ classList
      [ "tile"
      , "is-parent"
      ]
    ]
    [ p
      [ classList
        [ "control"
        , "tile"
        , "is-child"
        ]
      ]
      [ a
        [ onClick Run
        , classList
          [ "button"
          , "is-primary"
          , "is-outlined"
          , "is-large"
          ]
        ]
        [ span [ class "icon" ]
          [ i [ classList [ "fa", "fa-play-circle" ] ] [] ]
        , span [] [ text "Run" ]
        ]
      ]
    ]

novelView : Model -> Html Msg
novelView { screen } =
  div
    [ classList
      [ "box"
      , "container"
      , "tile"
      , "is-parent"
      , "is-5"
      ]
    ]
    [ pre
      [ classList
        [ "tile"
        , "is-child"
        ]
      , onClick Feed
      ]
      [ text screen ]
    ]


messageView : Model -> Html msg
messageView { error } =
  div
    [ classList
      [ "box"
      , "container"
      ]
    ]
    [ article
      [ classList
        [ "notification"
        , "is-info"
        ]
      ]
      [ p [] [ text error ] ]
    ]