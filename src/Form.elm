module Form exposing (..)

import Basics.Extra exposing ((=>))
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, rows, cols)
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


view : Model -> Html Msg
view model =
  div []
    [ errorView model
    , novelView model
    , inputForm model
    , runButton
    ]


inputForm : Model -> Html Msg
inputForm { input } =
  textarea
    [ placeholder "Input Your Novel!"
    , value input
    , onInput Input
    , rows 10
    , cols 50
    ]
    []


runButton : Html Msg
runButton =
  button
    [ onClick Run ]
    [ text "Run" ]


novelView : Model -> Html Msg
novelView { screen } =
  pre
    [ toStyleAttribute stringViewStyle
    , onClick Feed
    ]
    [ text screen ]


stringViewStyle : List (Declaration number)
stringViewStyle =
  [ "width" => Unit 50 Em
  , "height" => Unit 20 Em
  , "background-color" => (Col Color.lightGrey)
  ]


errorView : Model -> Html msg
errorView { error } =
  div
    []
    [ text error ]