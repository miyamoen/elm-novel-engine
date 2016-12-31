module Form exposing (..)

import Basics.Extra exposing ((=>))
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)
import Time
import Color
import Debug
import CssBasics exposing (Declaration, toStyleAttribute, CssValue(..), UnitType(..))
import Combine exposing (parse)
import Novel exposing (Novel)


main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




type alias Model =
  { novel : Novel
  , input : String
  }


defaultModel : Model
defaultModel =
  { novel = Novel.empty
  , input = ""
  }


type Msg
  = Input String
  | Run


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
      case parse Novel.parser model.input of
        Ok (_, _, novel) ->
          { model | novel = novel }
            |> withNone

        _ ->
          withNone model


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ novelView model
    , inputForm model
    , runButton
    ]


inputForm : Model -> Html Msg
inputForm { input } =
  Html.input
    [ placeholder "Input Your Novel!"
    , value input
    , onInput Input
    ]
    []


runButton : Html Msg
runButton =
  button
    [ onClick Run ]
    [ text "Run" ]


novelView : Model -> Html Msg
novelView { novel } =
  div [ toStyleAttribute stringViewStyle ]
    [ text (toString novel) ]


stringViewStyle : List (Declaration number)
stringViewStyle =
  [ "width" => Unit 50 Em
  , "height" => Unit 20 Em
  , "background-color" => (Col Color.lightGrey)
  ]
