module Form exposing (..)

import Basics.Extra exposing ((=>))
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Time

import CssBasics exposing (Declaration, toStyleAttribute, CssValue(..), UnitType(..))



main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




type alias Model =
  { scenario : String }


defaultModel : Model
defaultModel =
  { scenario = "Initial Scenario" }

type Msg
  = Input String


init : ( Model, Cmd Msg )
init =
  withNone defaultModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input scenario ->
      { model | scenario = scenario }
        |> withNone


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ scenarioView model
    , inputForm model
    ]


inputForm : Model -> Html Msg
inputForm { scenario } =
  input
    [ placeholder "Input Your Scenario!"
    , value scenario
    , onInput Input
    ]
    []


scenarioView : Model -> Html Msg
scenarioView { scenario } =
  div []
    [ text scenario ]
