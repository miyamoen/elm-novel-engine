module Form exposing (..)

import Response exposing (withCmd, withNone)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug
import HtmlTree exposing (..)
import HtmlTree.Modify exposing (..)
import BulmaClasses exposing (..)

import View exposing (..)
import ViewTree exposing (..)
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
  { novel : Novel.Model ()
  , script : String
  , error : String
  }


defaultModel : Model
defaultModel =
  { novel = Novel.init Novel.return
  , script = ""
  , error = ""
  }


type Msg
  = NovelMsg Novel.Msg
  | Parse
  | Input String


init : ( Model, Cmd Msg )
init =
  withNone defaultModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NovelMsg subMsg ->
      { model | novel = Novel.update subMsg model.novel }
        |> withNone

    Input input ->
      { model | script = input }
        |> withNone

    Parse ->
      case Parser.parse model.script of
        Ok novel ->
          { model | novel = Novel.init (Debug.log "novelの中身" novel) }
            |> withNone

        Err error ->
          { model | error = error }
            |> withNone



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



novelInputCard : String -> HtmlTree Msg
novelInputCard text =
  View.novelInputCard text
    |> matchingTag "textarea" (withObserver (onInput Input))
    |> matchingTag "a" (addAction ("click", Parse))


novelCard : String -> Novel a -> HtmlTree Msg
novelCard label novel =
  View.novelCard "Novel" (Novel.htmlTree novel)
    |> addAction ("click", NovelMsg Novel.Feed)





view : Model -> Html Msg
view model =
  let
    inputTile =
      novelInputCard model.script
        |> appendParent featureTree
        |> addClass feature.sizing.isFluid
        |> appendParent tileChildTree
        |> appendParent tileParentTree
        |> addClass tile.width.is6

    novelTile =
      novelCard "Novel" model.novel.rest
        |> appendParent featureTree
        |> addClass feature.sizing.isFluid
        |> appendParent tileChildTree
        |> appendParent tileParentTree
        |> addClass tile.width.is6
  in
    tileAncestorTree
      |> appendChild inputTile
      |> appendChild novelTile
      |> List.singleton
      |> container "div"
      |> prependChild heroHeader
      |> prependChild cdnTree
      |> assembleHtml
