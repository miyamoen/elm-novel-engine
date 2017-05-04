module Novel exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Return exposing (singleton)

import Novel.Script as Script exposing (Script, Out(..), Label)


type alias Scene = String


main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


type Action
  = Feed
  | Choice Int
  -- Game画面的なactionの処理を入れるつもり
  | NoOp


type alias Novel state msg =
  { scripts : Dict Scene (Script msg Action)
  , label : Scene
  , cursor : Script msg Action

  , state : state
  , nextScene : state -> Scene -> Scene
  , update : msg -> state -> state

  , lines : List (Label, String)
  -- Game画面的な設定
  }


type NovelView
  = TextList (List (Label, String))
  | ChoiceList (List (String, Action))


update : Action -> Novel state msg -> (Novel state msg, Cmd Action)
update msg novel =
  case msg of
    Feed ->
      let
        (next, out) = Script.feed novel.cursor
      in
        { novel | cursor = next }
          |> updateWithScriptOut out

    Choice num ->
      let
        (next, out) = Script.choice num novel.cursor
      in
        { novel | cursor = next }
          |> updateWithScriptOut out

    NoOp ->
      novel
        |> singleton


updateWithScriptOut : Script.Out msg Action -> Novel state msg -> (Novel state msg, Cmd Action)
updateWithScriptOut out novel =
  case out of
    OutText lines ->
      { novel | lines = lines }
        |> singleton

    OutChoices lines ->
      { novel | lines = lines }
        |> singleton

    OutActs acts ->
      singleton novel |>
        (acts
        |> List.map (\act -> Return.andThen (update act))
        |> Return.pipel)

    OutMsg maybeMsg ->
      let
        newState =
          maybeMsg
            |> Maybe.map (flip novel.update novel.state)
            |> Maybe.withDefault novel.state

        label =
          novel.nextScene newState novel.label

        next =
          case Dict.get label novel.scripts of
            Just script ->
              script

            Nothing ->
              Debug.crash "次の話が見つからないんですけどー"

        out =
          Script.out next
      in
        { novel
        | cursor = next
        , label = label
        , state = newState
        } |> updateWithScriptOut out
