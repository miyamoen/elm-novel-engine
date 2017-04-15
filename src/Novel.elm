module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild, addAction)
import Tuple
import List.Extra exposing (getAt)
import Html exposing (Html)
import Return exposing (singleton)

import Novel.Script as Script exposing (Script, Out(..), Label)


main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


type Msg
  = Feed
  | GetScriptOut
  | Restart
  | NoOp


type ScriptAction
  = NoAct


type alias Novel flg =
  { revHistory : List (Script flg Msg)
  , cursor : Script flg Msg
  , sections : Dict String (Script flg Msg)
  , view : NovelView
  , flags : List flg
  }


type NovelView
  = TextList (List (Label, String))
  | ChoiceList (List (String, Msg))


update : Msg -> Novel flg -> (Novel flg, Cmd Msg)
update msg novel =
  case msg of
    Feed ->
      { novel | cursor = Script.feed novel.cursor }
        |> singleton

    GetScriptOut ->
      execScriptOut novel

    Restart ->
      { novel
        | cursor = List.head novel.revHistory
          |> Maybe.withDefault Script.return_
      } |> singleton

    -- Jump section ->
    --   { novel | rest =
    --     Dict.get section novel.sections
    --       |> Maybe.withDefault return
    --       -- |> andAppend novel.rest
    --   }

    NoOp ->
      novel
        |> singleton


execScriptOut : Novel flg -> (Novel flg, Cmd Msg)
execScriptOut novel =
  case Script.out novel.cursor of
    OutText texts ->
      novel
        |> setView (TextList texts)
        |> singleton

    OutActs msgs ->
      singleton novel |>
        (msgs
        |> List.map (\msg -> Return.andThen (update msg))
        |> Return.pipel)

    OutChoice choices ->
      novel
        |> setView (ChoiceList choices)
        |> singleton

    OutFlgs flgs ->
      { novel | flags = List.append novel.flags flgs }
        |> singleton


setView : NovelView -> Novel flg -> Novel flg
setView view novel =
  { novel | view = view }