module Novel.View exposing (..)

import Html exposing (Html)
import HtmlTree exposing (..)
import BulmaClasses exposing (..)
import Bulma.HtmlTree exposing (..)

import Novel exposing (..)


main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


htmlTree : InnerNovel Msg -> HtmlTree Msg
htmlTree novel =
  let
    tagger v =
      case v of
        VText str ->
          textWrapper "p" str

        VLine label str ->
          textWrapper "p" ("[" ++ label ++ "]" ++ str)

        VSelect choices ->
          choices
            |> List.map (VText >> tagger)
            |> List.indexedMap (\num p -> addAction ("click", Select num) p)
            |> container "div"

        VEnd ->
          textWrapper "p" "End"
  in
    view novel
      |> List.map tagger
      |> container "div"
      |> addClass "content"

