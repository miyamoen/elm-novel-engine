module Novel.View exposing (..)


import HtmlTree exposing (..)
import BulmaClasses exposing (..)
import Bulma.HtmlTree exposing (..)


view : InnerNovel a -> List View
view novel =
  case novel of
    Return [] ->
      [ VEnd ]

    Return _ ->
      []

    Text str rest ->
      VText str :: view rest

    Line label str rest ->
      VLine label str :: view rest

    At rest ->
      []

    Choice (Choices choices) rest ->
      VSelect (List.map (Tuple.first) choices)
        |> List.singleton



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

