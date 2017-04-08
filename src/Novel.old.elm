module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild, addAction)
import Tuple
import List.Extra exposing (getAt)



type Msg
  = Feed
  | Restart
  | Jump String
  | Select Int
  | NoOp


type alias Novel =
  { main : InnerNovel Msg
  , rest : InnerNovel Msg
  , sections : Dict String (InnerNovel Msg)
  }


init : InnerNovel Msg -> Novel
init novel =
  { main = novel
  , rest = novel
  , sections = Dict.empty
  }


update : Msg -> Novel -> Novel
update msg model =
  case msg of
    Feed ->
      feed model

    Restart ->
      { model | rest = model.main }

    Select num ->
      select num model

    Jump section ->
      { model | rest =
        Dict.get section model.sections
          |> Maybe.withDefault return
          -- |> andAppend model.rest
      }

    NoOp ->
      model





getMsg : Novel -> List Msg
getMsg model =
  case model.rest of
    Return msgs ->
      msgs

    _ ->
      []


select : Int -> Novel -> Novel
select num model =
  let
    getMsg choices =
      getAt num choices
        |> Maybe.map Tuple.second
        |> Maybe.withDefault []
  in
    case model.rest of
      Choice (Choices choices) rest ->
        List.foldl
          (\msg model_ -> update msg model_)
          { model | rest = rest }
          (getMsg choices)

      _ ->
        model


novelFromList : List (String, InnerNovel Msg) -> Novel
novelFromList novels =
  let
    main =
      List.filter (Tuple.first >> (==) "main") novels
        |> List.map Tuple.second
        |> concat

    sections =
      List.filter (Tuple.first >> (/=) "main") novels
        |> Dict.fromList
  in
    { main = main
    , rest = main
    , sections = sections
    }

type View
  = VText String
  | VLine Label String
  | VSelect (List String)
  | VEnd


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

