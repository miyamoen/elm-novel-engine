module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild, addAction)
import Tuple
import List.Extra exposing (getAt)
type alias Label = String


type InnerNovel msg
  = Text String (InnerNovel msg)
  | Line Label String (InnerNovel msg)
  | At (InnerNovel msg)
  | Choice (Choices msg) (InnerNovel msg)
  | Return (List msg)


type Choices msg =
  Choices (List (String, List msg))


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


feed : Novel -> Novel
feed model1 =
  let
    model2 =
      { model1 | rest = feed_ model1.rest }
  in
    case getMsg model2 of
      [] ->
        model2

      msgs ->
        List.foldl (\msg model_ -> update msg model_) model2 msgs


feed_ : InnerNovel a -> InnerNovel a
feed_ novel =
  case novel of
    Text _ rest ->
      feed_ rest

    Line _ _ rest ->
      feed_ rest

    At rest ->
      rest

    _ ->
      novel


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


text : String -> InnerNovel a
text str =
  Text str return


line : Label -> String -> InnerNovel a
line label str =
  Line label str return


labeled : Label -> InnerNovel a -> InnerNovel a
labeled label novel =
  case (Debug.log "daypo" novel) of
    Text str rest ->
      Line label str <| labeled label rest

    At rest ->
      At <| labeled label rest

    Return a ->
      Return a

    _ ->
      novel


at : InnerNovel a
at =
  At return


choice : List (String, List a) -> InnerNovel a
choice choices =
  Choice (Choices choices) return


return : InnerNovel a
return =
  Return []


andThen : (a -> b) -> (List a -> InnerNovel b) -> InnerNovel a -> InnerNovel b
andThen tagger func novel =
  case novel of
    Text text next ->
      Text text <| andThen tagger func next

    Line label str next ->
      Line label str <| andThen tagger func next

    At next ->
      At <| andThen tagger func next

    Choice choices next ->
      Choice (mapChoices tagger choices) <| andThen tagger func next

    Return msgs ->
      func msgs


mapChoices : (a -> b) -> Choices a -> Choices b
mapChoices tagger (Choices choices) =
  choices
    |> List.map (Tuple.mapSecond (List.map tagger))
    |> Choices




append : InnerNovel a -> InnerNovel a -> InnerNovel a
append for back =
  for
    |> andThen identity (\forMsgs ->
      back
        |> andThen identity (\backMsgs ->
          Return <| forMsgs ++ backMsgs
        )
    )


andAppend : InnerNovel a -> InnerNovel a -> InnerNovel a
andAppend =
  flip append


concat : List (InnerNovel a) -> InnerNovel a
concat =
  List.foldl andAppend return



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

