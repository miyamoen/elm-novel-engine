module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild)

type alias Label = String


type InnerNovel a
  = Text String (InnerNovel a)
  | Line Label String (InnerNovel a)
  | At (InnerNovel a)
  | Choice (List String) (Char -> InnerNovel a)
  | Return (List a)


type Msg
  = Feed
  | Input Char
  | Restart
  | Jump String
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

    Input char ->
      { model | rest = branch char model.rest }

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


branch : Char -> InnerNovel a -> InnerNovel a
branch input novel =
  case novel of
    Choice _ restF ->
      restF input

    _ ->
      novel


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


choice : List String -> InnerNovel a
choice choices =
  Choice choices <| always return


return : InnerNovel a
return =
  Return []


andThen : (List a -> InnerNovel b) -> InnerNovel a -> InnerNovel b
andThen func novel =
  case novel of
    Text text next ->
      Text text <| andThen func next

    Line label str next ->
      Line label str <| andThen func next

    At next ->
      At <| andThen func next

    Choice choices junction ->
      Choice choices <| (\char -> junction char |> andThen func)

    Return msgs ->
      func msgs



append : InnerNovel a -> InnerNovel a -> InnerNovel a
append for back =
  for
    |> andThen (\forMsgs ->
      back
        |> andThen (\backMsgs ->
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

    Choice choices junction ->
      List.map VText choices



htmlTree : InnerNovel a -> HtmlTree msg
htmlTree novel =
  let
    tagger v =
      case v of
        VText str ->
          textWrapper "p" str

        VLine label str ->
          textWrapper "p" ("[" ++ label ++ "]" ++ str)

        VEnd ->
          textWrapper "p" "End"
  in
    view novel
      |> List.map tagger
      |> container "div"
      |> addClass "content"

