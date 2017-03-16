module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild)
import View exposing (appendParent)

type alias Label = String


type InnerNovel a
  = Text String (InnerNovel a)
  | Line Label String (InnerNovel a)
  | At (InnerNovel a)
  | Choice (List String) (Char -> InnerNovel a)
  | Return a


type Msg
  = Feed
  | Input Char
  | Restart


type alias Novel a =
  { main : Maybe (InnerNovel a)
  , rest : Maybe (InnerNovel a)
  , sections : Dict String (InnerNovel a)
  }


init : InnerNovel a -> Novel a
init novel =
  { main = Just novel
  , rest = Just novel
  , sections = Dict.empty
  }


update : Msg -> Novel a -> Novel a
update msg model =
  case msg of
    Feed ->
      { model | rest = model.rest |> Maybe.map feed }

    Restart ->
      { model | rest = model.main }

    Input char ->
      { model | rest = model.rest |> Maybe.map (branch char) }


feed : InnerNovel a -> InnerNovel a
feed novel =
  case novel of
    Text _ rest ->
      feed rest

    Line _ _ rest ->
      feed rest

    At rest ->
      rest

    _ ->
      novel


branch : Char -> InnerNovel a -> InnerNovel a
branch input novel =
  case novel of
    Choice _ restF ->
      restF input

    _ ->
      novel


novelFromList : List (String, InnerNovel a) -> Novel a
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


text : String -> InnerNovel ()
text str =
  Text str return


line : Label -> String -> InnerNovel ()
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


at : InnerNovel ()
at =
  At return


choice : List String -> InnerNovel ()
choice choices =
  Choice choices <| always return


return : InnerNovel ()
return =
  Return ()


andThen : (a -> InnerNovel b) -> InnerNovel a -> InnerNovel b
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

    Return a ->
      func a


append : InnerNovel a -> InnerNovel a -> InnerNovel a
append left right =
  andThen (\_ -> right) left


andAppend : InnerNovel a -> InnerNovel a -> InnerNovel a
andAppend =
  flip append


concat : List (InnerNovel a) -> Maybe (InnerNovel a)
concat novels =
  case novels of
    [] ->
      Nothing

    head :: rest ->
      List.foldl (\right left -> andThen (\_ -> right) left) head rest
        |> Just


type View
  = VText String
  | VLine Label String
  | VEnd


view : InnerNovel a -> List View
view novel =
  case novel of
    Return a ->
      [ VEnd ]

    Text str rest ->
      VText str :: view rest

    Line label str rest ->
      VLine label str :: view rest

    At rest ->
      []

    Choice choices junction ->
      List.map VText choices



htmlTree : Maybe (InnerNovel a) -> HtmlTree msg
htmlTree maybeNovel =
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
    case maybeNovel of
      Just novel ->
        view novel
          |> List.map tagger
          |> container "div"
          |> addClass "content"

      Nothing ->
        textWrapper "p" "Nothing"
          |> appendParent (leaf "div")
          |> addClass "content"

