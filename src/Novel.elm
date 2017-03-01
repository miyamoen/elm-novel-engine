module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild)

type alias Label = String


type Novel a
  = Text String (Novel a)
  | Line Label String (Novel a)
  | At (Novel a)
  | Choice (List String) (Char -> Novel a)
  | Return a
  | Moduled (Dict String (Novel a)) (Novel a)


type Msg
  = Feed
  | Input Char
  | Restart


type alias Model a =
  { novel : Novel a
  , rest : Novel a
  }


init : Novel () -> Model ()
init novel =
  { novel = novel
  , rest = Text "Click Start" (At novel)
  }


update : Msg -> Model a -> Model a
update msg model =
  case msg of
    Feed ->
      { model | rest = feed  model.rest }

    Restart ->
      { model | rest = model.novel }

    Input char ->
      { model | rest = branch char model.rest }


feed : Novel a -> Novel a
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


branch : Char -> Novel a -> Novel a
branch input novel =
  case novel of
    Choice _ restF ->
      restF input

    _ ->
      novel


text : String -> Novel ()
text str =
  Text str return


line : Label -> String -> Novel ()
line label str =
  Line label str return


labeled : Label -> Novel () -> Novel ()
labeled label novel =
  case novel of
    Text str rest ->
      Line label str <| labeled label rest

    At rest ->
      At <| labeled label rest

    Return a ->
      Return a

    _ ->
      Debug.crash "Crash!"


at : Novel ()
at =
  At return


choice : List String -> Novel ()
choice choices =
  Choice choices <| always return


return : Novel ()
return =
  Return ()


andThen : (a -> Novel a) -> Novel a -> Novel a
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

    Return val ->
      func val

    Moduled novels main ->
      andThen func main
        |> Moduled novels


append : Novel a -> Novel a -> Novel a
append left right =
  andThen (\_ -> right) left


andEnd : Novel a -> Novel a -> Novel a
andEnd =
  flip append


concat : List (Novel ()) -> Novel ()
concat =
  -- List.Extra.foldl1 append
  List.foldl (\right left -> andThen (\_ -> right) left) return


type View
  = VText String
  | VLine Label String
  | VEnd


view : Novel a -> List View
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

    Moduled _ main ->
      view main


htmlTree : Novel a -> HtmlTree msg
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


-- sample : Novel ()
-- sample =
--   text "1行目"
--     |> append (line "わたし" "ばかめ")
--     |> append (Choice [ "1c", "2c", "3c" ])