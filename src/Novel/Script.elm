module Novel.Script exposing (..)

import Html exposing (Html)
import List.Extra exposing (getAt)

main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


(=>) : a -> b -> (a, b)
(=>) = (,)


type alias Label = String


type Script msg act
  = Text String (Script msg act)
  | Labeled Label String (Script msg act)
  | Wait (Script msg act)
  | Call act (Script msg act)
  | Choices (List (String, Script msg act))
  | Return (Maybe msg)


-- Constructor

return : msg -> Script msg act
return msg =
  Return <| Just msg


return_ : Script msg act
return_ =
  Return Nothing


text : String -> Script msg act
text str =
  Text str return_


labeled : Label -> String -> Script msg act
labeled label str =
  Labeled label str return_


wait : Script msg act
wait =
  Wait return_


call : act -> Script msg act
call act =
  Call act return_


choices : List (String , Script msg act) -> Script msg act
choices choices =
  Choices choices


-- monad?

andThen : (Maybe msg -> Script msg_ act) -> Script msg act -> Script msg_ act
andThen tagger script =
  case script of
    Text str next ->
      Text str <| andThen tagger next

    Labeled label str next ->
      Labeled label str <| andThen tagger next

    Wait next ->
      Wait <| andThen tagger next

    Call act next ->
      Call act <| andThen tagger next

    Choices choices ->
      Choices (List.map (\(label, next) -> (label, andThen tagger next)) choices)

    Return msgs ->
      tagger msgs


append : Script msg act -> Script msg act -> Script msg act
append a b =
  a |> andThen (always b)


andAppend : Script msg act -> Script msg act -> Script msg act
andAppend =
  flip append


concat : List (Script msg act) -> Script msg act
concat =
  List.foldl andAppend return_


-- Out

type Out msg act
  = OutLines (List Line)
  | OutActs (List act)
  | OutMsg (Maybe msg)


type Line
  = TextList (List String)
  | LabeledList Label (List String)
  | ChoiceList (List String)


out : Script msg act -> Out msg act
out script =
  case script of
    Return msg ->
      OutMsg msg

    Call act next ->
      outWithActs [ act ] next

    Choices choices ->
      choices
        |> List.map Tuple.first
        |> ChoiceList
        |> List.singleton
        |> OutLines

    Text str next ->
      outWithText [] (TextList [ str ]) next

    Labeled label str next ->
      outWithText [] (LabeledList label [ str ]) next

    Wait _ ->
      OutLines []


outWithText : List Line -> Line -> Script msg act -> Out msg act
outWithText acc line script =
  case (script, line) of
    (Text str next, TextList texts) ->
      outWithText acc (TextList (str :: texts)) next

    (Text str next, _) ->
      outWithText (line :: acc) (TextList [ str ]) next

    (Labeled label str next, LabeledList label_ texts) ->
      if label == label_ then
        outWithText acc (LabeledList label (str :: texts)) next
      else
        outWithText (line :: acc) (LabeledList label [ str ]) next

    (Labeled label str next, _) ->
      outWithText (line :: acc) (LabeledList label [ str ]) next

    _ ->
      line :: acc
        |> List.reverse
        |> OutLines


outWithActs : List act -> Script msg act -> Out msg act
outWithActs acc script =
  case script of
    Call act next ->
      outWithActs (act :: acc) next

    _ ->
      OutActs <| List.reverse acc


-- operation

feed : Script msg act -> (Script msg act, Out msg act)
feed script =
  (case script of
    Text _ next ->
      feedText next

    Labeled _ _ next ->
      feedText next

    Wait next ->
      next

    Call _ next ->
      feedCall next

    Choices choices ->
      script

    Return _ ->
      script
  ) |> (\new -> new => out new)


feedText : Script msg act -> Script msg act
feedText script =
  case script of
    Text _ next ->
      feedText next

    Labeled _ _ next ->
      feedText next

    Wait next ->
      next

    _ ->
      script


feedCall : Script msg act -> Script msg act
feedCall script =
  case script of
    Call _ next ->
      feedCall next

    _ ->
      script


choice : Int -> Script msg act -> (Script msg act, Out msg act)
choice index script =
  (case script of
    Choices choices ->
      getAt index choices
        |> Maybe.map Tuple.second
        |> Maybe.withDefault script

    _ ->
      script
  ) |> (\new -> new => out new)
