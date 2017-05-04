module Novel.Script exposing (..)

import Html exposing (Html)
import List.Extra exposing (getAt)

main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


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
  = OutText (List (Label, String))
  | OutChoices (List (Label, String))
  | OutActs (List act)
  | OutMsg (Maybe msg)


out : Script msg act -> Out msg act
out script =
  case script of
    Return msg ->
      OutMsg msg

    Call act next ->
      outActs [ act ] next

    Choices choices ->
      choices
        |> List.map Tuple.first
        |> List.map ((,) "")
        |> OutChoices

    Text str next ->
      outText [ ("", str) ] next

    Labeled label str next ->
      outText [ (label, str) ] next

    Wait _ ->
      OutText []


outText : List (Label, String) -> Script msg act -> Out msg act
outText acc script =
  case script of
    Text str next ->
      outText (("", str) :: acc) next

    Labeled label str next ->
      outText ((label, str) :: acc) next

    _ ->
      OutText <| List.reverse acc


outActs : List act -> Script msg act -> Out msg act
outActs acc script =
  case script of
    Call act next ->
      outActs (act :: acc) next

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
