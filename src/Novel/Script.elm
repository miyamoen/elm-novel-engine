module Novel.Script exposing (..)

import Html exposing (Html)

main : Html msg
main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"


type alias Label = String


type Script flg act
  = Text String (Script flg act)
  | Labeled Label String (Script flg act)
  | Wait (Script flg act)
  | Call act (Script flg act)
  | ChoiceCall (List (String, act)) (Script flg act)
  | Return (List flg)


-- Constructor

return : List flg -> Script flg act
return flgs =
  Return flgs


return_ : Script flg act
return_ =
  return []


text : String -> Script flg act
text str =
  Text str return_


labeled : Label -> String -> Script flg act
labeled label str =
  Labeled label str return_


wait : Script flg act
wait =
  Wait return_


call : act -> Script flg act
call act =
  Call act return_


choiceCall : List (String, act) -> Script flg act
choiceCall choices =
  ChoiceCall choices return_


-- monad?

andThen : (act -> act_) -> (List flg -> Script flg_ act_) -> Script flg act -> Script flg_ act_
andThen actF flgF script =
  case script of
    Text str next ->
      Text str <| andThen actF flgF next

    Labeled label str next ->
      Labeled label str <| andThen actF flgF next

    Wait next ->
      Wait <| andThen actF flgF next

    Call act next ->
      Call (actF act) <| andThen actF flgF next

    ChoiceCall choices next ->
      ChoiceCall (List.map (Tuple.mapSecond actF) choices) <| andThen actF flgF next

    Return flgs ->
      flgF flgs


andThen_ : (List flg -> Script flg_ act) -> Script flg act -> Script flg_ act
andThen_ =
  andThen identity


append : Script flg act -> Script flg act -> Script flg act
append a b =
  a |> andThen_ (\flgs -> b |> andThen_ (\flgs_ -> Return <| flgs ++ flgs_))


andAppend : Script flg act -> Script flg act -> Script flg act
andAppend =
  flip append


concat : List (Script flg act) -> Script flg act
concat =
  List.foldl andAppend return_


-- Out

type Out flg act
  = OutText (List (Label, String))
  | OutActs (List act)
  | OutChoice (List (String, act))
  | OutFlgs (List flg)


out : Script flg act -> Out flg act
out script =
  case script of
    Return flgs ->
      OutFlgs flgs

    Call act next ->
      outActs [ act ] next

    ChoiceCall choices _ ->
      OutChoice choices

    Text str next ->
      outText [ ("", str) ] next

    Labeled label str next ->
      outText [ (label, str) ] next

    Wait _ ->
      OutText []


outText : List (Label, String) -> Script flg act -> Out flg act
outText acc script =
  case script of
    Text str next ->
      outText (("", str) :: acc) next

    Labeled label str next ->
      outText ((label, str) :: acc) next

    _ ->
      OutText <| List.reverse acc


outActs : List act -> Script flg act -> Out flg act
outActs acc script =
  case script of
    Call act next ->
      outActs (act :: acc) next

    _ ->
      OutActs <| List.reverse acc


-- operation

feed : Script flg act -> Script flg act
feed script =
  case script of
    Text _ next ->
      feedText next

    Labeled _ _ next ->
      feedText next

    Wait next ->
      next

    Call _ next ->
      feedCall next

    ChoiceCall _ next ->
      next

    Return _ ->
      script


feedText : Script flg act -> Script flg act
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


feedCall : Script flg act -> Script flg act
feedCall script =
  case script of
    Call _ next ->
      feedCall next

    _ ->
      script

