module Novel.Script exposing (..)


type alias Label = String


type Script msg action
  = Text String (Script msg action)
  | Labeled Label String (Script msg action)
  | Wait (Script msg action)
  | Call action (Script msg action)
  | ChoiceCall (List (String, action)) (Script msg action)
  | Return (List msg)


-- Constructor

return : List msg -> Script msg act
return msgs =
  Return msgs


return_ : Script msg act
return_ =
  return []


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


choiceCall : List (String, act) -> Script msg act
choiceCall choices =
  ChoiceCall choices return_


--

andThen : (act -> act_) -> (List msg -> Script msg_ act_) -> Script msg act -> Script msg_ act_
andThen actF msgF script =
  case script of
    Text str next ->
      Text str <| andThen actF msgF next

    Labeled label str next ->
      Labeled label str <| andThen actF msgF next

    Wait next ->
      Wait <| andThen actF msgF next

    Call act next ->
      Call (actF act) <| andThen actF msgF next

    ChoiceCall choices next ->
      ChoiceCall (List.map (Tuple.mapSecond actF) choices) <| andThen actF msgF next

    Return msgs ->
      msgF msgs


andThen_ : (List msg -> Script msg_ act) -> Script msg act -> Script msg_ act
andThen_ =
  andThen identity


append : Script msg act -> Script msg act -> Script msg act
append a b =
  a |> andThen_ (\msgs -> b |> andThen_ (\msgs_ -> Return <| msgs ++ msgs_))


andAppend : Script msg act -> Script msg act -> Script msg act
andAppend =
  flip append


concat : List (Script msg act) -> Script msg act
concat =
  List.foldl andAppend return_


-- Out

type Out msg act
  = OutText (List (Label, String))
  | OutActs (List act)
  | OutChoice (List (String, act))
  | OutMsgs (List msg)


out : Script msg act -> Out msg act
out script =
  case script of
    Return msgs ->
      OutMsgs msgs

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

feed : Script msg act -> Script msg act
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

