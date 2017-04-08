module Novel exposing (..)

import Dict exposing (Dict)
import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass, prependChild, addAction)
import Tuple
import List.Extra exposing (getAt)


type alias Label = String

type Script msg action
  = Text String (Script msg action)
  | Labeled Label String (Script msg action)
  | Wait (Script msg action)
  | Call action (Script msg action)
  | ChoiceCall (List (String, action)) (Script msg action)
  | Return (List msg)


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


append : Script msg act -> Script msg act -> Script msg act
append a b =
  a |> andThen identity (\msgs -> b |> andThen identity (\msgs_ -> Return <| msgs ++ msgs_))


andAppend : Script msg act -> Script msg act -> Script msg act
andAppend =
  flip append


concat : List (Script msg act) -> Script msg act
concat =
  List.foldl andAppend return_


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


