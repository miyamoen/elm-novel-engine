module Novel exposing (..)


import List.Extra
type alias Label = String



type Novel a
  = Text String (Novel a)
  -- | Line Label String Novel
  | At (Novel a)
  | Return a


text : String -> Novel ()
text text =
  Text text return


at : Novel ()
at =
  At return


return : Novel ()
return =
  Return ()


andThen : (a -> Novel b) -> Novel a -> Novel b
andThen func novel =
  case novel of
    Text text next ->
      Text text <| andThen func next

    At next ->
      At <| andThen func next

    Return val ->
      func val


append : Novel a -> Novel b -> Novel b
append left right =
  andThen (\_ -> right) left


andEnd : Novel b -> Novel a -> Novel b
andEnd =
  flip append


uncons : Novel a -> Maybe (Novel (), Novel a)
uncons novel =
  case novel of
    Return a ->
      Nothing

    Text str rest ->
      Just (text str, rest)

    At rest ->
      Just (at, rest)


concat : List (Novel a) -> Maybe (Novel a)
concat =
  List.Extra.foldl1 append


untilAt : Novel a -> (Novel (), Novel a)
untilAt novel =
  case novel of
    Return a ->
      (return, Return a)

    Text str rest ->
      let
        (head, rest_) = untilAt rest
      in
        (text str |> andEnd head, rest_)

    At rest ->
      (at, rest)


toString : Novel a -> String
toString novel =
  case novel of
    Return a ->
      ""

    Text str rest ->
      str ++ toString rest

    At rest ->
      " ▼"