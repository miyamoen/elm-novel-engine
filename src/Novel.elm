module Novel exposing (..)


type alias Label = String



type Novel a
  = Text String (Novel a)
  | Line Label String (Novel a)
  | At (Novel a)
  | Return a


text : String -> Novel ()
text str =
  Text str return


line : Label -> String -> Novel ()
line label str =
  Line label str return


labeled : Label -> Novel a -> Novel a
labeled label novel =
  case novel of
    Text str rest ->
      Line label str <| labeled label rest

    Line label_ str rest ->
      Line label_ str <| labeled label rest

    At rest ->
      At <| labeled label rest

    Return a ->
      Return a


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

    Line label str next ->
      Line label str <| andThen func next

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

    Line label str rest ->
      Just (line label str, rest)

    At rest ->
      Just (at, rest)


concat : List (Novel ()) -> Novel ()
concat =
  -- List.Extra.foldl1 append
  List.foldl (\right left -> andThen (\_ -> right) left) return


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

    Line label str rest ->
      let
        (head, rest_) = untilAt rest
      in
        (line label str |> andEnd head, rest_)

    At rest ->
      (at, rest)


toString : Novel a -> String
toString novel =
  case novel of
    Return a ->
      ""

    Text str rest ->
      str ++ "\n" ++ toString rest

    Line label str rest ->
      "[ " ++ label ++ " ] : " ++ str ++ "\n" ++ toString rest

    At rest ->
      "▼"