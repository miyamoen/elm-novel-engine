module Novel.Parser exposing (..)


import Novel exposing (Novel(..))


import Combine exposing (Parser, many1, many, while, sepEndBy1, ($>), (<*))
import Combine.Char exposing (anyChar, char, noneOf, eol)

type alias Label = String


parse_ : Parser () (Novel a) -> String -> Result String (Novel a)
parse_ p input =
  case Combine.parse p input of
    Ok (state, stream, novel) ->
      Ok novel

    Err (state, stream, errors) ->
      Err (String.join " or " errors)


parse : String -> Result String (Novel ())
parse =
  parse_ novel


novel : Parser () (Novel ())
novel =
  (Combine.many factor <* Combine.end)
    |> Combine.map (Novel.concat >> Maybe.withDefault Novel.return)


factor : Parser () (Novel ())
factor =
  [ at
  , text
  ] |> Combine.choice


text : Parser () (Novel ())
text =
  many1 (noneOf [ '@' ])
    |> Combine.map (String.fromList >> Novel.text)


at : Parser () (Novel ())
at =
  char '@' <* many eol
    $> Novel.at
