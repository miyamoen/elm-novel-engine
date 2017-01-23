module Novel.Parser exposing (..)


import Novel exposing (Novel(..))


import Combine exposing (Parser, many1, many, manyTill, while, sepEndBy1, lazy, lookAhead, ($>), (*>), (<*), (<|>))
import Combine.Char exposing (anyChar, char, noneOf, oneOf)

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


andEnd : Parser () (Novel ()) -> Parser () (Novel ()) -> Parser () (Novel ())
andEnd right left =
  left
    |> Combine.map Novel.append
    |> Combine.andMap right


andIgnore : Parser s x -> Parser s a -> Parser s a
andIgnore right left =
  left <* right


novel : Parser () (Novel ())
novel =
  Combine.many factor
    |> Combine.map Novel.concat


factor : Parser () (Novel ())
factor =
  Combine.choice
    [ endOrEol $> Novel.return
    , at <* endOrEol
    , line <* endOrEol
    , text <* endOrEol
    ]


text : Parser () (Novel ())
text =
  Combine.choice
    [ text0
    , text1
    , text2
    ]


text0 : Parser () (Novel ())
text0 =
  string
    |> Combine.map Novel.text
    |> andIgnore (lookAhead endOrEol)


text1 : Parser () (Novel ())
text1 =
  string
    |> Combine.map Novel.text
    |> andEnd at
    |> andIgnore (lookAhead endOrEol)


text2 : Parser () (Novel ())
text2 =
  string
    |> Combine.map Novel.text
    |> andEnd at
    |> andEnd (lazy (\_ -> text))


line : Parser () (Novel ())
line =
  at *> label
    |> Combine.map Novel.labeled
    |> Combine.andMap text


label : Parser () String
label =
  let
    until =
      space $> Novel.return

    char =
      noneOf [ '@', '＠', '\n', '\r' ]
  in
    char
      |> Combine.map (::)
      |> Combine.andMap (manyTill char until)
      |> Combine.map String.fromList
      |> Combine.mapError (\_ -> [ "expected label" ])


string : Parser () String
string =
  let
    until =
      endOrEol $> Novel.return <|> at
        |> lookAhead

    char =
      noneOf [ '@', '＠', '\n', '\r' ]
  in
    char
      |> Combine.map (::)
      |> Combine.andMap (manyTill char until)
      |> Combine.map String.fromList
      |> Combine.mapError (\_ -> [ "expected string" ])


at : Parser () (Novel ())
at =
  oneOf [ '@', '＠' ]
    $> Novel.at
    |> Combine.mapError (\_ -> [ "expected @" ])


space : Parser () Char
space =
  oneOf [ ' ', '　' ]
    |> Combine.mapError (\_ -> [ "expected space" ])


endOrEol : Parser () Char
endOrEol =
  Combine.Char.eol <|> Combine.end
    $> '\n'
    |> Combine.mapError (\_ -> [ "expected eol and input end" ])

