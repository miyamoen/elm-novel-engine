module Novel.Parser exposing (..)

import Novel exposing (Novel, InnerNovel(..))
import Parser exposing (..)
import Parser.LanguageKit as LK exposing (..)
import Parser.LowLevel exposing (getCol)
import Debug exposing (log)


parse : String -> Result String Novel
parse input=
  case run (novel []) input of
    Err problem ->
      Err <| toString problem

    Ok res ->
      Ok res


novel : List (String, InnerNovel Novel.Msg) -> Parser Novel
novel revNovels =
  oneOf
    [ succeed ((,) "main")
      |= innerNovel
      |> andThen (\nov -> novel <| (log "main" nov) :: revNovels)
    , succeed identity
      |= sectionNovel
      |> andThen (\nov -> novel <| (log "section" nov) :: revNovels)
    , succeed (List.reverse revNovels |> Novel.novelFromList)
      |. end
    ]


sectionNovel : Parser ((String, InnerNovel Novel.Msg))
sectionNovel =
  succeed (,)
    |. string (containsChar [ '{', '｛' ])
    |. chompSeparators
    |. at
    |= string isLabel
    |= innerNovel
    |. string (containsChar [ '}', '｝' ])
    |. chompSpaces


innerNovel : Parser (InnerNovel Novel.Msg)
innerNovel =
  succeed identity
    |. chompSpaces
    |= innerNovelHelp []
    |. chompSpaces


innerNovelHelp : List (InnerNovel Novel.Msg) -> Parser (InnerNovel Novel.Msg)
innerNovelHelp revNovels =
  let
    tagger factor =
      factor
        |. chompSpaces
        |> andThen (\nov -> innerNovelHelp <| nov :: revNovels)
  in
    oneOf
      [ tagger line
      , tagger text
      , tagger jumpMessage
      , tagger at
      , recursionHelp revNovels
      ]


text : Parser (InnerNovel a)
text =
  succeed Novel.text
    |= string isText


string : (Char -> Bool) -> Parser String
string checker =
  source <|
    ignore oneOrMore checker


textWithAt : List (InnerNovel a) -> Parser (InnerNovel a)
textWithAt revNovels =
  oneOf
    [ text
      |. chompSeparators
      |> andThen (\nov -> textWithAt <| nov :: revNovels)
    , at
      |. chompSeparators
      |> andThen (\nov -> textWithAt <| nov :: revNovels)
    , recursionHelp revNovels
    ]


recursionHelp : List (InnerNovel a) -> Parser (InnerNovel a)
recursionHelp revNovels =
  case revNovels of
    [] ->
      fail "Novel is empty."

    _ ->
      List.reverse revNovels
        |> Novel.concat
        |> succeed


line : Parser (InnerNovel a)
line =
  getCol
    |> andThen lineHelp


lineHelp : Int -> Parser (InnerNovel a)
lineHelp col =
  case col of
    1 ->
      delayedCommit at <|
        succeed Novel.labeled
          |= string isLabel
          |. chompSeparators
          |= textWithAt []

    _ ->
      fail "A line starts with @"


at : Parser (InnerNovel a)
at =
  succeed Novel.at
    |. ignore (Exactly 1) (containsChar [ '@', '＠' ])


jumpMessage : Parser (InnerNovel Novel.Msg)
jumpMessage =
  delayedCommit (ignore (Exactly 1) (containsChar [ '@', '＠' ])) <|
    (succeed Novel.Jump
      |. ignore (Exactly 1) (containsChar [ '!', '！' ])
      |. chompSeparators
      |. keyword "jump"
      |. chompSeparators
      |= string isLabel
      |> map (List.singleton >> Novel.Return)
    )


isText : Char -> Bool
isText char =
  not <| containsChar [ '\r', '\n', '@', '＠', '{', '｛', '}', '｝' ] (log "isText" char)


isLabel : Char -> Bool
isLabel char =
  not <| containsChar [ '\r', '\n', '@', '＠', ' ', '　', '\t', '!', '！' ] (log "isLabel" char)


chompSeparators : Parser ()
chompSeparators =
  ignore zeroOrMore isSeparator


chompSpaces : Parser ()
chompSpaces =
  ignore zeroOrMore isSpace


isSpace : Char -> Bool
isSpace char =
  isSeparator char || containsChar [ '\r', '\n' ] char


isSeparator : Char -> Bool
isSeparator =
  containsChar [ ' ', '　', '\t']


containsChar : List Char -> Char -> Bool
containsChar list key =
  List.foldl (\elm bool -> bool || elm == key) False list