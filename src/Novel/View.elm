module Novel.View exposing (..)

import List exposing (singleton)
import Html exposing (Html)
import HtmlTree exposing (..)
import HtmlTree.Tags exposing (tag)
import BulmaClasses as B exposing (element, message, content)
import Bulma.HtmlTree exposing (..)
import CssBasics.Properties exposing (property)
import CssBasics exposing (CssValue(..), UnitType(..))
import Novel exposing (..)
import Novel.Script exposing (..)



main : Html Action
-- main = Html.text "すっごーい　君は型合わせが得意なフレンズなんだね！"
main =
  container tag.div
  [ lines sample
  , cdn
  ]
    |> assembleHtml


(=>) = (,)


game : Novel state msg -> HtmlTree Action
game novel =
  container tag.div
    [ lines novel.lines
    , cdn
    ]


-- type Line
--   = TextList (List String)
--   | LabeledList Label (List String)
--   | ChoiceList (List String)
sample : List Line
sample =
  [ TextList
    [ "ham", "egg", "spamspamspamspamspamspamspamspam spamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspam" ]
  , ChoiceList
    [ "ham", "egg", "spamspamspamspamspamspamspamspam spamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspam" ]
  , LabeledList "Python"
    [ "ham", "egg", "spamspamspamspamspamspamspamspam spamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspamspam" ]
  ]

lines : List Line -> HtmlTree Action
lines ls =
  List.map line ls
    |> container tag.div


line : Line -> HtmlTree Action
line l =
  case l of
    TextList texts ->
      text texts

    LabeledList label texts ->
      labeled label texts

    ChoiceList texts ->
      choice texts


labeled : Label -> List String -> HtmlTree Action
labeled label strs =
  messageBox (Just label) strs
    |> addClass message.color.isPrimary
    |> addAction ("click", Feed)


text : List String -> HtmlTree Action
text strs =
  messageBox Nothing strs
    |> addAction ("click", Feed)


choice : List String -> HtmlTree Action
choice strs =
  strs
    |> List.map (List.singleton >> messageBox Nothing)
    |> List.indexedMap
      (\idx tree ->
        tree
          |> addClass message.color.isInfo
          |> addAction ("click", Choice idx)
      )
    |> container tag.div


messageBox : Maybe Label -> List String -> HtmlTree msg
messageBox maybeLabel strs =
  let
    labeled tree =
      case maybeLabel of
        Just label ->
          textWrapper tag.p label
            |> container_ tag.div
            |> addClass message.header
            |> containerTreeAhead tree

        Nothing ->
          tree
  in
    strs
      |> List.map (textWrapper tag.p)
      |> container tag.div
      |> addClass message.body
      |> addStyle (property.wordWrap, Str "break-word")
      |> container_ tag.div
      |> addClass message.container
      |> addStyle (property.margin, Unit 1.0 Em)
      |> labeled


cdn : HtmlTree msg
cdn =
  let
    style url =
      leaf "link"
        |> addAttribute ("rel", "stylesheet")
        |> addAttribute ("type", "text/css")
        |> addAttribute ("href", url)
  in
    container "div"
      [ style "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.1/css/bulma.min.css"
      , style "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
      ]


container_ : String -> HtmlTree msg -> HtmlTree msg
container_ tag child =
  child
    |> List.singleton
    |> container tag


containerTreeBehind : HtmlTree msg -> HtmlTree msg -> HtmlTree msg
containerTreeBehind =
  flip appendChild

containerTreeAhead : HtmlTree msg -> HtmlTree msg -> HtmlTree msg
containerTreeAhead =
  flip prependChild