module View exposing (..)


import HtmlTree exposing (..)
import BulmaClasses exposing (..)
import ViewTree exposing (..)


appendParent : HtmlTree msg -> HtmlTree msg -> HtmlTree msg
appendParent =
  flip appendChild


styleLeaf : String -> HtmlTree msg
styleLeaf url =
  leaf "link"
    |> addAttribute ("rel", "stylesheet")
    |> addAttribute ("type", "text/css")
    |> addAttribute ("href", url)


cdnTree : HtmlTree msg
cdnTree =
  let
    bulma =
      styleLeaf "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.1/css/bulma.min.css"

    fontAwesome =
      styleLeaf "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  in
    container "div" [ bulma, fontAwesome ]


heroHeader : HtmlTree msg
heroHeader =
  let
    head =
      iconLeaf "github"
        |> appendParent navItemTree
        |> addAttribute ("href", "https://github.com/miyamoen/elm-novel-engine")
        |> appendParent navRightTree
        |> appendParent navTree
        |> addClass nav.style.hasShadow
        |> appendParent heroHeadTree

    body =
      "Novel Engine"
        |> textWrapper "h1"
        |> withClasses [ heading.title, heading.size.is1 ]
        |> appendParent heroBodyTree
        |> addClass element.alignment.hasTextCentered
  in
    heroTree
      |> addClass hero.color.isPrimary
      |> appendChild head
      |> appendChild body


novelInputCard : String -> HtmlTree msg
novelInputCard text =
  let
    footer =
      cardFooterItemLeaf
        |> appendChild (iconLeaf "play-circle")
        |> appendParent cardFooterTree

    header =
      cardHeaderTitleLeaf "Your Novel"
        |> appendParent cardHeaderTree

    content =
      textareaLeaf
        |> addAttribute ("value", text)
        -- |> addClass textarea.size.isLarge
        |> appendParent controlTree
        |> appendParent cardContentTree
  in
    cardTree
      |> appendChild header
      |> appendChild content
      |> appendChild footer



novelCard : String -> HtmlTree msg -> HtmlTree msg
novelCard label contentTree =
  let
    content =
      contentTree
        |> appendParent cardContentTree

    header =
      cardHeaderTitleLeaf label
        |> appendParent cardHeaderTree

    footer =
      cardFooterItemLeaf
        |> appendChild (iconLeaf "forward")
        |> appendParent cardFooterTree
  in
    cardTree
      |> appendChild header
      |> appendChild content
      |> appendChild footer