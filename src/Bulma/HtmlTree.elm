module Bulma.HtmlTree exposing (..)


import HtmlTree exposing (HtmlTree, leaf, withClasses, container, textWrapper, addClass)
import BulmaClasses exposing (..)


{-
  iconLeaf "book"
    |> addClass icon.size.isSmall
-}
iconLeaf : String -> HtmlTree msg
iconLeaf name =
  leaf "i"
    |> withClasses [ "fa", "fa-" ++ name ]
    |> List.singleton
    |> container "span"
    |> withClasses [ icon.container ]


{-
  tagLeaf
    |> addClass icon.size.isLarge
    |> addClass tag.color.isInfo
    |> withChildren [ deleteLeaf ]
-}
tagLeaf : HtmlTree msg
tagLeaf =
  "Tag Label"
    |> textWrapper "span"
    |> withClasses [ tag.ui ]


{-
  deleteLeaf
    |> addClass delete.size.isMedium
-}
deleteLeaf : HtmlTree msg
deleteLeaf =
  leaf "button"
    |> withClasses [ delete.ui ]


{-
  buttonLeaf
    |> addClass button.style.isInverted
    |> addClass button.size.isMedium
    |> addClass button.color.isPrimary
    |> addClass button.addon.isExpanded
    |> withText "ok"

  buttonLeaf
    |> appendChild (iconLeaf "book")
-}
buttonLeaf : HtmlTree msg
buttonLeaf =
  leaf "button"
    |> withClasses [ button.ui ]


{-
  sectionTree
    |> addClass section.spacing.isMedium
    |> appendChild (iconLeaf "book")
-}
sectionTree : HtmlTree msg
sectionTree =
  leaf "section"
    |> withClasses [ section.container ]


{-
  footerTree
    |> appendChild (iconLeaf "book")
-}
footerTree : HtmlTree msg
footerTree =
  leaf "footer"
    |> withClasses [ footer.container ]


{-
  navTree
    |> addClass nav.style.hasShadow
-}
navTree : HtmlTree msg
navTree =
  leaf "nav"
    |> withClasses [ nav.container ]


navLeftTree : HtmlTree msg
navLeftTree =
  leaf "div"
    |> withClasses [ nav.left ]


navRightTree : HtmlTree msg
navRightTree =
  leaf "div"
    |> withClasses [ nav.right ]


navCenterTree : HtmlTree msg
navCenterTree =
  leaf "div"
    |> withClasses [ nav.center ]


navToggleTree : HtmlTree msg
navToggleTree =
  leaf "span"
    |> withClasses [ nav.toggle ]


navMenuTree : HtmlTree msg
navMenuTree =
  navRightTree
    |> addClass nav.menu.container


{-
  navItemTree
    |> addClass nav.item.style.isTab
    |> addClass nav.item.state.isActive
    |> withText "Link"
-}
navItemTree : HtmlTree msg
navItemTree =
  leaf "a"
    |> withClasses [ nav.item.container ]


{-
  heroTree
    |> addClass hero.style.isBold
    |> addClass hero.size.isMedium
    |> addClass hero.color.isSuccess
-}
heroTree : HtmlTree msg
heroTree =
  leaf "section"
    |> addClass hero.container


heroHeadTree : HtmlTree msg
heroHeadTree =
  leaf "div"
    |> addClass hero.head


heroBodyTree : HtmlTree msg
heroBodyTree =
  leaf "div"
    |> addClass hero.body


heroFootTree : HtmlTree msg
heroFootTree =
  leaf "div"
    |> addClass hero.foot


{-
  levelTree
    |> addClass level.mobile.isHorizontal
-}
levelTree : HtmlTree msg
levelTree =
  leaf "div"
    |> addClass level.container


levelLeftTree : HtmlTree msg
levelLeftTree =
  leaf "div"
    |> addClass level.left


levelRightTree : HtmlTree msg
levelRightTree =
  leaf "div"
    |> addClass level.right


levelItemTree : HtmlTree msg
levelItemTree =
  leaf "div"
    |> addClass level.item


{-
  featureTree
    |> addClass feature.sizing.isFluid

  iconTree "book"
    |> flip appendChild featureTree
-}
featureTree : HtmlTree msg
featureTree =
  leaf "div"
    |> addClass feature.container


{-
  tileTree
    |> addClass tile.orientation.isVertical
    |> addClass tile.width.is5
-}
tileTree : HtmlTree msg
tileTree =
  leaf "div"
    |> addClass tile.container


tileAncestorTree : HtmlTree msg
tileAncestorTree =
  tileTree
    |> addClass tile.level.isAncestor


tileParentTree : HtmlTree msg
tileParentTree =
  tileTree
    |> addClass tile.level.isParent


tileChildTree : HtmlTree msg
tileChildTree =
  tileTree
    |> addClass tile.level.isChild
    -- |> addClass element.style.box


{--}
controlTree : HtmlTree msg
controlTree =
  leaf "p"
    |> addClass control.container


controlContainerTree : HtmlTree msg
controlContainerTree =
  leaf "div"
    |> addClass control.container


textareaLeaf : HtmlTree msg
textareaLeaf =
  leaf "textarea"
    |> addClass textarea.ui


{--}
cardTree : HtmlTree msg
cardTree =
  leaf "div"
    |> addClass card.container


cardHeaderTree : HtmlTree msg
cardHeaderTree =
  leaf "header"
    |> addClass card.header.container


cardHeaderIconLeaf : String -> HtmlTree msg
cardHeaderIconLeaf icon =
  iconLeaf icon
    |> List.singleton
    |> container "a"
    |> addClass card.header.icon


cardHeaderTitleLeaf : String -> HtmlTree msg
cardHeaderTitleLeaf title =
  title
    |> textWrapper "p"
    |> addClass card.header.title


cardFooterTree : HtmlTree msg
cardFooterTree =
  leaf "footer"
    |> addClass card.footer.container


cardFooterItemLeaf : HtmlTree msg
cardFooterItemLeaf =
  leaf "a"
    |> addClass card.footer.item


cardContentTree : HtmlTree msg
cardContentTree =
  leaf "div"
    |> addClass card.content


cardImageTree : HtmlTree msg
cardImageTree =
  leaf "div"
    |> addClass card.image

