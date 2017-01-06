module Parser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Util exposing (..)

import Novel exposing (Novel(..))
import Novel.Parser as Parser exposing (..)





plain : String
plain = "str"

all : Test
all =
  describe "Paser Test"
    [ "at produces At End"
      => parse_ at "@" === Ok (At End)
    , "text produces text"
      => parse_ text "text" === Ok (Text "text" End)
    , "factor matches at"
      => parse_ Parser.factor "@tees" === Ok (At End)
    , "factor matches text"
      => parse_ Parser.factor "ddes@kdl" === Ok (Text "ddes" End)
    , "novel matches text @"
      => parse "novel@" === Ok (Text "novel" (At End))
    , "novel matches empty"
      => parse "" === Ok (End)
    ]
