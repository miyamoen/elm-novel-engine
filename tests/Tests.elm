module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Util exposing (..)
import Parser

import Novel exposing (..)



plain : String
plain = "str"

all : Test
all =
  describe "Novel Parser Test"
    [ describe "Novel Test"
      [ "at produces At Return"
        => at === At (Return ())
      , "text produces Text string Return"
        => text plain === Text plain (Return ())
      , "return produces Return"
        => return === Return ()
      , "text appends return"
        => append (text plain) return === Text plain (Return ())
      , "return appends return"
        => append return return === Return ()
      , "text appends at"
        => append (text plain) at === Text plain (At (Return ()))
      , "text appends text"
        => append (text "1") (text "2") === Text "1" (Text "2" (Return ()))
      , "at appends at"
        => append at at === At (At (Return ()))
      , "at appends text"
        => append at (text plain) === At (Text plain (Return ()))
      , "at appends return"
        => append at return === At (Return ())
      , "return appends at"
        => append return at === At (Return ())
      , "return appends text"
        => append return (text plain) === Text plain (Return ())
      , Parser.all
      ]
    ]
