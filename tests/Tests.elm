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
      [ "at produces At End"
        => at === At End
      , "text produces Text string End"
        => text plain === Text plain End
      , "end produces End"
        => end === End
      , "text appends end"
        => append (text plain) end === Text plain End
      , "end appends end"
        => append end end === End
      , "text appends at"
        => append (text plain) at === Text plain (At End)
      , "text appends text"
        => append (text "1") (text "2") === Text "1" (Text "2" End)
      , "at appends at"
        => append at at === At (At End)
      , "at appends text"
        => append at (text plain) === At (Text plain End)
      , "at appends end"
        => append at end === At End
      , "end appends at"
        => append end at === At End
      , "end appends text"
        => append end (text plain) === Text plain End
      , Parser.all
      ]
    ]
