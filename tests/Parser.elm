module Parser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Util exposing (..)

import Novel as N exposing (Novel(..))
import Novel.Parser as Parser exposing (..)





plain : String
plain = "str"

all : Test
all =
  describe "Paser Test"
    [ "1@"
      => parse_ at "@" === Ok N.at
    , "2@"
      => parse_ at "＠" === Ok N.at
    , "3"
      => parse_ text "asa＠sss\n" === Ok (Text "asa" (At (Text "sss" (Return ()))))
    , "4text0"
      => parse_ text0 "aaa" === Ok (N.text "aaa")
    , "5text0"
      => parse_ text0 "aaa\n" === Ok (N.text "aaa")
    , "6text0"
      => parse_ text0 "aaa\r\n" === Ok (N.text "aaa")
    , "7text1"
      => parse_ text1 "aaa@\n" === Ok (Text "aaa" (At (Return ())))
    , "8text1"
      => parse_ text1 "aaa@" === Ok (Text "aaa" (At (Return ())))
    , "9text2"
      => parse_ text2 "aaa@aaaa\n" === Ok (Text "aaa" (At (Text "aaaa" (Return ()))))
    , "10line"
      => parse_ line "@aa dare\n" ===
        Ok (N.line "aa" "dare")

        -- Ok (N.text "aaa"
        --   |> N.andThen (\_ -> N.at))
    -- , "text produces text"
    --   => parse_ text "text" === Ok (Novel.text "text")
    -- , "factor matches at"
    --   => parse_ Parser.factor "@tees" === Ok Novel.at
    -- , "factor matches text"
    --   => parse_ Parser.factor "ddes@kdl" === Ok (Novel.text "ddes")
    -- , "novel matches text @"
    --   => parse "novel@" === Ok (Text "novel" Novel.at)
    -- , "novel matches empty"
    --   => parse "" === Ok (Novel.return)
    ]
