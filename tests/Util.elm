module Util exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

(=>) : String -> (() -> Expectation) -> Test
(=>) = Test.test


(===) : a -> a -> () -> Expectation
(===) actual expected _ = Expect.equal actual expected

(/==) : a -> a -> () -> Expectation
(/==) actual expected _ = Expect.notEqual actual expected


infixl 1 ===
infixl 1 /==
infixl 0 =>