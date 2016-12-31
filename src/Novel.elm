module Novel exposing (Novel, empty, parser)


import Combine exposing (Parser, manyTill)
import Combine.Char exposing (anyChar)

type alias Label = String


{-
type Novel
  = Text String Novel
  | Line Label String Novel
  | End
-}

type Novel
  = Text String


parser : Parser s Novel
parser =
  manyTill anyChar Combine.end
    |> Combine.map (String.fromList >> Text)

empty : Novel
empty =
  Text ""