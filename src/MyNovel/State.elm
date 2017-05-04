module Novel.State exposing (..)


type alias State =
  { daikon : DaikonFlag
  -- , konnyaku : KonnyakuFlag
  -- , tamago : TamagoFlag
  , scene : List String
  }


type DaikonFlag
  = DZokkon
  | DKininaru
  | DMukatsuku


-- type KonnyakuFlag
--   = KHot
--   | KLove
--   | KShiranai


-- type TamagoFlag
--   = TIchizu
--   | TBitch

initialState : State
initialState  =
  { daikon = DMukatsuku
  , scene = []
  }


type Msg
  = Read String
  | Daikon DaikonFlag
  | NoMsg


update : Msg -> State -> State
update msg state =
  case msg of
    Read scene ->
      { state | scene = scene :: state.scene }

    Daikon flag ->
      { state | daikon = flag }

    NoMsg ->
      state


parseMsg : String -> Msg
parseMsg str =
  case str of
    "daikon zokkon" ->
      Daikon DZokkon

    "daikon kininaru" ->
      Daikon DKininaru

    "daikon mukatsuka" ->
      Daikon DMukatsuku

    label ->
      Read label


nextScene : State -> String
nextScene state =
  "一章"

