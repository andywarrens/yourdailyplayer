module Model exposing (..)

import Css exposing (Color, hsl, hex)

type InputType = Key String | Backspace | Enter
type HintType = CharCount Int | Nationality String
type alias HintPrice = Int
type alias Hint = 
  { price: HintPrice
  , ix: Int
  , hintType: HintType }

type Msg = 
    Input InputType 
  | OpenHint Hint | BuyHint Hint | CloseHint 
  | GiveUp
  | ToggleHelp Bool

type GameState = Playing | GameOver | Won
type alias Model =
  { userInput: String
  , state   : GameState
  , score   : Int
  , guesses : List String
  , isHint1Bought : Bool
  , isHint2Bought : Bool
  , showHelp : Bool
  , showHint : Maybe Hint
  }
type alias GameModel =
  { answer : String
  , nGuesses : Int
  , initialScore : Int 
  , hint1 : Hint
  , hint2 : Hint
  }

initialModel : Model
initialModel =
  { userInput = "An"
  , state     = Playing
  , score     = gameModel.initialScore
  , guesses   = []
  , isHint1Bought = False
  , isHint2Bought = False
  , showHelp  = False
  , showHint  = Nothing
  }

gameModel : GameModel
gameModel = 
  { answer   = "Andy"
  , nGuesses = 5
  , initialScore = 10
  , hint1 = { ix = 1, price = 2, hintType = CharCount 4 }
  , hint2 = { ix = 2, price = 2, hintType = Nationality "Belg" }
  }

type alias ThemeColor =
  { l1: Color
  , l2: Color
  , l3: Color
  , l4: Color
  , l5: Color
  }

theme : 
  { secondary : ThemeColor
  , primary   : ThemeColor
  }
theme =
    { primary = ThemeColor
        (hsl 223 0.90 0.85)
        (hsl 223 0.90 0.65)
        (hsl 223 0.90 0.45)
        (hsl 223 0.90 0.30)
        (hsl 223 0.90 0.10)
    , secondary = ThemeColor
        (hsl 29 0.90 0.85)
        (hsl 29 0.90 0.65)
        (hsl 29 0.90 0.45)
        (hsl 29 0.90 0.30)
        (hsl 29 0.90 0.15)
    }

white : Color
white = (hex "ffffff")

black : Color
black = (hex "000000")