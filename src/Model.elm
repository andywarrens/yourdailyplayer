module Model exposing (..)

import Css exposing (Color, hsl, hex)

type InputType = Key String | Backspace
type Msg = 
    Input InputType 
  | OpenHint | CloseHint
  | ToggleHelp Bool

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