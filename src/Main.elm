module Main exposing (main)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class)
import Html.Styled.Events exposing (onClick)
import Browser

type alias ThemeColor =
  { l1: Color
  , l2: Color
  , l3: Color
  , l4: Color
  , l5: Color
  }

{-| A plain old record holding a couple of theme colors.
-}
theme : 
  { secondary : ThemeColor
  , primary   : ThemeColor
  , white     : Color
  }
theme =
    { primary = ThemeColor
        (hsl 223 0.90 0.85)
        (hsl 223 0.90 0.65)
        (hsl 223 0.90 0.45)
        (hsl 223 0.90 0.30)
        (hsl 223 0.90 0.15)
    , secondary = ThemeColor
        (hsl 29 0.90 0.85)
        (hsl 29 0.90 0.65)
        (hsl 29 0.90 0.45)
        (hsl 29 0.90 0.30)
        (hsl 29 0.90 0.15)
    , white = (hex "ffffff")
    }


{-| A reusable button which has some styles pre-applied to it.
-}
btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn =
    styled button
        [ margin (px 12)
        , color (rgb 250 250 250)
        , hover
            [ backgroundColor theme.primary.l1
            , textDecoration underline
            ]
        ]

type alias Model = Int
type Msg = Decrement | Increment

main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        }

update : Msg -> Model -> Model
update arg1 arg2 =
    Debug.todo "TODO"

initialModel : Model
initialModel = 0

view : Model -> Html Msg
view model =
  div 
    [ css 
        [ displayFlex
        , flexDirection column
        ]
    , class "viewport"
    ]
    [ helpBanner
    , puzzleContent
    , puzzleInput
    , hintSection
--    , button [ onClick Decrement ] [ text "--" ]
--    , div [] [ text (String.fromInt model) ]
--    , button [ onClick Increment ] [ text "++" ]
    ]

helpBanner : Html msg
helpBanner = 
  div 
    [ css 
        [ displayFlex
        , height (pct 10)
        , hover
            [ backgroundColor theme.primary.l1
            ]
        ]
    ]
    [ text "Hi!" ]

puzzleContent : Html msg
puzzleContent = 
  div 
    [ css 
        [ flexGrow (Css.int 1)
        ]
    ] 
  <| List.map infoToText puzzleInfo

puzzleInput : Html msg
puzzleInput = div 
  [ css
      [ displayFlex
      , flexDirection column
      , alignItems center
      , justifyContent center
      , height (pct 20)
      , color theme.white
      , backgroundColor theme.primary.l5
      ]
  ]
  [ div []
      [ text "input player name" ]
  , div []
      [ text "........." ]
  ]

hintSection : Html msg
hintSection = div 
  [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceAround
      , padding2 (rem 0.5) (rem 1.5)
      , height (pct 15)
      ]
  ]
  <| List.map toHtmlCircle ["Hint1", "Hint2", "GiveUp"]

toHtmlCircle : String -> Html msg
toHtmlCircle value = 
  div 
    [ css
        [ borderRadius (pct 50)
        , textTransform uppercase 
        , displayFlex
        , alignItems center
        , justifyContent center
        , maxWidth (px 60)
        , maxHeight (px 60)
        , height (pct 100)
        , flexGrow (Css.int 1)
        , backgroundColor theme.secondary.l3
        ]
    ]
    [ text value ]

infoToText : Info -> Html msg
infoToText info = div []
  [ span [] [ text info.year ]
  , span [] [ text info.team ]
  , span [] [ text info.number ]
  , span [] [ text info.goals ]
  ]

puzzleInfo : List Info
puzzleInfo = 
  [ Info "1995–1997" "Chemnitzer FC II" "18" "(5)"
  , Info "1995–1997" "Chemnitzer FC" "49" "(10)"
  , Info "1997–1998" "1. FC Kaiserslautern II" "17" "(8)"
  , Info "1997–1999" "1. FC Kaiserslautern" "46" "(4)"
  , Info "1999–2002" "Bayer Leverkusen" "79" "(27)"
  , Info "2002–2006" "Bayern Munich" "107" "(44)"
  , Info "2006–2010" "Chelsea" "105" "(17)"
  , Info "2010–2012" "Bayer Leverkusen" "35" "(2)"
  ]

type alias Info =
 { year: String
 , team: String
 , number: String
 , goals: String
 }