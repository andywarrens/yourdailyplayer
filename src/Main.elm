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
logoBannerHeight : Px
logoBannerHeight = (px 65)

type Padding
  = Px0 | Px1 | Px2 | Px3 | Px4 | Px5
  | Py0 | Py1 | Py2 | Py3 | Py4 | Py5
padding : Padding -> Style
padding a = 
  Css.batch <|
    case a of
      Px0 -> [ paddingLeft (rem 0   ), paddingRight (rem 0   ) ]
      Px1 -> [ paddingLeft (rem 0.25), paddingRight (rem 0.25) ]
      Px2 -> [ paddingLeft (rem 0.5 ), paddingRight (rem 0.5 ) ]
      Px3 -> [ paddingLeft (rem 1   ), paddingRight (rem 1   ) ]
      Px4 -> [ paddingLeft (rem 1.5 ), paddingRight (rem 1.5 ) ]
      Px5 -> [ paddingLeft (rem 3   ), paddingRight (rem 3   ) ]
      Py0 -> [ paddingTop (rem 0   ), paddingBottom (rem 0   ) ]
      Py1 -> [ paddingTop (rem 0.25), paddingBottom (rem 0.25) ]
      Py2 -> [ paddingTop (rem 0.5 ), paddingBottom (rem 0.5 ) ]
      Py3 -> [ paddingTop (rem 1   ), paddingBottom (rem 1   ) ]
      Py4 -> [ paddingTop (rem 1.5 ), paddingBottom (rem 1.5 ) ]
      Py5 -> [ paddingTop (rem 3   ), paddingBottom (rem 3   ) ]

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
    [ logoBanner
    , puzzleContent
    , puzzleInput
    , hintSection
--    , button [ onClick Decrement ] [ text "--" ]
--    , div [] [ text (String.fromInt model) ]
--    , button [ onClick Increment ] [ text "++" ]
    ]

logoBanner : Html msg
logoBanner = 
  div 
    [ 
      css 
        [ property "display" "grid"
        , property "grid-template-columns" "45% auto 45%"
        , property "justify-items" "center"
        , padding Py2
        , alignItems center
        , height logoBannerHeight
        , hover
            [ backgroundColor theme.primary.l1
            ]
        ]
    ]
    [ yourDailyPlayerText
    , helpButton
    , fbPlayerLogo
    ]


fbPlayerLogo : Html msg
fbPlayerLogo =
    div 
      []
      [ img 
        [ src "/fbplayer2.png" 
        , css 
            [ width auto 
            , height logoBannerHeight
            ]
        ] [] ]

helpButton : Html msg
helpButton =
    div 
      [ css
        [ borderRadius (pct 50)
        , displayFlex
        , alignItems center
        , justifyContent center
        , width (px 30)
        , height (px 30)
        , backgroundColor theme.primary.l4
        , color theme.white
        ]
      ]
      [ text "?" ]


yourDailyPlayerText : Html msg
yourDailyPlayerText =
    div 
      [ css
        [ transform (rotate (deg -30))
        ]
      ]
      [ div [ css [ color theme.secondary.l3, textAlign center ]] [ text "Your Daily"]
      , div [ css [ color theme.white, backgroundColor theme.primary.l4, textAlign center, padding Px4 ]] [ text "Player"]
      ]

  

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