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

appSize : { logoBannerHeight : Px, hintBannerHeight : Px }
appSize = 
  { logoBannerHeight = (px 65)
  , hintBannerHeight = (px 55)
  }

type Direction = 
    Top | Right | Bottom | Left 
  | X | Y
size : Int -> Float
size sizeP = case sizeP of
    0 -> 0
    1 -> 0.25
    2 -> 0.5
    3 -> 1
    4 -> 1.5
    _ -> 3
padding : Direction -> Int -> Style
padding dir sizeP = 
  let  
    cssFun = case dir of
      Top   -> [ paddingTop ]
      Right -> [ paddingRight ]
      Bottom -> [ paddingBottom ]
      Left -> [ paddingLeft ]
      X -> [ paddingLeft, paddingRight ]
      Y -> [ paddingTop, paddingBottom ]
  in
    Css.batch <| List.map (\t -> t (rem <| size sizeP)) cssFun

margin : Direction -> Int -> Style
margin dir sizeP = 
  let  
    cssFun = case dir of
      Top   -> [ marginTop ]
      Right -> [ marginRight ]
      Bottom -> [ marginBottom ]
      Left -> [ marginLeft ]
      X -> [ marginLeft, marginRight ]
      Y -> [ marginTop, marginBottom ]
  in
    Css.batch <| List.map (\t -> t (rem <| size sizeP)) cssFun



type CssGridSize =
  --  Rem (Css.ExplicitLength a)
    Rem Rem
  | Pct Pct
  | Fr
  | Auto

toStyle : CssGridSize -> String
toStyle sizeP =
    case sizeP of
      Rem rem -> (.value rem)
      Pct pct -> (.value pct)
      Fr -> "1fr"
      Auto -> "auto"

gridTemplateColumns : List (CssGridSize) -> Style
gridTemplateColumns sizes =
  property "grid-template-columns" 
    <| String.join " " 
    <| List.map toStyle sizes

gridTemplateRows : List (CssGridSize) -> Style
gridTemplateRows sizes =
  property "grid-template-rows" 
    <| String.join " " 
    <| List.map toStyle sizes



type alias Model =
  { answer: String
  , userInput: String
  , state : GameState
  }
type InputType = Key String | Backspace
type Msg = Input InputType
type GameState = Playing | GameOver | Won

main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        }

update : Msg -> Model -> Model
update action old =
  let
      userInput = case action of
        Input (Key a) -> old.userInput ++ a 
        Input Backspace -> String.dropRight 1 old.userInput
      isFinished = (String.length userInput) == (String.length old.answer)
      newState : GameState
      newState =
        if old.answer == userInput 
          then Won
        else 
          if isFinished 
            then GameOver 
            else Playing

  in
    { userInput = userInput
    , answer = old.answer
    , state = newState
    }

initialModel : Model
initialModel = Model "Andy" "And" Playing

view : Model -> Html Msg
view model =
  div 
    [ css 
        [ displayFlex
        , flexDirection column
        ]
    , class "viewport"
    ]
    <|
      ( case model.state of
          Playing -> []
          Won -> [ finishedOverlay IsWinner ]
          GameOver -> [ finishedOverlay IsLoser ]
      )
      ++
      [ logoBanner
      , puzzleContent
      , puzzleInput model
      , hintSection
      ]

type GameFinished = IsWinner | IsLoser
finishedOverlay : GameFinished -> Html a
finishedOverlay state =
  div 
    [ css 
      [ position absolute
      , top (pct 10)
      , left (pct 5)
      , backgroundColor (rgba 255 255 255 0.9)
      , border3 (px 1) (solid) (hex "000000")
      , height (pct 80)
      , width (pct 90)
      , zIndex (int 5)
      , displayFlex
      , flexDirection column
      , alignItems center
      , justifyContent center
      ]
    ]
    [ h1 [] [ text "Finished!" ]
    , case state of 
        IsWinner  -> span [] [ text "You are a winner!" ]
        IsLoser  -> span [] [ text "You have lost :(" ]
    ]

logoBanner : Html msg
logoBanner = 
  div 
    [ 
      css 
        [ property "display" "grid"
        , gridTemplateColumns [ (Pct (pct 45))
                              , Auto
                              , (Pct (pct 45)) ]
        , property "justify-items" "center"
        , padding Y 2
        , alignItems center
        , height appSize.logoBannerHeight
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
            , height appSize.logoBannerHeight
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
      , div [ css [ color theme.white, backgroundColor theme.primary.l4, textAlign center, padding X 4 ]] [ text "Player"]
      ]

  

puzzleContent : Html msg
puzzleContent = 
  div 
    [ css 
        [ flexGrow (Css.int 1)
        , backgroundImage (url "/fbsilhouette.png")
        , backgroundPosition center
        , backgroundSize2 auto (pct 100)
        , backgroundRepeat noRepeat
        , property "display" "grid"
        , gridTemplateColumns [ (Pct (pct 35))
                              , (Pct (pct 50))
                              , Fr
                              , Fr ]
        , padding X 2
        , overflowY auto
        ]
    ] 
    <| List.concatMap infoToText puzzleInfo

puzzleInput : Model -> Html Msg
puzzleInput model = 
  let
    guesses = String.length model.userInput
    fullHiddenAnswer = List.range 0 (String.length model.answer)
      |> List.map (\_ -> "") 
      |> String.join "."
    hiddenAnswerPart = List.drop guesses (String.toList fullHiddenAnswer)
    userAnswer = model.userInput ++ String.fromList hiddenAnswerPart
  in
    div 
      [ css
          [ displayFlex
          , flexDirection column
          , alignItems center
          , justifyContent center
          , color theme.white
          , backgroundColor theme.primary.l5
          , padding Y 2
          , margin Top 2
          ]
      ]
      [ div []
          [ text "Who's your daily football player?" ]
      , div [] [ text userAnswer ]
      , keyboardInput "qwertyuiop"
      , keyboardInput "asdfghjkl"
      , keyboardInput "zxcvbnm"
      , keyboardInputOthers
      ]


keyboardInput : String -> Html Msg
keyboardInput letters =
  div []
    <| String.foldr 
        (\letter xs -> button [ onClick (Input (Key (String.fromChar letter))) ] [ text <| String.fromChar letter ] :: xs )
        []
        letters

keyboardInputOthers : Html Msg
keyboardInputOthers = div 
  [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceAround
      ]
  ]
  [ button [ onClick (Input (Key " "))] [ text "space" ]
  , button [ onClick (Input (Backspace))] [ text "🔙" ]
  ]


hintSection : Html msg
hintSection = div 
  [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceAround
      , padding2 (rem 0.5) (rem 1.5)
      , height appSize.hintBannerHeight
      ]
  ]
  <| List.indexedMap toHtmlCircle 
    [ HintButton
    , HintButton
    , GiveUpButton
    ]

type HintSectionButton = HintButton | GiveUpButton

toHtmlCircle : Int -> HintSectionButton -> Html msg
toHtmlCircle ix buttonP = 
  let
    (isGiveUpBtn, value) = case buttonP of
      HintButton -> (False, "Hint")
      GiveUpButton -> (True, "Give Up")
    textEl = 
      span [] [ text value ]
      :: if isGiveUpBtn then [] else [ span [] [ text <| String.fromInt ix ] ]
  in
    button 
      [ css
          [ borderRadius (pct 50)
          , textTransform uppercase 
          , width <| px (appSize.hintBannerHeight.numericValue)
          , height <| px (appSize.hintBannerHeight.numericValue)
          , textAlign center
          , property "display" "grid"
          , property "align-content" "center"
          , property "justify-items" "center"
          , fontWeight bold
          , backgroundColor theme.primary.l5
          , color (if isGiveUpBtn then theme.secondary.l3 else theme.white)
          , borderWidth (px 0)
          , cursor pointer
          ]
      ]
      textEl

infoToText : Info -> List (Html msg)
infoToText info = 
  [ span [] [ text info.year ]
  , strong [] [ text info.team ]
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