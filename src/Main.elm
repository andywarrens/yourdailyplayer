module Main exposing (main)

import Html.Styled exposing (
  div, h1, h2, h3, button, img, span, strong, caption)
import Html.Styled exposing (
  Html, styled, toUnstyled, text)
import Html.Styled.Attributes exposing (css, src, class)
import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import Browser

import Views.Helpers as H
import Model exposing (..)
import Views.Helpers exposing (padd, size, Direction(..))
import Views.Helpers exposing (uncontained, circleIconCss, marg)


type alias Model =
  { userInput: String
  , state   : GameState
  , score   : Int
  , guesses : List String
  , hint1   : HintModel
  , showHint : Bool
  }
type alias HintModel = { active: Bool }
type GameState = Playing | GameOver | Won

gameModel : { answer : String, nGuesses : number, initialScore : Int }
gameModel = 
  { answer   = "Andy"
  , nGuesses = 5
  , initialScore = 10
  }

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
    updateHint oldHint value = { oldHint | active = value }
  in
    case action of 
      OpenHint -> { old | hint1 = updateHint old.hint1 True }
      CloseHint -> { old | hint1 = updateHint old.hint1 False }
      ToggleHelp isVisible -> { old | showHint = isVisible }
      _ -> 
        let
          userInput = case action of
            Input (Key a) -> old.userInput ++ a 
            Input Backspace -> String.dropRight 1 old.userInput
            _ -> old.userInput
          isFinished = (String.length userInput) == (String.length gameModel.answer)
          hasWon = String.toLower gameModel.answer == String.toLower userInput 
          newScore = if isFinished && not hasWon then old.score - 1 else old.score
          newState : GameState
          newState =
            if hasWon
              then Won
            else 
              if (isFinished && newScore == 0)
                then GameOver 
                else Playing
          newGuesses = if isFinished && newState == Playing 
            then userInput :: old.guesses
            else old.guesses
        in
          { old |
            userInput = if newState == Playing && isFinished then "" else userInput
          , state = newState
          , score = newScore
          , guesses = newGuesses
          }

initialModel : Model
initialModel = Model 
  "An" 
  Playing 
  gameModel.initialScore 
  [] 
  { active = False }
  False

view : Model -> Html Msg
view model =
  div 
    [ css 
        [ displayFlex
        , flexDirection column
        ]
    , class "full-page"
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
      -- popups
      , helpScreen model.showHint
      , hintScreen model.hint1
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

logoBanner : Html Msg
logoBanner = 
  div 
    [ 
      css 
        [ property "display" "grid"
        , H.gridTemplateColumns [ (H.Pct (pct 40))
                              , H.Auto
                              , (H.Pct (pct 40)) ]
        , property "justify-items" "center"
        , padd H.Y 2
        , alignItems center
        , height H.appSize.logoBannerHeight
        , boxSizing borderBox
        ]
    ]
    [ yourDailyPlayerText
    , helpButton
    , fbPlayerLogo
    ]


fbPlayerLogo : Html msg
fbPlayerLogo =
    div 
      [ css [ padd H.Right 1 ] 
      ]
      [ img 
        [ src "/fbplayer2.png" 
        , css [ width (pct 100) ]
        ] 
        []
      ]

helpButton : Html Msg
helpButton =
    button
      [ onClick (ToggleHelp True)
      , css
        [ uncontained
        , circleIconCss (px 30)
        , backgroundColor theme.primary.l5
        , color white
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
      [ div 
          [ css [ color theme.secondary.l3, textAlign center ]] 
          [ text "Your Daily"]
      , div [ css 
              [ color white
              , backgroundColor theme.primary.l5
              , textAlign center
              , padd H.X 4 ]
              ] 
              [ text "Player"]
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
        , H.gridTemplateColumns [ (H.Pct (pct 35))
                              , (H.Pct (pct 50))
                              , H.Fr
                              , H.Fr ]
        , padd H.X 2
        , padd H.Bottom 5
        , overflowY auto
        ]
    ] 
    <| List.concatMap infoToText puzzleInfo

puzzleInput : Model -> Html Msg
puzzleInput model = 
  let
    guesses = String.length model.userInput
    fullHiddenAnswer = List.range 0 (String.length gameModel.answer)
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
          , color white
          , backgroundColor theme.primary.l5
          , padd H.Y 2
          , marg H.Top 2
          , position relative
          ]
      ]
      [ scoreCircle model
      , h3 [ css [ marg (H.XY 2) 0 ] ] [ text "Who's your daily football player?" ]
      , div [ css [ marg H.Bottom 2 ]] [ text userAnswer ]
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

scoreCircle : Model -> Html a
scoreCircle { score } =
  h3 
    [ css
      [ circleIconCss H.appSize.scoreCircleDiam
      , backgroundColor theme.secondary.l3
      , position absolute
      , right (px 0)
      , top   (px (-0.75 * H.appSize.scoreCircleDiam.numericValue))
      , marg (H.XY 0) 0
      , marg H.Right 3
      , zIndex <| Css.int 1
      , textTransform uppercase 
      , fontSize (rem <| size 4)
      ]
    ]
    [ text <| String.fromInt score ]



hintSection : Html Msg
hintSection = div 
  [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceAround
      , padding2 (rem 0.5) (rem 1.5)
      , height H.appSize.hintBannerHeight
      ]
  ]
  <| List.indexedMap toHtmlCircle 
    [ HintButton
    , HintButton
    , GiveUpButton
    ]

type HintSectionButton = HintButton | GiveUpButton

toHtmlCircle : Int -> HintSectionButton -> Html Msg
toHtmlCircle ix buttonP = 
  let
    (isGiveUpBtn, value) = case buttonP of
      HintButton -> (False, "Hint")
      GiveUpButton -> (True, "Give Up")
    textEl = 
      span [] [ text value ]
  in
    button 
      [ css
          [ circleIconCss 
              (px (H.appSize.hintBannerHeight.numericValue))
          , fontWeight bold
          , backgroundColor theme.primary.l5
          , color (if isGiveUpBtn then theme.secondary.l3 else white)
          , borderWidth (px 0)
          , cursor pointer
          ]
      , onClick OpenHint
      ]
      [ textEl ]

helpScreen : Bool -> Html Msg
helpScreen isVisible =
   H.popup 
    isVisible
    H.Info
      [ h2 
        [ css 
            [ fontSize (rem <| size 4 )  
            , color theme.secondary.l3
            , property "grid-column" "1 / 3"
            , marg H.Bottom 4
            ]
        ]
        [ text "HOW TO PLAY" ]
      , div 
          [ css
            [
              color white
            , property "grid-column" "1 / 3"
            ]
          ]
          <|
            List.map 
              (text >> List.singleton >> div [ css [ marg Bottom 1 ]])
              [ "One day, one player", "Cheating is for losers", "You start with 10 points.", "Each wrong guess will deduct 1 point from your total score", "You can buy a hint, it will cost you 2 points", "Share your final score with your friends", "Click on your player’s name to learn more about him/her.", "Sometimes stats are not 100% accurate (especially for players still playing),. We do apologise but it’s just a game so get over it please ;-)" ]
      ]
   

hintScreen : HintModel -> Html Msg
hintScreen { active } =
  let 
    uncontainedBtn = styled button 
      [ uncontained
      , fontSize (rem <| size 4)
      , height (px 40)
      , hover
          [ border3 (px 1) solid black ]
      ]
  in
    H.popup active H.Question
      [ h2 
        [ css 
            [ fontSize (rem <| size 4 )  
            , color white
            , backgroundColor theme.primary.l5
            , property "grid-column" "1 / 3"
            , marg H.Bottom 4
            ]
        ]
        [ text "Wanna know the total characters in your player’s name?" ]
      , uncontainedBtn 
          [ css [ color theme.secondary.l3 ]
          , onClick CloseHint
          ]
          [ text "YES" ]
      , uncontainedBtn 
          [ css [ color theme.primary.l5 ] 
          , onClick CloseHint
          ]
          [ text "NO" ]
      , caption [ css [ color theme.secondary.l3 ]] [ text "-2 points" ]
      ]

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