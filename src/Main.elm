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
import Views.Components.Buttons exposing (roundButton)
import Model exposing (..)
import Views.Helpers exposing (padd, size, Direction(..))
import Views.Helpers exposing (uncontained, circleIconCss, marg)
import Views.Helpers exposing (popup, PopupType(..))
import Html.Styled exposing (Attribute)

main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        }

update : Msg -> Model -> Model
update action old =
  case action of 
    OpenHint x -> { old | showHint = Just x }
    BuyHint {ix, price} -> 
      let
        isHint1Bought = case ix of 
          1 -> True
          _ -> old.isHint1Bought
        isHint2Bought = case ix of 
          1 -> old.isHint2Bought
          _ -> True
      in 
          { old | showHint = Nothing
                , score = old.score - price
                , isHint1Bought = isHint1Bought
                , isHint2Bought = isHint2Bought }
    CloseHint -> { old | showHint = Nothing }
    ToggleHelp isVisible -> { old | showHelp = isVisible }
    GiveUp -> { old | state = GameOver }
    Input key -> 
      let
        userInput = case key of
          (Key a) -> old.userInput ++ a 
          Backspace -> String.dropRight 1 old.userInput
          Enter -> old.userInput
        isFinished = case key of 
          Enter -> True
          _ -> False
        hasWon = isFinished && String.toLower gameModel.answer == String.toLower userInput 
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

-- View functions
-- ----------------------------------------

view : Model -> Html Msg
view model =
  let 
    showGameover = case model.state of
          Playing -> False
          _ -> True
  in
    div 
      [ css 
          [ displayFlex
          , flexDirection column
          ]
      , class "full-page"
      ]
      ([ logoBanner
      , puzzleContent
      , puzzleInput model
      , hintSection model
      -- popups
      , finishedOverlay showGameover model.state
      , helpScreen model.showHelp ]
      ++ (case model.showHint of
            Just x  -> [hintScreen x]
            Nothing -> [])
      )

finishedOverlay : Bool -> GameState -> Html Msg
finishedOverlay isVisible state =
  popup isVisible Finished
    [ h1 [] [ text "Finished!" ]
    , case state of 
        Won -> span [] [ text "You are a winner!" ]
        _   -> span [] [ text "You have lost :(" ]
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
    userAnswer = if model.isHint1Bought then
        let
          fullHiddenAnswer = List.repeat (String.length gameModel.answer) "." |> String.join ""
          hiddenAnswerPart = List.drop guesses (String.toList fullHiddenAnswer)
        in
          model.userInput ++ String.fromList hiddenAnswerPart
      else
        model.userInput
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
      , keyboard
      ]
  
keyboard : Html Msg
keyboard =
  div 
    [ css 
       [ displayFlex
       , property "gap" "0.25rem"
       ]
    ]
  [ div 
      [ css 
        [ displayFlex
        , flexDirection column
        , alignItems center
        , property "gap" "0.25rem"
        ]
      ]
      [ keyboardInput "qwertyuiop"
      , keyboardInput "asdfghjkl"
      , keyboardInput "zxcvbnm"
      , keyboardInputOthers
      ]
  , button 
    [ css [ height (rem 3) ] 
    , onClick (Input Enter)
    ]
    [ text "↩" ]
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

hintSection : Model -> Html Msg
hintSection model = div 
  [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceAround
      , padding2 (rem 0.5) (rem 1.5)
      , height H.appSize.hintBannerHeight
      ]
  ]
   
    [ toHintButton gameModel.hint1 model.isHint1Bought
    , toHintButton gameModel.hint2 model.isHint2Bought
    , giveUpButton
    ]

toHintButton : Hint -> Bool -> Html Msg
toHintButton hint isBought = 
  let
    finalCss = css <|
      (roundButton 
        white 
        (if isBought then theme.primary.l1 else theme.primary.l5)
        H.appSize.hintBannerHeight.numericValue)
      ++ 
        (if isBought then [] else [ cursor pointer ])
    in
      if isBought then
        button [ finalCss ] [ span [] [ text "Hint" ] ]
      else
        let
          hintBoughtLabel = case hint.hintType of
            CharCount x -> String.fromInt x
            Nationality x -> x
          textEl = span 
            [ css [ color theme.gray.l2 ] ]
            [ text hintBoughtLabel ]
        in
          button 
            ( finalCss :: [ onClick (OpenHint hint) ] )
            [ textEl ]

giveUpButton = 
  let
    finalCss = css <|
      (roundButton 
        white 
        theme.secondary.l3 
        H.appSize.hintBannerHeight.numericValue)
  in
    button 
      ( finalCss :: [onClick GiveUp] )
      [ text "GiveUp" ]

helpScreen : Bool -> Html Msg
helpScreen isVisible =
   popup 
    isVisible
    Informative
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
   

hintScreen : Hint -> Html Msg
hintScreen hint =
  let 
    uncontainedBtn = styled button 
      [ uncontained
      , fontSize (rem <| size 4)
      , height (px 40)
      , hover
          [ border3 (px 1) solid black ]
      ]
    hintType = hint.hintType
    (active, question) = case hintType of
        (CharCount _) -> (True, "Wanna know the total characters in your player’s name?")
        (Nationality _) -> (True, "Wanna know your player’s nationality?")
  in
    popup active Question
      [ h2 
        [ css 
            [ fontSize (rem <| size 4 )  
            , color white
            , backgroundColor theme.primary.l5
            , property "grid-column" "1 / 3"
            , marg H.Bottom 4
            ]
        ]
        [ text question]
      , uncontainedBtn 
          [ css [ color theme.secondary.l3 ]
          , onClick (BuyHint hint)
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

type alias Info =
 { year: String
 , team: String
 , number: String
 , goals: String
 }
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
