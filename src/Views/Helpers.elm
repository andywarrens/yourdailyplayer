module Views.Helpers exposing (..)

import Html.Styled exposing (div, button)
import Html.Styled exposing (Html, text)
import Html.Styled.Attributes exposing (css, class)
import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import Css.Transitions as T

import Model exposing (Msg, theme)
import Model exposing (Msg(..))
import Model exposing (white)

type Direction = 
    Top | Right | Bottom | Left 
  | X | Y | XY Int

type CssGridSize =
    Rem Rem
  | Pct Pct
  | Px Px
  | Fr
  | Auto

toStyle : CssGridSize -> String
toStyle sizeP =
    case sizeP of
      Rem rem -> (.value rem)
      Pct pct -> (.value pct)
      Px px -> (.value px)
      Fr -> "1fr"
      Auto -> "auto"
appSize : 
  { logoBannerHeight : Px
  , hintBannerHeight : Px 
  , scoreCircleDiam  : Px 
  }
appSize = 
  { logoBannerHeight = (px 80)
  , hintBannerHeight = (px 55)
  , scoreCircleDiam  = (px (55 * 1.25))
  }

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


type PopupType = Info | Question
popup : Bool -> PopupType -> List (Html Msg) -> Html Msg
popup isVisible popupType content = 
  let
    layout = case popupType of 
      Question -> Css.batch
        [ property "display" "grid"
        , gridTemplateColumns [ Fr, Fr ]
        , gridTemplateRows [ Auto, Auto, Fr ]
        , property "justify-items" "center"
        , property "align-items" "start"
        ]
      _ -> Css.batch []
    closeButton = 
      button [ css 
                [ position absolute
                , top (px 0)
                , right (px 0)
                , uncontained
                , backgroundColor transparent
                , color white
                , fontSize (rem <| size 3)
                , width (rem 3)
                , height (rem 3)
                , displayFlex
                , alignItems center
                , justifyContent center
                ]
             , onClick (ToggleHelp False)
             ]
             [ text "X" ]
    offscreenX = case popupType of
      Info -> -100
      Question -> 100
  in
    div 
      [ css
        [ position absolute
        , transform <| translate (vw <| if isVisible then 0 else offscreenX)
        , zIndex (Css.int 2)
        , top (px 0)
        , paddingTop appSize.logoBannerHeight
        , padd X 2
        , layout
        , boxSizing borderBox
        , textAlign center
        , case popupType of 
            Info -> backgroundColor theme.primary.l5
            Question -> backgroundColor (hsla 0 0 1 0.9)
        , T.transition
            [ T.transform3 200 0 T.linear
            ]
        ]
        , class "full-page"
      ]
      ( case popupType of
          Info -> closeButton :: content
          Question -> content
      )

padd : Direction -> Int -> Style
padd dir sizeP = 
  let  
    cssFun = case dir of
      Top   -> [ paddingTop ]
      Right -> [ paddingRight ]
      Bottom -> [ paddingBottom ]
      Left -> [ paddingLeft ]
      X   -> [ paddingLeft, paddingRight ]
      Y   -> [ paddingTop, paddingBottom ]
      XY _ -> []
  in
    Css.batch <| 
      case dir of 
        XY xSize -> [ padding2 (rem <| toFloat xSize) (rem <| toFloat sizeP) ]
        _  -> List.map (\t -> t (rem <| size sizeP)) cssFun

size : Int -> Float
size sizeP = case sizeP of
    0 -> 0
    1 -> 0.25
    2 -> 0.5
    3 -> 1
    4 -> 1.5
    _ -> 3

uncontained : Style
uncontained =
    Css.batch 
      [ border (px 0)
      , backgroundColor white
      ]

circleIconCss : ExplicitLength unit -> Style
circleIconCss circleDiam =
  Css.batch
    [ borderRadius (pct 50)
    , width <| circleDiam
    , height <| circleDiam
    , textTransform uppercase 
    , textAlign center
    , property "display" "grid"
    , property "align-content" "center"
    , property "justify-items" "center"
    ]

marg : Direction -> Int -> Style
marg dir sizeP = 
  let  
    cssFun = case dir of
      Top   -> [ marginTop ]
      Right -> [ marginRight ]
      Bottom -> [ marginBottom ]
      Left -> [ marginLeft ]
      X -> [ marginLeft, marginRight ]
      Y -> [ marginTop, marginBottom ]
      XY _ -> []
  in
    Css.batch <| 
      case dir of 
        XY xSize -> [ margin2 (rem <| size xSize) (rem <| size sizeP) ]
        _  -> List.map (\t -> t (rem <| size sizeP)) cssFun