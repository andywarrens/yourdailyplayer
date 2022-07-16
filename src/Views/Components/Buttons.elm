module Views.Components.Buttons exposing (..)

import Html.Styled.Attributes exposing (css, src, class)
import Css exposing (..)
import Views.Helpers exposing (uncontained, circleIconCss, marg)

roundButton : { compatible | value : String, color : Compatible } -> { a | value : String, color : Compatible } -> Float -> List Style
roundButton textColor bgColor radius =
    [ backgroundColor bgColor
    , cursor pointer 
    , circleIconCss (px radius)
    , fontWeight bold
    , color textColor
    , borderWidth (px 0)
    , position relative
    ]