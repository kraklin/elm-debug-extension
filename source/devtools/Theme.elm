module Theme exposing (..)

import Css


type Theme
    = Light
    | Dark


type alias ThemeColors =
    { background : Css.Color
    , foreground : Css.Color
    , buttonBackground : Css.Color
    , buttonForeground : Css.Color
    , primary : Css.Color
    , panelBackground : Css.Color
    , stringColor : Css.Color
    , internalsColor : Css.Color
    , keysColor : Css.Color
    , guidelinesColor : Css.Color
    , expandTriangleColor : Css.Color
    , valueBackgroundColor : Css.Color
    }


lightTheme : ThemeColors
lightTheme =
    { background = Css.hex "ffffff"
    , foreground = Css.hex "000000"
    , buttonBackground = Css.hex "f0f0f0"
    , buttonForeground = Css.hex "000000"
    , primary = Css.hex "ff00ff"
    , panelBackground = Css.rgba 0 0 0 0.03
    , stringColor = Css.hex "0000ff"
    , internalsColor = Css.hex "808080"
    , keysColor = Css.hex "ff00ff"
    , guidelinesColor = Css.hex "a0a0a0"
    , expandTriangleColor = Css.hex "808080"
    , valueBackgroundColor = Css.hex "ffffff"
    }


themeColors : Theme -> ThemeColors
themeColors theme =
    if theme == Dark then
        { lightTheme
            | background = Css.hex "0f0f0f"
            , foreground = Css.hex "F8F8F2"
            , buttonBackground = Css.hex "303030"
            , buttonForeground = Css.hex "F8F8F2"
            , panelBackground = Css.rgba 255 255 255 0.1
            , stringColor = Css.hex "E6DB74"
            , internalsColor = Css.hex "808080"
            , keysColor = Css.hex "F92672"
            , guidelinesColor = Css.hex "AE81FF"
            , expandTriangleColor = Css.hex "F8F8F0"
            , valueBackgroundColor = Css.hex "0f0f0f"
        }

    else
        lightTheme
