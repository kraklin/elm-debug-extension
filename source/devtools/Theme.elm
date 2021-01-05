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
    , numbersColor : Css.Color
    , customTypesColor : Css.Color
    , booleanColor : Css.Color
    , sequenceNameColor : Css.Color
    }


{-| based on BluLoco Light : <https://github.com/uloco/theme-bluloco-light>
-}
lightTheme : ThemeColors
lightTheme =
    { background = Css.hex "f9f9f9"
    , foreground = Css.hex "383a42"
    , buttonBackground = Css.hex "f0f0f0"
    , buttonForeground = Css.hex "383a42"
    , primary = Css.hex "ff00ff"
    , panelBackground = Css.hex "ffffff"
    , stringColor = Css.hex "c5a332"
    , internalsColor = Css.hex "a0a1a7"
    , keysColor = Css.hex "a05a48"
    , guidelinesColor = Css.hex "a0a1a7"
    , expandTriangleColor = Css.hex "7a82da"
    , valueBackgroundColor = Css.hex "f9f9f9"
    , numbersColor = Css.hex "ce33c0"
    , customTypesColor = Css.hex "d52753"
    , booleanColor = Css.hex "0098dd"
    , sequenceNameColor = Css.hex "23974a"
    }


{-| based on BluLoco dark : <https://github.com/uloco/theme-bluloco-dark>
-}
darkTheme : ThemeColors
darkTheme =
    { background = Css.hex "282c34"
    , foreground = Css.hex "abb2bf"
    , buttonBackground = Css.hex "303030"
    , buttonForeground = Css.hex "F8F8F2"
    , primary = Css.hex "ff00ff"
    , panelBackground = Css.rgba 255 255 255 0.1
    , stringColor = Css.hex "f9c859"
    , internalsColor = Css.hex "636d83"
    , keysColor = Css.hex "ce9887"
    , guidelinesColor = Css.hex "636d83"
    , expandTriangleColor = Css.hex "7a82da"
    , valueBackgroundColor = Css.hex "282c34"
    , numbersColor = Css.hex "ff78f8"
    , customTypesColor = Css.hex "ff6480"
    , booleanColor = Css.hex "10b1fe"
    , sequenceNameColor = Css.hex "3fc56b"
    }


themeColors : Theme -> ThemeColors
themeColors theme =
    if theme == Dark then
        darkTheme

    else
        lightTheme
