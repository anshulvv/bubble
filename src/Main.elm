module Main exposing (..)

import Array exposing (get)
import Browser
import Element
import Element.Background
import Element.Border


type Color
    = Green
    | Red
    | Blue
    | Yellow
    | Brown



-- will include in Color type later
-- | Pink
-- | Violet
-- | Orange


type BubbleState
    = Popped
    | Unpopped


type MatrixState
    = Idle
    | Popping


type alias Bubble =
    { state : BubbleState
    , color : Color
    }


type alias Matrix valType =
    List (List valType)


type alias Model =
    { matrix : Matrix Bubble
    , matrixState : MatrixState
    }


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }


view model =
    Element.layout [] (bubbleGrid model.matrix)


bubbleGrid matrix =
    Element.column [ Element.spacing 3 ] (List.map bubbleGridRow matrix)


bubbleGridRow row =
    Element.row [ Element.spacing 3 ] (List.map bubble row)


bubble bub =
    Element.el
        [ Element.Background.color (getRGB bub.color |> (\( r, g, b ) -> Element.rgb255 r g b))
        , Element.width (Element.px 20)
        , Element.height (Element.px 20)
        , Element.Border.rounded 10
        ]
        Element.none


getRGB : Color -> ( Int, Int, Int )
getRGB color =
    case color of
        Red ->
            ( 255, 0, 0 )

        Blue ->
            ( 0, 0, 255 )

        Green ->
            ( 0, 255, 0 )

        Yellow ->
            ( 0, 125, 125 )

        Brown ->
            ( 0, 0, 0 )


update msg model =
    model


initialMatrix : Matrix Bubble
initialMatrix =
    [ [ { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Brown } ]
    , [ { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Brown }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red } ]
    , [ { state = Unpopped, color = Yellow }, { state = Unpopped, color = Brown }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue } ]
    , [ { state = Unpopped, color = Brown }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow } ]
    , [ { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Brown }, { state = Unpopped, color = Green } ]
    ]


initModel : Model
initModel =
    { matrix = initialMatrix
    , matrixState = Idle
    }
