module Main exposing (..)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Events


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


type Msg
    = ClickedBubble Int Int


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }


view model =
    Element.layout [] (bubbleGrid model.matrix)


bubbleGrid matrix =
    Element.column [ Element.spacing 3 ] (List.indexedMap bubbleGridRow matrix)


bubbleGridRow x row =
    Element.row [ Element.spacing 3 ] (List.indexedMap (\ind bub -> bubble bub x ind) row)


bubble bub x y =
    Element.el
        [ Element.Background.color (getRgbColor bub |> (\( r, g, b ) -> Element.rgb255 r g b))
        , Element.width (Element.px bubbleDiameter)
        , Element.height (Element.px bubbleDiameter)
        , Element.Border.rounded 10
        , Element.Events.onClick (ClickedBubble x y)
        ]
        Element.none


bubbleDiameter : Int
bubbleDiameter =
    30


getRgbColor : Bubble -> ( Int, Int, Int )
getRgbColor bub =
    case bub.state of
        Popped ->
            ( 255, 255, 255 )

        Unpopped ->
            case bub.color of
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedBubble x y ->
            let
                newMatrix : Matrix Bubble
                newMatrix =
                    List.indexedMap
                        (\indexRow row ->
                            if indexRow == x then
                                List.indexedMap
                                    (\indexCol bub ->
                                        if indexCol == y then
                                            { bub | state = Popped }

                                        else
                                            bub
                                    )
                                    row

                            else
                                row
                        )
                        model.matrix
            in
            { model | matrix = newMatrix }



-- case msg of
--     ClickedBubble x y ->
--         List.indexedMap
--         (\indexRow row ->
--             if indexRow == x then
--                 (\indexCol bubble ->
--                     if indexCol == y then
--                         {model | matrix = popBubble bubble
--                 )
--                 row
--         )
--         model.matrix


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
