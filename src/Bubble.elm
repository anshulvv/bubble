module Bubble exposing
    ( Bubble
    , BubbleState(..)
    , Color(..)
    , bubbleDiameter
    , getRgbColor
    )


type alias Bubble =
    { state : BubbleState
    , color : Color
    }


type BubbleState
    = Popped
    | Unpopped


type Color
    = Green
    | Red
    | Blue
    | Yellow
    | Black
    | Grey
    | NoColor


bubbleDiameter : Int
bubbleDiameter =
    30


getRgbColor : Color -> ( Int, Int, Int )
getRgbColor color =
    case color of
        Red ->
            ( 255, 0, 0 )

        Blue ->
            ( 0, 0, 255 )

        Green ->
            ( 0, 255, 0 )

        Yellow ->
            ( 0, 125, 125 )

        Black ->
            ( 0, 0, 0 )

        Grey ->
            ( 150, 150, 150 )

        NoColor ->
            ( 255, 255, 255 )
