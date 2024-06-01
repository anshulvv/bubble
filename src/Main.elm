module Main exposing (..)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Html exposing (b)
import Set


type Color
    = Green
    | Red
    | Blue
    | Yellow
    | Black



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
    Element.row [ Element.spacing 3 ] (List.indexedMap (viewBubble x) row)


viewBubble x y bubble =
    Element.el
        [ Element.Background.color (getRgbColor bubble |> (\( r, g, b ) -> Element.rgb255 r g b))
        , Element.width (Element.px bubbleDiameter)
        , Element.height (Element.px bubbleDiameter)
        , Element.Border.rounded 20
        , Element.Events.onClick (ClickedBubble x y)
        ]
        Element.none


bubbleDiameter : Int
bubbleDiameter =
    30


getRgbColor : Bubble -> ( Int, Int, Int )
getRgbColor bubble =
    case bubble.state of
        Popped ->
            ( 255, 255, 255 )

        Unpopped ->
            case bubble.color of
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedBubble x y ->
            let
                targetBubble : Maybe Bubble
                targetBubble =
                    getBubble x y model.matrix

                newMatrix =
                    case targetBubble of
                        Just bubble ->
                            bfsBubble x y model.matrix bubble.color |> updateBubbles

                        Nothing ->
                            model.matrix
            in
            { model | matrix = newMatrix }


getBubble : Int -> Int -> Matrix Bubble -> Maybe Bubble
getBubble x y matrix =
    List.drop x matrix
        |> List.head
        |> Maybe.andThen (List.drop y >> List.head)


bfsBubble x y matrix color =
    let
        queue =
            [ ( x, y ) ]

        visited =
            Set.empty
    in
    bfsHelper queue visited matrix color


bfsHelper queue visited matrix color =
    case queue of
        [] ->
            matrix

        ( row, col ) :: rest ->
            if Set.member ( row, col ) visited then
                bfsHelper rest visited matrix color

            else
                let
                    bubble =
                        getBubble row col matrix

                    updatedVisited =
                        Set.insert ( row, col ) visited

                    newQueue =
                        rest ++ List.filter (\( r, c ) -> isValidBubble r c matrix && isSameColor r c matrix color) (neighbors row col)

                    newMatrix =
                        case bubble of
                            Just _ ->
                                changeBubbleState row col matrix

                            Nothing ->
                                matrix
                in
                bfsHelper newQueue updatedVisited newMatrix color


isValidBubble : Int -> Int -> Matrix Bubble -> Bool
isValidBubble r c matrix =
    r
        >= 0
        && r
        < List.length matrix
        && c
        >= 0
        && c
        < (List.length <| Maybe.withDefault [] <| List.head matrix)


isSameColor : Int -> Int -> Matrix Bubble -> Color -> Bool
isSameColor r c matrix color =
    case getBubble r c matrix of
        Just bub ->
            bub.color == color && bub.state == Unpopped

        Nothing ->
            False


neighbors : Int -> Int -> List ( Int, Int )
neighbors r c =
    [ ( r + 1, c )
    , ( r - 1, c )
    , ( r, c + 1 )
    , ( r, c - 1 )
    ]


changeBubbleState : Int -> Int -> Matrix Bubble -> Matrix Bubble
changeBubbleState r c matrix =
    List.indexedMap
        (\ir row ->
            if ir == r then
                List.indexedMap
                    (\ic bubble ->
                        if ic == c then
                            { bubble | state = Popped }

                        else
                            bubble
                    )
                    row

            else
                row
        )
        matrix


movePoppedBubblesUpwards column =
    let
        ( popped, unpopped ) =
            List.partition (\bubble -> bubble.state == Popped) column
    in
    popped ++ unpopped


updateBubbles : Matrix Bubble -> Matrix Bubble
updateBubbles matrix =
    let
        columns =
            transpose matrix

        newColumns =
            List.map movePoppedBubblesUpwards columns
    in
    transpose newColumns


initialMatrix : Matrix Bubble
initialMatrix =
    [ [ { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Black } ]
    , [ { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Green }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red } ]
    , [ { state = Unpopped, color = Yellow }, { state = Unpopped, color = Green }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue } ]
    , [ { state = Unpopped, color = Black }, { state = Unpopped, color = Green }, { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow } ]
    , [ { state = Unpopped, color = Red }, { state = Unpopped, color = Blue }, { state = Unpopped, color = Yellow }, { state = Unpopped, color = Black }, { state = Unpopped, color = Green } ]
    ]


initModel : Model
initModel =
    { matrix = initialMatrix
    , matrixState = Idle
    }



--- HELPERS


transpose : Matrix a -> Matrix a
transpose matrix =
    case matrix of
        [] ->
            []

        [] :: _ ->
            []

        _ ->
            let
                heads : List a
                heads =
                    List.map List.head matrix |> List.filterMap identity

                tails : List (List a)
                tails =
                    List.map List.tail matrix |> List.filterMap identity
            in
            heads :: transpose tails


identity : Maybe a -> Maybe a
identity val =
    val
