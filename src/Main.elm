module Main exposing (..)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Html
import Random
import Set
import Simple.Transition as Transition


type Color
    = Green
    | Red
    | Blue
    | Yellow
    | Black
    | NoColor --white



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
    , rows : Int
    , columns : Int
    }


type Msg
    = ClickedBubble Int Int
    | GeneratedRandomMatrix (Matrix Bubble)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, randomMatrix initModel.rows initModel.columns |> Random.generate GeneratedRandomMatrix )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


randomMatrix : Int -> Int -> Random.Generator (Matrix Bubble)
randomMatrix r c =
    Random.map (\color -> { state = Unpopped, color = color }) randomColor
        |> Random.list c
        |> Random.list r


randomColor : Random.Generator Color
randomColor =
    Random.uniform Red [ Green, Blue, Yellow, Black ]


view : Model -> Html.Html Msg
view model =
    Element.layout [] (bubbleGrid model.matrix)


bubbleGrid : Matrix Bubble -> Element.Element Msg
bubbleGrid matrix =
    let
        viewBubble : Int -> Int -> Bubble -> Element.Element Msg
        viewBubble x y bubble =
            Element.el
                [ Element.Events.onClick (ClickedBubble x y)
                , Element.mouseOver [ Element.scale 1.2 ]
                , Transition.properties [ Transition.transform 150 [ Transition.easeInOut ] ] |> Element.htmlAttribute
                ]
                (Element.el
                    [ Element.width (Element.px bubbleDiameter)
                    , Element.height (Element.px bubbleDiameter)
                    , Element.Background.color (getRgbColor bubble |> (\( r, g, b ) -> Element.rgb255 r g b))
                    , Element.Border.rounded 20
                    ]
                    Element.none
                )

        bubbleGridRow : Int -> List Bubble -> Element.Element Msg
        bubbleGridRow x row =
            Element.row [ Element.spacing 3 ] (List.indexedMap (viewBubble x) row)
    in
    Element.column [ Element.spacing 3 ] (List.indexedMap bubbleGridRow matrix)


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

                NoColor ->
                    ( 255, 255, 255 )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ClickedBubble x y ->
            let
                targetBubble : Maybe Bubble
                targetBubble =
                    getBubble x y model.matrix

                newMatrix : Matrix Bubble
                newMatrix =
                    case targetBubble of
                        Just bubble ->
                            bfsBubble x y model.matrix bubble.color |> updateBubbles

                        Nothing ->
                            model.matrix
            in
            ( { model | matrix = newMatrix }, Cmd.none )

        GeneratedRandomMatrix matrix ->
            ( { model | matrix = matrix }, Cmd.none )


getBubble : Int -> Int -> Matrix Bubble -> Maybe Bubble
getBubble x y matrix =
    List.drop x matrix
        |> List.head
        |> Maybe.andThen (List.drop y >> List.head)


bfsBubble : Int -> Int -> Matrix Bubble -> Color -> Matrix Bubble
bfsBubble x y matrix color =
    let
        queue : List ( Int, Int )
        queue =
            [ ( x, y ) ]

        visited : Set.Set ( Int, Int )
        visited =
            Set.empty
    in
    bfsHelper queue visited matrix color


bfsHelper : List ( Int, Int ) -> Set.Set ( Int, Int ) -> Matrix Bubble -> Color -> Matrix Bubble
bfsHelper queue visited matrix color =
    case queue of
        [] ->
            matrix

        ( row, col ) :: rest ->
            if Set.member ( row, col ) visited then
                bfsHelper rest visited matrix color

            else
                let
                    bubble : Maybe Bubble
                    bubble =
                        getBubble row col matrix

                    updatedVisited : Set.Set ( Int, Int )
                    updatedVisited =
                        Set.insert ( row, col ) visited

                    newQueue : List ( Int, Int )
                    newQueue =
                        rest ++ List.filter (\( r, c ) -> isValidBubble r c matrix && isSameColor r c matrix color) (neighbors row col)

                    newMatrix : Matrix Bubble
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


movePoppedBubblesUpwards : List Bubble -> List Bubble
movePoppedBubblesUpwards column =
    let
        ( popped, unpopped ) =
            List.partition (\bubble -> bubble.state == Popped) column
    in
    popped ++ unpopped


updateBubbles : Matrix Bubble -> Matrix Bubble
updateBubbles matrix =
    let
        columns : Matrix Bubble
        columns =
            transpose matrix

        newColumns : Matrix Bubble
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
    , rows = 10
    , columns = 10
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
