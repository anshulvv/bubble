module Helper exposing (..)

import Bubble exposing (Bubble)
import Matrix exposing (Matrix)
import Set


getBubbleFromMatrix : Int -> Int -> Matrix Bubble -> Bubble
getBubbleFromMatrix x y matrix =
    let
        bubble =
            List.drop x matrix
                |> List.head
                |> Maybe.andThen (List.drop y >> List.head)
    in
    case bubble of
        Just bub ->
            bub

        Nothing ->
            Bubble Bubble.Unpopped Bubble.NoColor


bfsBubble : Int -> Int -> Matrix Bubble -> Bubble.Color -> Matrix Bubble
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


bfsHelper : List ( Int, Int ) -> Set.Set ( Int, Int ) -> Matrix Bubble -> Bubble.Color -> Matrix Bubble
bfsHelper queue visited matrix color =
    case queue of
        [] ->
            matrix

        ( row, col ) :: rest ->
            if Set.member ( row, col ) visited then
                bfsHelper rest visited matrix color

            else
                let
                    updatedVisited : Set.Set ( Int, Int )
                    updatedVisited =
                        Set.insert ( row, col ) visited

                    newQueue : List ( Int, Int )
                    newQueue =
                        rest ++ List.filter (\( r, c ) -> isValidBubble r c matrix && isMatrixBubbleOfColor r c matrix color) (Matrix.neighbors row col)

                    newMatrix : Matrix Bubble
                    newMatrix =
                        changeBubbleState row col matrix
                in
                bfsHelper newQueue updatedVisited newMatrix color


changeBubbleState : Int -> Int -> Matrix Bubble -> Matrix Bubble
changeBubbleState r c matrix =
    List.indexedMap
        (\ir row ->
            if ir == r then
                List.indexedMap
                    (\ic bubble ->
                        if ic == c then
                            { bubble | state = Bubble.Popped, color = Bubble.NoColor }

                        else
                            bubble
                    )
                    row

            else
                row
        )
        matrix


isValidBubble : Int -> Int -> Matrix Bubble -> Bool
isValidBubble r c matrix =
    (r >= 0)
        && (r < List.length matrix)
        && (c >= 0)
        && (c < (List.length <| Maybe.withDefault [] <| List.head matrix))


isMatrixBubbleOfColor : Int -> Int -> Matrix Bubble -> Bubble.Color -> Bool
isMatrixBubbleOfColor r c matrix color =
    let
        bub : Bubble
        bub =
            getBubbleFromMatrix r c matrix
    in
    bub.color == color && bub.state == Bubble.Unpopped
