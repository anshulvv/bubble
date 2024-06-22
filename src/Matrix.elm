module Matrix exposing (..)


type alias Matrix valType =
    List (List valType)


type MatrixState
    = Idle
    | Popping


neighbors : Int -> Int -> List ( Int, Int )
neighbors r c =
    [ ( r + 1, c )
    , ( r - 1, c )
    , ( r, c + 1 )
    , ( r, c - 1 )
    ]
