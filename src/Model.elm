module Model exposing (..)

import Bubble
import Matrix
import Quote exposing (Quote)


type alias Model =
    { matrix : Matrix.Matrix Bubble.Bubble
    , matrixState : Matrix.MatrixState
    , rows : Float
    , columns : Float
    , quote : Maybe Quote
    , keyPressed : Maybe String
    }


initModel : Model
initModel =
    { matrix = [ [] ]
    , matrixState = Matrix.Idle
    , rows = 10
    , columns = 10
    , quote = Nothing
    , keyPressed = Nothing
    }
