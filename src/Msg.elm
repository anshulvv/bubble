module Msg exposing (..)

import Bubble
import Http
import Matrix
import Quote exposing (Quote)


type Msg
    = ClickedBubble Int Int
    | GeneratedRandomMatrix (Matrix.Matrix Bubble.Bubble)
    | ChangedNumOfRows Float
    | ChangedNumOfColumns Float
    | ConfigChanged
    | QuoteRecieved (Result Http.Error (List Quote))
    | KeyPressed String
    | ConsoleLogMessageInputChanged String
    | ConsoleLogButtonClicked
