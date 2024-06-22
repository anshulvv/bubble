module Cmd exposing (..)

import Model
import Msg exposing (Msg)
import RandomGenerator


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ RandomGenerator.generateNewMatrix Model.initModel
        , RandomGenerator.getRandomQuote Msg.QuoteRecieved
        ]
