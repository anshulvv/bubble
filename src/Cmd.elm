module Cmd exposing (..)

import Model
import Msg exposing (Msg)
import Port
import RandomGenerator


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ RandomGenerator.generateNewMatrix Model.initModel
        , RandomGenerator.getRandomQuote Msg.QuoteRecieved
        ]


consoleLog : String -> Cmd Msg
consoleLog printStr =
    Port.sendStringToJsForConsoleLog printStr
