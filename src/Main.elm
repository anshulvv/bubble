module Main exposing (..)

import Browser
import Cmd
import Model exposing (Model)
import Msg exposing (Msg)
import Update
import View


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Model.initModel, Cmd.initCmd )
        , update = Update.update
        , view = View.view
        , subscriptions = \_ -> Sub.none
        }
