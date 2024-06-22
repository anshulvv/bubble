module Subscriptions exposing (..)

import Browser.Events
import Json.Decode as D exposing (Decoder)
import Model exposing (Model)
import Msg exposing (Msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress subscriptionDecoderMsg


subscriptionDecoderMsg : Decoder Msg
subscriptionDecoderMsg =
    D.map toMsg (D.field "key" D.string)


toMsg : String -> Msg
toMsg str =
    Msg.KeyPressed str
