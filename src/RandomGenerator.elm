module RandomGenerator exposing (..)

import Bubble exposing (Color(..))
import Http
import Matrix exposing (Matrix)
import Model exposing (Model)
import Msg exposing (Msg)
import Quote exposing (Quote)
import Random


getRandomQuote : (Result Http.Error (List Quote) -> msg) -> Cmd msg
getRandomQuote msg =
    Http.get
        { url = "https://api.quotable.io/quotes/random"
        , expect = Http.expectJson msg Quote.quoteDecoder
        }


randomColor : Random.Generator Color
randomColor =
    Random.uniform Red [ Green, Blue, Yellow, Black ]


generateNewMatrix : Model -> Cmd Msg
generateNewMatrix model =
    randomMatrix model.rows model.columns |> Random.generate Msg.GeneratedRandomMatrix


randomMatrix : Float -> Float -> Random.Generator (Matrix Bubble.Bubble)
randomMatrix r c =
    Random.map (\color -> { state = Bubble.Unpopped, color = color }) randomColor
        |> Random.list (floor c)
        |> Random.list (floor r)
