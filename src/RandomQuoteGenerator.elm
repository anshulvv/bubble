module RandomQuoteGenerator exposing (..)

import Http
import Json.Decode as D exposing (Decoder)


type alias Quote =
    { quote : String
    , author : String
    }


getRandomQuote : (Result Http.Error Quote -> msg) -> Cmd msg
getRandomQuote msg =
    Http.get
        { url = "https://api.quotable.io/quotes/random"
        , expect = Http.expectJson msg quoteDecoder
        }


quoteDecoder : Decoder Quote
quoteDecoder =
    D.succeed (Quote "quote" "author")


emptyQuote : Quote
emptyQuote =
    Quote "" ""
