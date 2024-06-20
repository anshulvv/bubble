module RandomQuoteGenerator exposing (..)

import Http
import Json.Decode as D exposing (Decoder)


type alias Quote =
    { msg : String
    , quote : String
    , author : String
    }


getRandomQuote : (Result Http.Error (List Quote) -> msg) -> Cmd msg
getRandomQuote msg =
    Http.get
        { url = "https://api.quotable.io/quotes/random"
        , expect = Http.expectJson msg quoteDecoder
        }


quoteDecoder : Decoder (List Quote)
quoteDecoder =
    D.list
        (D.map2 (Quote "")
            (D.field "content" D.string)
            (D.field "author" D.string)
        )


emptyQuote : Quote
emptyQuote =
    Quote "" "" ""


emptyQuoteWithMsg : String -> Quote
emptyQuoteWithMsg msg =
    Quote msg "" ""
