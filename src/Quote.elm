module Quote exposing (..)

import Json.Decode as D exposing (Decoder)


type alias Quote =
    { msg : String
    , quote : String
    , author : String
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
