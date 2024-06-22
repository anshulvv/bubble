module Update exposing (..)

import Bubble exposing (Bubble)
import Helper
import Matrix exposing (Matrix)
import Model exposing (Model)
import Msg exposing (Msg)
import Quote
import RandomGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.ClickedBubble x y ->
            let
                targetBubble : Bubble
                targetBubble =
                    Helper.getBubbleFromMatrix x y model.matrix

                newMatrix : Matrix Bubble
                newMatrix =
                    Helper.bfsBubble x y model.matrix targetBubble.color |> updateBubbles
            in
            ( { model | matrix = newMatrix }, Cmd.none )

        Msg.GeneratedRandomMatrix matrix ->
            ( { model | matrix = matrix }, Cmd.none )

        Msg.ChangedNumOfRows newRows ->
            ( { model | rows = newRows }, Cmd.none )

        Msg.ChangedNumOfColumns newColumns ->
            ( { model | columns = newColumns }, Cmd.none )

        Msg.ConfigChanged ->
            ( model, RandomGenerator.generateNewMatrix model )

        Msg.QuoteRecieved result ->
            case result of
                Ok quotesList ->
                    ( { model | quote = List.head quotesList }, Cmd.none )

                Err _ ->
                    ( { model | quote = Just (Quote.emptyQuoteWithMsg "Error Fetching data") }, Cmd.none )

        Msg.KeyPressed key ->
            ( { model | keyPressed = Just key }, Cmd.none )


updateBubbles : Matrix Bubble -> Matrix Bubble
updateBubbles matrix =
    let
        columns : Matrix Bubble
        columns =
            transpose matrix

        newColumns : Matrix Bubble
        newColumns =
            List.map movePoppedBubblesUpwards columns
    in
    transpose newColumns


movePoppedBubblesUpwards : List Bubble -> List Bubble
movePoppedBubblesUpwards column =
    let
        ( popped, unpopped ) =
            List.partition (\bubble -> bubble.state == Bubble.Popped) column
    in
    popped ++ unpopped



--- HELPERS


transpose : Matrix a -> Matrix a
transpose matrix =
    case matrix of
        [] ->
            []

        [] :: _ ->
            []

        _ ->
            let
                heads : List a
                heads =
                    List.map List.head matrix |> List.filterMap identity

                tails : List (List a)
                tails =
                    List.map List.tail matrix |> List.filterMap identity
            in
            heads :: transpose tails
