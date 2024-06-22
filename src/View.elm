module View exposing (..)

import Bubble exposing (Bubble)
import Debug
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html exposing (Html)
import Matrix exposing (Matrix)
import Model exposing (Model)
import Msg exposing (Msg)
import Quote exposing (Quote)
import Simple.Transition as Transition


view : Model -> Html Msg
view model =
    let
        heading =
            Element.column []
                [ Element.el
                    [ Element.Font.size 30
                    , Element.Font.bold
                    ]
                    (Element.text "Bubble")
                , Element.el [] (Element.text "Click on a bubble to pop it and some bubbles close by of same color")
                ]

        content =
            Element.column
                [ Element.spacingXY 0 70 ]
                [ Element.row
                    [ Element.spacingXY 50 0 ]
                    [ bubbleGrid model.matrix, config model ]
                ]
    in
    Element.layout [ Element.padding 40 ] (Element.column [ Element.spacingXY 0 50 ] [ heading, content, viewQuote model.quote ])


bubbleGrid : Matrix Bubble -> Element.Element Msg
bubbleGrid matrix =
    let
        viewBubble : Int -> Int -> Bubble -> Element.Element Msg
        viewBubble x y bubble =
            Element.el
                [ Element.Events.onClick (Msg.ClickedBubble x y)
                , Element.mouseOver [ Element.scale 1.2 ]
                , Element.pointer
                , Transition.properties [ Transition.transform 100 [ Transition.easeInOut ] ] |> Element.htmlAttribute
                ]
                (Element.el
                    [ Element.width (Element.px Bubble.bubbleDiameter)
                    , Element.height (Element.px Bubble.bubbleDiameter)
                    , Element.Background.color (Bubble.getRgbColor bubble.color |> (\( r, g, b ) -> Element.rgb255 r g b))
                    , Element.Border.rounded 20
                    ]
                    Element.none
                )

        bubbleGridRow : Int -> List Bubble -> Element.Element Msg
        bubbleGridRow x row =
            Element.row [ Element.spacing 3 ] (List.indexedMap (viewBubble x) row)
    in
    Element.column [ Element.spacing 3 ] (List.indexedMap bubbleGridRow matrix)


config : Model -> Element.Element Msg
config model =
    Element.column []
        [ slider "Rows" model.rows Msg.ChangedNumOfRows
        , slider "Columns" model.columns Msg.ChangedNumOfColumns
        , generateNewMatrixButton
        ]


generateNewMatrixButton : Element.Element Msg
generateNewMatrixButton =
    Element.Input.button []
        { onPress = Just Msg.ConfigChanged
        , label = Element.text "Generate New Matrix"
        }


slider : String -> Float -> (Float -> msg) -> Element.Element msg
slider label value msg =
    Element.el [] <|
        Element.Input.slider
            [ Element.height (Element.px 30)
            , Element.width (Element.px 300)

            -- Here is where we're creating/styling the "track"
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Element.Background.color <| (\( r, g, b ) -> Element.rgb255 r g b) <| Bubble.getRgbColor Bubble.Grey
                    , Element.Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = msg
            , label =
                Element.Input.labelAbove []
                    (Element.text (label ++ " " ++ Debug.toString value))
            , min = 0
            , max = 25
            , step = Just 1
            , value = value
            , thumb =
                Element.Input.defaultThumb
            }


viewQuote : Maybe Quote -> Element.Element Msg
viewQuote quote =
    let
        quoteHeading =
            Element.el
                [ Element.Font.size 30
                , Element.Font.bold
                ]
                (Element.text "Random Quote")

        quoteContent : Quote -> Element.Element msg
        quoteContent quote_ =
            Element.column
                [ Element.spacingXY 0 10
                ]
                [ Element.paragraph
                    [ Element.Font.size 20
                    , Element.Font.italic
                    ]
                    [ Element.text ("\"" ++ quote_.quote ++ "\"") ]
                , Element.el
                    [ Element.Font.size 15
                    ]
                    (Element.text ("~ " ++ quote_.author))
                , additionalMsg quote_.msg
                ]

        additionalMsg msg =
            Element.el []
                (Element.text msg)
    in
    case quote of
        Just quote_ ->
            Element.column [ Element.spacingXY 0 20 ]
                [ quoteHeading
                , quoteContent quote_
                ]

        Nothing ->
            Element.none
