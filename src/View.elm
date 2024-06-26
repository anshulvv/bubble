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


sectionAttr : List (Element.Attribute msg)
sectionAttr =
    [ Element.spacingXY 0 10 ]


buttonAttr : List (Element.Attribute msg)
buttonAttr =
    [ Element.padding 15
    , Element.Background.color <| Element.rgb255 240 240 240
    , Element.Border.rounded 10
    ]


viewTitle : String -> Element.Element Msg
viewTitle string =
    Element.el
        [ Element.Font.size 30
        , Element.Font.bold
        ]
        (Element.text string)


viewTitleWithSubheading : String -> String -> Element.Element msg
viewTitleWithSubheading title subheading =
    Element.column sectionAttr
        [ Element.el
            [ Element.Font.size 30
            , Element.Font.bold
            ]
            (Element.text title)
        , Element.el [] (Element.text subheading)
        ]


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 40 ]
        (Element.column [ Element.spacingXY 0 50 ]
            [ viewBubbleGrid model
            , viewKeyPressed model.keyPressed
            , viewConsolePrintInput
            , viewQuote model.quote
            ]
        )


viewBubbleGrid : Model -> Element.Element Msg
viewBubbleGrid model =
    Element.column sectionAttr
        [ viewTitleWithSubheading "Bubble" "Click on a bubble to pop it and some bubbles close by of same color"
        , Element.row
            [ Element.spacingXY 50 0 ]
            [ bubbleGrid model.matrix, config model ]
        ]


viewConsolePrintInput : Element.Element Msg
viewConsolePrintInput =
    let
        viewInputLabel =
            Element.Input.labelLeft []
                (Element.el [] (Element.text "Console Log Input: "))

        viewPrintConsoleInput =
            Element.Input.text []
                { onChange = Msg.ConsoleLogMessageInputChanged
                , text = ""
                , placeholder = Nothing
                , label = viewInputLabel
                }

        viewButtonLabel =
            Element.el [] (Element.text "Print")

        viewPrintButton =
            Element.Input.button buttonAttr
                { onPress = Just Msg.ConsoleLogButtonClicked
                , label = viewButtonLabel
                }

        viewContent =
            Element.row
                [ Element.spacing 10
                , Element.alignBottom
                ]
                [ viewPrintConsoleInput, viewPrintButton ]
    in
    Element.column sectionAttr [ viewTitle "Console Print", viewContent ]


viewKeyPressed : Maybe String -> Element.Element Msg
viewKeyPressed keyPressed =
    let
        viewHeading =
            Element.el
                [ Element.Font.size 30
                , Element.Font.bold
                ]
                (Element.text "Pressed Key: ")

        viewContent =
            Element.paragraph []
                [ Element.text "Key Pressed: "
                , Element.el
                    [ Element.Font.bold
                    , Element.Font.italic
                    ]
                    (Element.text (keyPressed |> Maybe.withDefault ""))
                ]
    in
    Element.column sectionAttr [ viewHeading, viewContent ]


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
            Element.column sectionAttr
                [ quoteHeading
                , quoteContent quote_
                ]

        Nothing ->
            Element.none
