module Main exposing (..)

import Ui.ColorPanel
import Ext.Color exposing (Hsv, hsvToRgb)
import Config exposing (websocketUrl)
import Ui
import Ui.App
import Ui.Container
import Ui.Button
import Color exposing (Color, black, red, blue, white, hsla)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html, div, text, button, br)
import Html.Attributes exposing (..)
import Mouse exposing (..)
import WebSocket
import Task
import Json.Decode exposing (decodeString, map)
import Line exposing (Line, encodedLine, linesDecoder)
import VirtualDom


type alias Model =
    { app : Ui.App.Model
    , lines : List Line
    , currentLine : Line
    , isDrawing : Bool
    , x : Int
    , y : Int
    , windowWidth : Int
    , windowHeight : Int
    , selectedColor : Color
    , colorPanel : Ui.ColorPanel.Model
    }


type Msg
    = App Ui.App.Msg
    | DrawStart Mouse.Position
    | DrawStop Mouse.Position
    | MouseMsg Mouse.Position
    | ClearCollage
    | NewMessage String
    | SendNewLine
    | SendClear
    | ColorPanel Ui.ColorPanel.Msg


initialModel : Model
initialModel =
    { app = Ui.App.init
    , lines = []
    , currentLine = Line [] Color.black
    , isDrawing = False
    , x = 0
    , y = 0
    , windowWidth = 600
    , windowHeight = 300
    , selectedColor = Color.black
    , colorPanel = Ui.ColorPanel.init Color.black
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        App act ->
            let
                ( app, effect ) =
                    Ui.App.update act model.app
            in
                ( { model | app = app }, Cmd.map App effect )

        DrawStart _ ->
            { model | isDrawing = True, currentLine = (Line [] model.selectedColor) } ! []

        DrawStop _ ->
            saveLine model ! [ msgToCmd SendNewLine ]

        MouseMsg position ->
            isDrawing position model ! []

        ClearCollage ->
            model ! [ msgToCmd SendClear ]

        SendClear ->
            let
                message =
                    "{\"clear\": true}"
            in
                model ! [ WebSocket.send websocketUrl message ]

        SendNewLine ->
            let
                latestLine =
                    case List.head model.lines of
                        Nothing ->
                            Line [] black

                        Just val ->
                            val

                stringifiedLine =
                    encodedLine latestLine
            in
                model ! [ WebSocket.send websocketUrl stringifiedLine ]

        NewMessage stringifiedLine ->
            case (decodeString linesDecoder stringifiedLine) of
                Ok lines ->
                    { model | lines = lines } ! []

                Err err ->
                    Debug.log err
                        model
                        ! []

        ColorPanel act ->
            let
                ( colorPanel, effect ) =
                    Ui.ColorPanel.update act model.colorPanel

                { saturation, alpha, hue, value } =
                    colorPanel.value

                pickedColor =
                    hsvToRgb (Hsv saturation value alpha hue)
            in
                ( { model | colorPanel = colorPanel, selectedColor = pickedColor }, Cmd.map ColorPanel effect )


view : Model -> Html.Html Msg
view model =
    Ui.App.view
        App
        model.app
        [ Ui.Container.column
            []
            [ Ui.title [] [ Html.text "Elm-UI Paint Online" ]
            , Ui.textBlock "This is simple paint online integrated with Elm-UI library."
            , Ui.Container.row
                []
                [ createCollage model
                , Ui.Container.column
                    []
                    [ Html.map ColorPanel (Ui.ColorPanel.view model.colorPanel)
                    , Ui.Button.primary "Clear collage" ClearCollage
                    ]
                ]
            ]
        ]


collageStyles : Html.Attribute Msg
collageStyles =
    Html.Attributes.style [ ( "margin", "20" ), ( "border-style", "solid" ), ( "display", "inline-block" ) ]


createCollage : Model -> Html Msg
createCollage model =
    let
        background =
            rect (toFloat model.windowWidth) (toFloat model.windowHeight)
                |> filled white

        createdLines =
            (drawLines model.lines)

        allElements =
            (List.append (background :: [ drawLine model.currentLine ]) createdLines)
    in
        div []
            [ div [ collageStyles, VirtualDom.onWithOptions "mousemove" options (Json.Decode.map MouseMsg offsetPosition) ]
                [ collage
                    model.windowWidth
                    model.windowHeight
                    allElements
                    |> Element.toHtml
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs DrawStart
        , Mouse.ups DrawStop
        , WebSocket.listen websocketUrl NewMessage
        , Sub.map ColorPanel (Ui.ColorPanel.subscriptions model.colorPanel)
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


msgToCmd : a -> Cmd a
msgToCmd msg =
    Task.perform (always msg) (Task.succeed ())


saveLine : Model -> Model
saveLine model =
    { model | y = 0, x = 0, lines = model.currentLine :: model.lines, isDrawing = False }


isDrawing : Mouse.Position -> Model -> Model
isDrawing position model =
    if model.isDrawing then
        appendPointToLine position model
    else
        model


appendPointToLine : Mouse.Position -> Model -> Model
appendPointToLine position model =
    let
        newX =
            position.x

        newY =
            position.y

        currentLine =
            model.currentLine

        newCurrentLine =
            { currentLine | points = List.append [ { x = position.x - (model.windowWidth // 2), y = (model.windowHeight // 2) - position.y } ] currentLine.points }
    in
        { model | y = newY, x = newX, currentLine = newCurrentLine }


drawLines : List Line -> List Form
drawLines lines =
    List.map drawLine lines


drawLine : Line -> Form
drawLine line =
    let
        intsToFloats : Mouse.Position -> ( Float, Float )
        intsToFloats p =
            ( toFloat p.x, toFloat p.y )

        shape =
            path (List.map intsToFloats line.points)
    in
        shape
            |> traced (solid line.lineColor)


options =
    { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decode.Decoder Mouse.Position
offsetPosition =
    Json.Decode.map2 Position (Json.Decode.field "offsetX" Json.Decode.int) (Json.Decode.field "offsetY" Json.Decode.int)
