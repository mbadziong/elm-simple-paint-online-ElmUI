module Main exposing (..)

import Ui.ColorPicker
import Ext.Color exposing (Hsv, hsvToRgb)
import Config exposing (websocketUrl)
import Ui.App
import Color exposing (Color, black, red, blue, white, hsla)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html, div, text, button, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mouse exposing (..)
import WebSocket
import Task
import Json.Decode exposing (decodeString)
import Line exposing (Line, encodedLine, linesDecoder)


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
    , colorPicker : Ui.ColorPicker.Model
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
    | ColorPicker Ui.ColorPicker.Msg


initialModel : Model
initialModel =
    { app = Ui.App.init
    , lines = []
    , currentLine = Line [] Color.black
    , isDrawing = False
    , x = 0
    , y = 0
    , windowWidth = 300
    , windowHeight = 300
    , selectedColor = Color.black
    , colorPicker = Ui.ColorPicker.init Color.black
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

        ColorPicker act ->
            let
                ( colorPicker, effect ) =
                    Ui.ColorPicker.update act model.colorPicker

                { saturation, alpha, hue, value } =
                    colorPicker.colorPanel.value

                pickedColor =
                    hsvToRgb (Hsv saturation value alpha hue)
            in
                ( { model | colorPicker = colorPicker, selectedColor = pickedColor }, Cmd.map ColorPicker effect )


view : Model -> Html.Html Msg
view model =
    Ui.App.view
        App
        model.app
        [ let
            background =
                rect (toFloat model.windowWidth) (toFloat model.windowHeight)
                    |> filled white

            createdLines =
                (drawLines model.lines)

            allElements =
                (List.append (background :: [ drawLine model.currentLine ]) createdLines)
          in
            div []
                [ div [ Html.Attributes.style [ ( "border-style", "solid" ), ( "display", "inline-block" ) ] ]
                    [ collage
                        model.windowWidth
                        model.windowHeight
                        allElements
                        |> Element.toHtml
                    ]
                , br [] []
                , button [ onClick (ClearCollage) ] [ Html.text "clear" ]
                , br [] []
                , Html.map ColorPicker (Ui.ColorPicker.view model.colorPicker)
                ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs DrawStart
        , Mouse.moves MouseMsg
        , Mouse.ups DrawStop
        , WebSocket.listen websocketUrl NewMessage
        , Sub.map ColorPicker (Ui.ColorPicker.subscriptions model.colorPicker)
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
        intsToFloats : { x : Int, y : Int } -> ( Float, Float )
        intsToFloats p =
            ( toFloat p.x, toFloat p.y )

        shape =
            path (List.map intsToFloats line.points)
    in
        shape
            |> traced (solid line.lineColor)
