module Main exposing (..)

import Ui.Container
import Ui.Button
import Ui.App
import Ui
import Color exposing (Color, black, red, blue, white)
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
    }


type Msg
    = App Ui.App.Msg
    | DrawStart Mouse.Position
    | DrawStop Mouse.Position
    | MouseMsg Mouse.Position
    | ChangeColor Color
    | ClearCollage
    | NewMessage String
    | SendNewLine
    | SendClear


websocketUrl : String
websocketUrl =
    "ws://localhost:1234/test"


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

        ChangeColor col ->
            { model | selectedColor = col } ! []

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


view : Model -> Html.Html Msg
view model =
    Ui.App.view
        App
        model.app
        [ Ui.Container.column
            []
            [ Ui.title [] [ Html.text "TEST" ]
            , Ui.textBlock "This is an minimal project to get you started with Elm-UI!"
            , let
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
                    , button [ onClick (ChangeColor red) ] [ Html.text "red" ]
                    , button [ onClick (ChangeColor blue) ] [ Html.text "blue" ]
                    , button [ onClick (ChangeColor black) ] [ Html.text "black" ]
                    , button [ onClick (ClearCollage) ] [ Html.text "clear" ]
                    ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs DrawStart
        , Mouse.moves MouseMsg
        , Mouse.ups DrawStop
        , WebSocket.listen websocketUrl NewMessage
        ]


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
