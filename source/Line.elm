module Line exposing (Line, encodedLine, linesDecoder)

import Color exposing (Color)
import Point exposing (Point)
import ColorUtils exposing (colorEncoder, colorDecoder, Rgba)
import Json.Encode exposing (list, string, object, encode, int)
import Json.Decode exposing (map3, field, Decoder, andThen)
import Point exposing (pointEncoder, pointDecoder)


type alias Line =
    { points : List Point
    , lineColor : Color
    , width : Int
    }


type alias EncodedLine =
    { points : List Point
    , lineColor : Rgba
    , width : Int
    }


encodedLine : Line -> String
encodedLine line =
    let
        lineColor =
            colorEncoder line.lineColor

        points =
            line.points

        width =
            line.width

        entireMessage =
            object
                [ ( "lineColor", Json.Encode.string lineColor )
                , ( "points", Json.Encode.list (List.map pointEncoder points) )
                , ( "width", Json.Encode.int width )
                ]
                |> encode 0
    in
        entireMessage


lineDecoder : Decoder Line
lineDecoder =
    map3 Line
        (field "points" (Json.Decode.list pointDecoder))
        (field "lineColor" colorDecoder)
        (field "width" Json.Decode.int)


linesDecoder : Decoder (List Line)
linesDecoder =
    Json.Decode.list lineDecoder
