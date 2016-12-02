module Line exposing (Line, encodedLine, linesDecoder)

import Color exposing (Color)
import Point exposing (Point)
import ColorUtils exposing (colorToString, colorDecoder)
import Json.Encode exposing (list, string, object, encode, int)
import Json.Decode exposing (map2, field, Decoder, andThen)
import Point exposing (pointEncoder, pointDecoder)


type alias Line =
    { points : List Point
    , lineColor : Color
    }


type alias EncodedLine =
    { points : List Point
    , lineColor : String
    }


encodedLine : Line -> String
encodedLine line =
    let
        lineColor =
            colorToString line.lineColor

        points =
            line.points

        entireMessage =
            object
                [ ( "lineColor", Json.Encode.string lineColor )
                , ( "points", Json.Encode.list (List.map pointEncoder points) )
                ]
                |> encode 0
    in
        entireMessage


lineDecoder : Decoder Line
lineDecoder =
    map2 Line
        (field "points" (Json.Decode.list pointDecoder))
        (field "lineColor" colorDecoder)


linesDecoder : Decoder (List Line)
linesDecoder =
    Json.Decode.list lineDecoder
