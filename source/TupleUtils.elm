module TupleUtils exposing (pointEncoder, pointDecoder)

import Json.Encode as JE exposing (object, int)
import Json.Decode as DE exposing (Decoder, map2, int, field)


type alias Point =
    { x : Int, y : Int }


pointToObject point =
    object
        [ ( "x", JE.int point.x )
        , ( "y", JE.int point.y )
        ]


pointEncoder point =
    pointToObject point


pointDecoder : Decoder Point
pointDecoder =
    map2 Point
        (field "x" DE.int)
        (field "y" DE.int)
