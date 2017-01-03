module ColorUtils exposing (colorEncoder, colorDecoder, Rgba)

import Json.Decode as DE exposing (Decoder, decodeString, list, string, andThen, succeed, map4, field, int, float)
import Json.Encode as JE exposing (encode, object, int, float)
import Color exposing (rgba, Color, toRgb)


type alias Rgba =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


colorEncoder : Color -> String
colorEncoder color =
    encodeRgba <| colorToRgba color


colorToRgba : Color -> Rgba
colorToRgba color =
    toRgb color


encodeRgba : Rgba -> String
encodeRgba rgba =
    encode 0 <|
        object
            [ ( "red", JE.int rgba.red )
            , ( "green", JE.int rgba.green )
            , ( "blue", JE.int rgba.blue )
            , ( "alpha", JE.float rgba.alpha )
            ]


rgbaDecoder : Decoder Rgba
rgbaDecoder =
    map4 Rgba
        (field "red" DE.int)
        (field "green" DE.int)
        (field "blue" DE.int)
        (field "alpha" DE.float)


stringToColorDecoder : String -> Decoder Color
stringToColorDecoder stringifiedColor =
    case (decodeString rgbaDecoder stringifiedColor) of
        Ok color ->
            succeed <| rgba color.red color.green color.blue color.alpha

        Err _ ->
            -- transparent line
            succeed <| rgba 0 0 0 0


colorDecoder : Decoder Color
colorDecoder =
    DE.string |> andThen stringToColorDecoder
