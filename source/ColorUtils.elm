module ColorUtils exposing (colorToString, colorDecoder)

import Json.Decode exposing (Decoder, decodeString, list, string, andThen, succeed, fail)
import Color exposing (Color, black, red, blue)
import Color.Convert exposing (colorToHex, hexToColor)


colorToString : Color -> String
colorToString color =
    colorToHex (color)


stringToColorDecoder : String -> Decoder Color
stringToColorDecoder hex =
    case hexToColor hex of
        Just color ->
            succeed color

        Nothing ->
            fail ("Cannot decode color " ++ hex)


colorDecoder : Decoder Color
colorDecoder =
    Json.Decode.string |> andThen stringToColorDecoder
