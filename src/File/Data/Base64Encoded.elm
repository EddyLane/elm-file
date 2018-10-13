module File.Data.Base64Encoded exposing (Base64Encoded, decoder, encoder, toString)

{-| Opaque wrapper for Base64 encoded data

@docs Base64Encoded, decoder, encoder, toString

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| Represents a Base64Encoded value
-}
type Base64Encoded
    = Base64Encoded String


{-| Get the string representation of the base64 encoding, can be used as a data-url
-}
toString : Base64Encoded -> String
toString (Base64Encoded base64) =
    base64


{-| Decode a string to a Base64Encoded type
-}
decoder : Decoder Base64Encoded
decoder =
    Decode.string
        |> Decode.andThen (Base64Encoded >> Decode.succeed)


{-| Encode a Base64Encoded type to string
-}
encoder : Base64Encoded -> Encode.Value
encoder (Base64Encoded base64) =
    Encode.string base64
