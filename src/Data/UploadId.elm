module Data.UploadId
    exposing
        ( Collection
        , UploadId
        , decoder
        , encoder
        , get
        , init
        , insert
        , remove
        , toList
        , update
        )

{-| Opaque wrapper for upload ids.

Library also that handles interaction with dict with keys of UploadId. Auto increments the current ID when new upload
is inserted.

@docs Collection, UploadId, decoder, encoder, get, init, insert, remove, toList, update

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


---- TYPES ----


{-| Represents an UploadId
-}
type UploadId
    = UploadId Int


{-| A dict which uses an UploadId for keys. The idea is you can store uploads in this collection and an UploadId is
automatically generated and returned when you insert.

The next UploadId is stored in the custom type itself, making collisions impossible.

-}
type Collection file
    = Collection UploadId (Dict Int file)



---- COLLECTION ----


{-| Initialize an empty UploadId collection
-}
init : Collection file
init =
    Collection (UploadId 1) Dict.empty


{-| Get the values in an UploadId collection
-}
values : Collection file -> List file
values (Collection _ collection) =
    Dict.values collection


{-| Get the upload stored at an UploadId if it exists
-}
get : UploadId -> Collection file -> Maybe file
get (UploadId id) (Collection _ collection) =
    Dict.get id collection


{-| Insert a new upload in to the collection, returns a tuple of the
-}
insert : file -> Collection file -> ( UploadId, Collection file )
insert file (Collection ((UploadId id) as uploadId) collection) =
    ( uploadId
    , Dict.insert id file collection
        |> Collection (UploadId (id + 1))
    )


{-| Remove the upload specified by an UploadId from a collection, if it exists
-}
remove : UploadId -> Collection file -> Collection file
remove (UploadId id) (Collection uploadId collection) =
    Dict.remove id collection
        |> Collection uploadId


{-| Turn the collection to a List
-}
toList : Collection file -> List ( UploadId, file )
toList (Collection _ collection) =
    collection
        |> Dict.toList
        |> List.map (Tuple.mapFirst UploadId)


{-| Update the upload specified by an UploadId, same API as a Dict
-}
update : UploadId -> (Maybe file -> Maybe file) -> Collection file -> Collection file
update (UploadId id) updateFn (Collection uploadId collection) =
    Dict.update id updateFn collection
        |> Collection uploadId



---- ENCODING ----


{-| Decode an UploadId from an integer
-}
decoder : Decoder UploadId
decoder =
    Decode.map UploadId Decode.int


{-| Encode an UploadId to an integer
-}
encoder : UploadId -> Encode.Value
encoder (UploadId id) =
    Encode.int id
