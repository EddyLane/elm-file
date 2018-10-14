module File.Upload
    exposing
        ( Config
        , State
        , SubsConfig
        , UploadingFile
        , cancel
        , config
        , configBase64EncodedMsg
        , configMaximumFileSize
        , configSetStateMsg
        , configUploadedMsg
        , encode
        , failure
        , fileData
        , fileFilename
        , fileIsFailed
        , fileIsImage
        , fileProgress
        , init
        , subscriptions
        , success
        , update
        , upload
        , uploads
        )

{-| Provides an interface to upload files to a remote destination, but requires you to fill in some blanks

The reason this package makes you fill in so many blanks is because it is a tricky subject.
There are many ways to do uploads, so we hope that you can fill in the blanks.


# Uploader

When you need to create an uploader you first need to init the state:

    import File.Upload as Upload

    -- You need to keep track of the uploader state in your model

    type alias Model =
        { upload : Upload.State }


    -- The uploader needs to be initialized

    initialState : Upload.State
    initialState =
        { upload = Upload.init }


    -- You need to then configure the uploader, starting with a NoOp message; a custom type which takes no arguments.

    uploadConfig : Upload.Config Msg
    uploadConfig =
        Upload.config NoOp


# Config

@docs Config, config, configBase64EncodedMsg, configMaximumFileSize, configSetStateMsg, configUploadedMsg


# File accessors

@docs fileData, fileFilename, fileIsFailed, fileIsImage, fileProgress


# State

@docs State, UploadingFile, cancel, encode, failure, init, success, update, upload, uploads


# Subscriptions

@docs subscriptions, SubsConfig

-}

import File.Data.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.Data.UploadId as UploadId exposing (UploadId)
import Html.Events.Extra.Drag as Drag
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


--- PORTS -----
---- STATE ----


{-| Opaque type for the state of the uploader. The state includes the current uploads and information about if the user is
hovering over the dropzone
-}
type State
    = State StateRec


{-| Custom type for a file which holds the original Drag.File and the status of the upload
-}
type UploadingFile
    = UploadingFile Drag.File UploadStatus


type UploadStatus
    = ReadingBase64
    | Uploading Base64Encoded Float
    | Failed


{-| State used to represent this uploader

    - `dropActive` Is the DropZone currently 'active'; files are hovering over ready to be dropped
    - `uploads` Is the current collection of uploading files

-}
type alias StateRec =
    { dropActive : Bool
    , uploads : UploadId.Collection UploadingFile
    }


{-| Get the collection of uploads from the state
-}
uploads : State -> UploadId.Collection UploadingFile
uploads (State stateRec) =
    stateRec.uploads



---- CONFIG ----


{-| Opaque type for a Config record
-}
type Config msg
    = Config (ConfigRec msg)


{-| Configuration information for describing the behaviour of the Uploader
-}
type alias ConfigRec msg =
    { dragMsg : Bool -> msg
    , maximumFileSize : Int
    , uploadedMsg : Result UploadId ( UploadId, Encode.Value ) -> msg
    , base64EncodedMsg : Result UploadId ( UploadId, UploadingFile ) -> msg
    , setStateMsg : State -> msg
    , noOpMsg : msg
    }


{-| Init the uploader
-}
init : State
init =
    State <|
        { dropActive = False
        , uploads = UploadId.init
        }


{-| Init the configuration of this uploader with a no-op msg
-}
config : msg -> Config msg
config noOpMsg =
    Config <|
        { dragMsg = always noOpMsg
        , maximumFileSize = 5000
        , uploadedMsg = always noOpMsg
        , noOpMsg = noOpMsg
        , setStateMsg = always noOpMsg
        , base64EncodedMsg = always noOpMsg
        }


{-| Configure a message that can be used to update the internal state of the uploader
-}
configSetStateMsg : (State -> msg) -> Config msg -> Config msg
configSetStateMsg setStateMsg (Config configRec) =
    Config <|
        { configRec | setStateMsg = setStateMsg }


{-| Configure what happens when the file is uploaded
-}
configUploadedMsg : (Result UploadId ( UploadId, Encode.Value ) -> msg) -> Config msg -> Config msg
configUploadedMsg msg (Config configRec) =
    Config <|
        { configRec | uploadedMsg = msg }


{-| Configure what to do to encode the file to a Base64Encoded model
-}
configBase64EncodedMsg : (Result UploadId ( UploadId, UploadingFile ) -> msg) -> Config msg -> Config msg
configBase64EncodedMsg msg (Config configRec) =
    Config <|
        { configRec | base64EncodedMsg = msg }


{-| Configure maximum size of the uploaded files
-}
configMaximumFileSize : Int -> Config msg -> Config msg
configMaximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }



---- FILE ----


{-| Get the filename for an uploading file
-}
fileFilename : UploadingFile -> String
fileFilename (UploadingFile { name } _) =
    name


{-| Is the upload failed?
-}
fileIsFailed : UploadingFile -> Bool
fileIsFailed (UploadingFile _ uploadState) =
    case uploadState of
        Failed ->
            True

        _ ->
            False


{-| Is the uploading file an image?
-}
fileIsImage : UploadingFile -> Bool
fileIsImage (UploadingFile { mimeType } _) =
    String.startsWith "image" mimeType


{-| Get the percentage that the uploading file has uploaded
-}
fileProgress : UploadingFile -> Float
fileProgress file =
    case file of
        UploadingFile _ (Uploading _ percentage) ->
            percentage

        _ ->
            0.0


{-| Get the base64 data for an uploading file, if ready.
-}
fileData : UploadingFile -> Maybe Base64Encoded
fileData (UploadingFile file status) =
    case status of
        ReadingBase64 ->
            Nothing

        Uploading base64Encoded _ ->
            Just base64Encoded

        Failed ->
            Nothing



---- UPDATE ----


{-| Start a list of files uploading. Returns tuple with state of the uploader with the new files and Cmds for ports
-}
encode : SubsConfig msg -> Config msg -> List Drag.File -> State -> ( State, Cmd msg )
encode subsConfig (Config configRec) files (State stateRec) =
    let
        ( updatedUploadCollection, insertedIds ) =
            files
                |> List.map
                    (\file ->
                        UploadingFile file
                            (if file.size > configRec.maximumFileSize then
                                Failed
                             else
                                ReadingBase64
                            )
                    )
                |> List.foldl
                    (\file ( uploadsCollection, inserted ) ->
                        let
                            ( id, collection ) =
                                UploadId.insert file uploadsCollection
                        in
                        ( collection
                        , id :: inserted
                        )
                    )
                    ( stateRec.uploads, [] )
    in
    ( State { stateRec | uploads = updatedUploadCollection }
    , stateReadCmds subsConfig insertedIds updatedUploadCollection
    )


stateReadCmds : SubsConfig msg -> List UploadId -> UploadId.Collection UploadingFile -> Cmd msg
stateReadCmds { readFileContent } uploadIds collection =
    uploadIds
        |> List.filterMap
            (\id ->
                collection
                    |> UploadId.get id
                    |> Maybe.map
                        (\(UploadingFile { data } _) ->
                            readFileContent ( UploadId.encoder id, data )
                        )
            )
        |> Cmd.batch


{-| Update a particular upload, specified by an UploadId, with a new upload
-}
update : UploadId -> UploadingFile -> State -> State
update uploadId file (State state) =
    State { state | uploads = UploadId.update uploadId (always <| Just file) state.uploads }


{-| Updates a particular uploading file when it the base64 data has been successfully read from JS-land
-}
upload : SubsConfig msg -> Encode.Value -> Encode.Value -> UploadId -> State -> Cmd msg
upload { uploadPort } uploadUrl additionalData uploadId (State state) =
    case UploadId.get uploadId state.uploads of
        Just (UploadingFile rawFile (Uploading base64 _)) ->
            uploadPort
                { uploadId = UploadId.encoder uploadId
                , uploadUrl = uploadUrl
                , base64Data = Base64Encoded.encoder base64
                , additionalData = additionalData
                }

        _ ->
            Cmd.none


{-| When the file has been successfully uploaded it needs to be removed from the collection
-}
success : UploadId -> State -> State
success uploadId (State state) =
    State { state | uploads = UploadId.remove uploadId state.uploads }


{-| When the upload fails we change the status of the upload to Failed
-}
failure : UploadId -> State -> State
failure requestId (State state) =
    State <|
        { state
            | uploads =
                UploadId.update requestId
                    (Maybe.map
                        (\(UploadingFile rawFile _) ->
                            UploadingFile rawFile Failed
                        )
                    )
                    state.uploads
        }


{-| Updates the progress of an upload to S3 from JS-land with a new percentage
-}
progress : UploadId -> Float -> State -> State
progress id progressFloat (State state) =
    State <|
        { state
            | uploads =
                UploadId.update id
                    (Maybe.map
                        (\targetUpload ->
                            case targetUpload of
                                UploadingFile rawFile (Uploading base64 _) ->
                                    UploadingFile rawFile (Uploading base64 progressFloat)

                                _ ->
                                    targetUpload
                        )
                    )
                    state.uploads
        }


{-| Cancel an upload specified by the UploadId
Returns a tuple with:

  - The new internal state of the uploader, with the file removed
  - Cmds to cancel both the upload and any artifacts created during the upload process

-}
cancel : SubsConfig msg -> UploadId -> State -> ( State, Cmd msg )
cancel { uploadCancelled } uploadId (State state) =
    ( State { state | uploads = UploadId.remove uploadId state.uploads }
    , uploadCancelled (UploadId.encoder uploadId)
    )



---- ENCODER ----


metadataEncoder : UploadingFile -> Encode.Value
metadataEncoder (UploadingFile { mimeType, name, size } _) =
    Encode.object
        [ ( "contentType", Encode.string mimeType )
        , ( "fileName", Encode.string name )
        , ( "size", Encode.int size )
        ]


base64PortDecoder : State -> Decode.Decoder ( UploadId, UploadingFile )
base64PortDecoder (State stateRec) =
    Decode.field "id" UploadId.decoder
        |> Decode.andThen
            (\requestId ->
                case UploadId.get requestId stateRec.uploads of
                    Just (UploadingFile rawFile _) ->
                        Decode.map (\base64 -> Uploading base64 0.0) (Decode.field "result" Base64Encoded.decoder)
                            |> Decode.andThen (UploadingFile rawFile >> Tuple.pair requestId >> Decode.succeed)

                    _ ->
                        Decode.fail "Can't find request"
            )



-- SUBSCRIPTIONS ----


type alias Subs msg =
    { state : State
    , conf : Config msg
    , subs : SubsConfig msg
    }


{-| Configure subscriptions
-}
type alias SubsConfig msg =
    { uploadPort : { uploadId : Encode.Value, uploadUrl: Encode.Value, base64Data: Encode.Value, additionalData: Encode.Value } -> Cmd msg
    , readFileContent : ( Encode.Value, Decode.Value ) -> Cmd msg
    , uploadCancelled : Encode.Value -> Cmd msg
    , uploadProgress : (( Encode.Value, Float ) -> msg) -> Sub msg
    , fileContentReadFailed : (Encode.Value -> msg) -> Sub msg
    , fileContentRead : (Encode.Value -> msg) -> Sub msg
    , uploadFailed : (Encode.Value -> msg) -> Sub msg
    , uploaded : (( Encode.Value, Encode.Value ) -> msg) -> Sub msg
    }


{-| Subscriptions needed for the Uploader
-}
subscriptions : Subs msg -> Sub msg
subscriptions { state, conf, subs } =
    Sub.batch
        [ fileUploadedSub subs conf
        , fileFailureSub subs conf
        , fileUploadProgressSub subs state conf
        , base64EncodeFileSub subs state conf
        , readFileContentFailedSub subs conf
        ]


base64EncodeFileSub : SubsConfig msg -> State -> Config msg -> Sub msg
base64EncodeFileSub { fileContentRead } state (Config { base64EncodedMsg, noOpMsg }) =
    fileContentRead
        (\encodedValue ->
            case Decode.decodeValue (base64PortDecoder state) encodedValue of
                Ok uploadingFie ->
                    base64EncodedMsg (Ok uploadingFie)

                Err _ ->
                    noOpMsg
        )


fileUploadedSub : SubsConfig msg -> Config msg -> Sub msg
fileUploadedSub { uploaded } (Config { noOpMsg, uploadedMsg }) =
    uploaded
        (\( encodedId, encodedAttachment ) ->
            case Decode.decodeValue UploadId.decoder encodedId of
                Ok uploadId ->
                    uploadedMsg (Ok ( uploadId, encodedAttachment ))

                Err _ ->
                    noOpMsg
        )


readFileContentFailedSub : SubsConfig msg -> Config msg -> Sub msg
readFileContentFailedSub { fileContentReadFailed } (Config { noOpMsg, base64EncodedMsg }) =
    fileContentReadFailed
        (Decode.decodeValue UploadId.decoder
            >> Result.toMaybe
            >> Maybe.map (Err >> base64EncodedMsg)
            >> Maybe.withDefault noOpMsg
        )


fileFailureSub : SubsConfig msg -> Config msg -> Sub msg
fileFailureSub { uploadFailed } (Config { noOpMsg, uploadedMsg }) =
    uploadFailed
        (Decode.decodeValue UploadId.decoder
            >> Result.toMaybe
            >> Maybe.map (Err >> uploadedMsg)
            >> Maybe.withDefault noOpMsg
        )


fileUploadProgressSub : SubsConfig msg -> State -> Config msg -> Sub msg
fileUploadProgressSub { uploadProgress } state (Config { noOpMsg, setStateMsg }) =
    uploadProgress
        (\( id, uploadProgressFloat ) ->
            case Decode.decodeValue UploadId.decoder id of
                Ok uploadId ->
                    setStateMsg <| progress uploadId uploadProgressFloat state

                Err _ ->
                    setStateMsg state
        )
