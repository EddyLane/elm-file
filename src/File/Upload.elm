module File.Upload exposing
    ( Config, config, configBase64EncodedMsg, configMaximumFileSize, configSetStateMsg, configUploadedMsg
    , fileData, fileFilename, fileIsFailed, fileIsImage, fileProgress
    , State, UploadingFile, cancel, encode, failure, init, success, update, upload, uploads
    , subscriptions
    , PortMsg, Ports, configPorts
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

@docs subscriptions

-}

import File.Data.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.Data.UploadId as UploadId exposing (UploadId)
import Html.Events.Extra.Drag as Drag
import Json.Decode as Decode
import Json.Encode as Encode



--- PORTS -----


type alias PortMsg =
    { message : Encode.Value
    , uploadId : Encode.Value
    , data : Encode.Value
    }


type alias PortMsgDecoded =
    { data : Encode.Value
    , message : String
    , uploadId : UploadId
    }


type alias Ports msg =
    { cmd : PortMsg -> Cmd msg
    , sub : (PortMsg -> msg) -> Sub msg
    }


decodePortMsg : PortMsg -> Maybe PortMsgDecoded
decodePortMsg { message, uploadId, data } =
    Result.toMaybe <|
        Result.map2
            (PortMsgDecoded data)
            (Decode.decodeValue Decode.string message)
            (Decode.decodeValue UploadId.decoder uploadId)



---- STATE ----


{-| Opaque type for the state of the uploader. The state includes the current uploads and information about if the user is
hovering over the dropzone
-}
type State
    = State (UploadId.Collection UploadingFile)


{-| Custom type for a file which holds the original Drag.File and the status of the upload
-}
type UploadingFile
    = UploadingFile Drag.File UploadStatus


type UploadStatus
    = ReadingBase64
    | Uploading Base64Encoded Float
    | Failed


{-| Get the collection of uploads from the state
-}
uploads : State -> UploadId.Collection UploadingFile
uploads (State uploadsCollection) =
    uploadsCollection



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
    , ports : Ports msg
    }


{-| Init the uploader
-}
init : State
init =
    State UploadId.init


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
        , ports =
            { cmd = always Cmd.none
            , sub = always Sub.none
            }
        }


configPorts : { cmd : PortMsg -> Cmd msg, sub : (PortMsg -> msg) -> Sub msg } -> Config msg -> Config msg
configPorts ports (Config configRec) =
    Config <|
        { configRec | ports = ports }


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
encode : Config msg -> List Drag.File -> State -> ( State, Cmd msg )
encode (Config configRec) files (State uploadsCollection) =
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
                    (\file ( collection, inserted ) ->
                        let
                            ( id, newCollection ) =
                                UploadId.insert file collection
                        in
                        ( newCollection
                        , id :: inserted
                        )
                    )
                    ( uploadsCollection, [] )
    in
    ( State updatedUploadCollection
    , stateReadCmds configRec.ports insertedIds updatedUploadCollection
    )


stateReadCmds : Ports msg -> List UploadId -> UploadId.Collection UploadingFile -> Cmd msg
stateReadCmds ports uploadIds collection =
    uploadIds
        |> List.filterMap
            (\id ->
                collection
                    |> UploadId.get id
                    |> Maybe.map
                        (\(UploadingFile { data } _) ->
                            ports.cmd
                                { message = Encode.string "encode"
                                , uploadId = UploadId.encoder id
                                , data = data
                                }
                        )
            )
        |> Cmd.batch


{-| Update a particular upload, specified by an UploadId, with a new upload
-}
update : UploadId -> UploadingFile -> State -> State
update uploadId file (State uploadsCollection) =
    State <| UploadId.update uploadId (always <| Just file) uploadsCollection


{-| Updates a particular uploading file when it the base64 data has been successfully read from JS-land
-}
upload : Config msg -> Encode.Value -> Encode.Value -> UploadId -> State -> Cmd msg
upload (Config { ports }) uploadUrl additionalData uploadId (State uploadsCollection) =
    case UploadId.get uploadId uploadsCollection of
        Just (UploadingFile rawFile (Uploading base64 _)) ->
            ports.cmd
                { message = Encode.string "upload"
                , uploadId = UploadId.encoder uploadId
                , data =
                    Encode.object
                        [ ( "uploadUrl", uploadUrl )
                        , ( "base64Data", Base64Encoded.encoder base64 )
                        , ( "additionalData", additionalData )
                        ]
                }

        _ ->
            Cmd.none


{-| When the file has been successfully uploaded it needs to be removed from the collection
-}
success : UploadId -> State -> State
success uploadId (State uploadsCollection) =
    State <| UploadId.remove uploadId uploadsCollection


{-| When the upload fails we change the status of the upload to Failed
-}
failure : UploadId -> State -> State
failure requestId (State uploadsCollection) =
    State <|
        UploadId.update requestId
            (Maybe.map
                (\(UploadingFile rawFile _) ->
                    UploadingFile rawFile Failed
                )
            )
            uploadsCollection


{-| Updates the progress of an upload to S3 from JS-land with a new percentage
-}
progress : UploadId -> Float -> State -> State
progress id progressFloat (State uploadsCollection) =
    State <|
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
            uploadsCollection


{-| Cancel an upload specified by the UploadId
Returns a tuple with:

  - The new internal state of the uploader, with the file removed
  - Cmds to cancel both the upload and any artifacts created during the upload process

-}



--    { message : Encode.Value
--    , uploadId : Encode.Value
--    , data : Encode.Value
--    , file : Decode.Value
--    }


cancel : Config msg -> UploadId -> State -> ( State, Cmd msg )
cancel (Config { ports }) uploadId (State uploadsCollection) =
    ( State <| UploadId.remove uploadId uploadsCollection
    , ports.cmd
        { message = Encode.string "cancel"
        , uploadId = UploadId.encoder uploadId
        , data = Encode.null
        }
    )



---- ENCODER ----


metadataEncoder : UploadingFile -> Encode.Value
metadataEncoder (UploadingFile { mimeType, name, size } _) =
    Encode.object
        [ ( "contentType", Encode.string mimeType )
        , ( "fileName", Encode.string name )
        , ( "size", Encode.int size )
        ]



-- SUBSCRIPTIONS ----


{-| Subscriptions needed for the Uploader
-}
subscriptions : Config msg -> State -> Sub msg
subscriptions ((Config { ports, noOpMsg }) as conf) state =
    ports.sub
        (decodePortMsg
            >> Maybe.map (dispatchSub conf state)
            >> Maybe.withDefault noOpMsg
        )


dispatchSub : Config msg -> State -> PortMsgDecoded -> msg
dispatchSub ((Config { noOpMsg }) as conf) state portsMsg =
    case portsMsg.message of
        "encode" ->
            subEncode conf state portsMsg

        _ ->
            noOpMsg


subEncode : Config msg -> State -> PortMsgDecoded -> msg
subEncode (Config { noOpMsg, base64EncodedMsg }) (State uploadsCollection) { uploadId, data } =
    case UploadId.get uploadId uploadsCollection of
        Just (UploadingFile rawFile _) ->
            data
                |> Decode.decodeValue
                    (Decode.map (\base64 -> Uploading base64 0.0) (Decode.field "result" Base64Encoded.decoder)
                        |> Decode.andThen (UploadingFile rawFile >> Tuple.pair uploadId >> Decode.succeed)
                    )
                |> Result.map (Ok >> base64EncodedMsg)
                |> Result.withDefault (base64EncodedMsg (Err uploadId))

        _ ->
            base64EncodedMsg (Err uploadId)



--    case Decode.decodeValue (base64PortDecoder state) data of
--        Ok uploadingFie ->
--            base64EncodedMsg (Ok uploadingFie)
--
--        Err _ ->
--            noOpMsg
--    Sub.batch
--        [ fileUploadedSub subs conf
----        , fileFailureSub subs conf
----        , fileUploadProgressSub subs state conf
----        , base64EncodeFileSub subs state conf
----        , readFileContentFailedSub subs conf
--        ]
--
--fileUploadedSub : SubsConfig msg -> Config msg -> Sub msg
--fileUploadedSub { uploaded } (Config { noOpMsg, uploadedMsg }) =
--    uploaded
--        (\( encodedId, encodedAttachment ) ->
--            case Decode.decodeValue UploadId.decoder encodedId of
--                Ok uploadId ->
--                    uploadedMsg (Ok ( uploadId, encodedAttachment ))
--
--                Err _ ->
--                    noOpMsg
--        )
--
--
--readFileContentFailedSub : SubsConfig msg -> Config msg -> Sub msg
--readFileContentFailedSub { fileContentReadFailed } (Config { noOpMsg, base64EncodedMsg }) =
--    fileContentReadFailed
--        (Decode.decodeValue UploadId.decoder
--            >> Result.toMaybe
--            >> Maybe.map (Err >> base64EncodedMsg)
--            >> Maybe.withDefault noOpMsg
--        )
--
--
--fileFailureSub : SubsConfig msg -> Config msg -> Sub msg
--fileFailureSub { uploadFailed } (Config { noOpMsg, uploadedMsg }) =
--    uploadFailed
--        (Decode.decodeValue UploadId.decoder
--            >> Result.toMaybe
--            >> Maybe.map (Err >> uploadedMsg)
--            >> Maybe.withDefault noOpMsg
--        )
--
--
--fileUploadProgressSub : SubsConfig msg -> State -> Config msg -> Sub msg
--fileUploadProgressSub { uploadProgress } state (Config { noOpMsg, setStateMsg }) =
--    uploadProgress
--        (\( id, uploadProgressFloat ) ->
--            case Decode.decodeValue UploadId.decoder id of
--                Ok uploadId ->
--                    setStateMsg <| progress uploadId uploadProgressFloat state
--
--                Err _ ->
--                    setStateMsg state
--        )
