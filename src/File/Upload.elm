module File.Upload exposing
    ( Config, config, configBase64EncodedMsg, configMaximumFileSize, configSetStateMsg, configUploadedMsg
    , fileData, fileFilename, fileIsFailed, fileIsImage, fileProgress
    , State, UploadingFile, cancel, encode, failure, init, success, update, upload, uploads
    , subscriptions
    , PortCmdMsg, Ports, configAllowedMimeTypes, configPorts, fileError
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


type alias PortCmdMsg =
    { message : Encode.Value
    , uploadId : Encode.Value
    , data : Encode.Value
    }


type alias PortSubMsg =
    { data : Result String Encode.Value
    , message : String
    , uploadId : UploadId
    }


type alias Ports msg =
    { cmd : PortCmdMsg -> Cmd msg
    , sub : (PortCmdMsg -> msg) -> Sub msg
    }


decodePortMsg : PortCmdMsg -> Maybe PortSubMsg
decodePortMsg { message, uploadId, data } =
    Result.toMaybe <|
        Result.map2
            (PortSubMsg <| decodePortMsgData data)
            (Decode.decodeValue Decode.string message)
            (Decode.decodeValue UploadId.decoder uploadId)


decodePortMsgData : Encode.Value -> Result String Encode.Value
decodePortMsgData data =
    data
        |> Decode.decodeValue (Decode.field "error" Decode.string |> Decode.andThen (Err >> Decode.succeed))
        |> Result.withDefault (Ok data)



---- STATE ----


{-| Opaque type for the state of the uploader
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
    | Failed (List String)


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
    , allowedMimeTypes : Maybe (List String)
    , uploadedMsg : Result ( UploadId, String ) ( UploadId, Encode.Value ) -> msg
    , base64EncodedMsg : Result ( UploadId, String ) ( UploadId, UploadingFile ) -> msg
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
        , allowedMimeTypes = Nothing
        , uploadedMsg = always noOpMsg
        , noOpMsg = noOpMsg
        , setStateMsg = always noOpMsg
        , base64EncodedMsg = always noOpMsg
        , ports =
            { cmd = always Cmd.none
            , sub = always Sub.none
            }
        }


configPorts : { cmd : PortCmdMsg -> Cmd msg, sub : (PortCmdMsg -> msg) -> Sub msg } -> Config msg -> Config msg
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
configUploadedMsg : (Result ( UploadId, String ) ( UploadId, Encode.Value ) -> msg) -> Config msg -> Config msg
configUploadedMsg msg (Config configRec) =
    Config <|
        { configRec | uploadedMsg = msg }


{-| Configure what to do to encode the file to a Base64Encoded model
-}
configBase64EncodedMsg : (Result ( UploadId, String ) ( UploadId, UploadingFile ) -> msg) -> Config msg -> Config msg
configBase64EncodedMsg msg (Config configRec) =
    Config <|
        { configRec | base64EncodedMsg = msg }


{-| Configure maximum size of the uploaded files
-}
configMaximumFileSize : Int -> Config msg -> Config msg
configMaximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }


configAllowedMimeTypes : List String -> Config msg -> Config msg
configAllowedMimeTypes allowedMimeTypes (Config configRec) =
    Config <|
        { configRec | allowedMimeTypes = Just allowedMimeTypes }



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
        Failed _ ->
            True

        _ ->
            False


fileError : UploadingFile -> String
fileError (UploadingFile _ uploadState) =
    case uploadState of
        Failed errors ->
            String.join " - " errors

        _ ->
            ""


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

        Failed _ ->
            Nothing



---- UPDATE ----
--validateFile : ConfigRec msg -> Drag.File -> List String
--validateFile configRec file =
--    let
--        invalidSize =
--            ( file.size > configRec.maximumFileSize, "Above maximum file size" )
--
--        invalidMimeType =
--            case configRec.allowedMimeTypes of
--                Just allowed ->
--                     not (List.member file.mimeType configRec.allowedMimeTypes)
--                Nothing ->
--                    False
--
--            ( not (List.member file.mimeType configRec.allowedMimeTypes), "Invalid file type" )
--    in
--    [ validateSize configRec file
--    , invalidMimeType configRec file
--    ]
--        |> List.filterMap
--            (\( invalid, errorMessage ) ->
--                if invalid then
--                    Just errorMessage
--
--                else
--                    Nothing
--            )


validate : ConfigRec msg -> Drag.File -> List String
validate configRec file =
    List.filterMap identity
        [ validateSize configRec file
        , validateMimeType configRec file
        ]


validateSize : ConfigRec msg -> Drag.File -> Maybe String
validateSize configRec file =
    if file.size > configRec.maximumFileSize then
        Just "Above maximum file size"

    else
        Nothing


validateMimeType : ConfigRec msg -> Drag.File -> Maybe String
validateMimeType configRec file =
    configRec.allowedMimeTypes
        |> Maybe.andThen
            (\allowedMimeTypes ->
                if List.member file.mimeType allowedMimeTypes then
                    Nothing

                else
                    Just "Invalid file type"
            )


{-| Start a list of files uploading. Returns tuple with state of the uploader with the new files and Cmds for ports
-}
encode : Config msg -> List Drag.File -> State -> ( State, Cmd msg )
encode (Config configRec) files (State uploadsCollection) =
    let
        uploadingFiles =
            List.map
                (\file ->
                    let
                        errors =
                            validate configRec file
                    in
                    if List.isEmpty errors then
                        UploadingFile file ReadingBase64

                    else
                        UploadingFile file (Failed errors)
                )
                files

        ( updatedUploadCollection, successfulUploadIds ) =
            uploadingFiles
                |> List.foldl
                    (\((UploadingFile _ status) as file) ( collection, inserted ) ->
                        let
                            ( id, newCollection ) =
                                UploadId.insert file collection
                        in
                        case status of
                            ReadingBase64 ->
                                ( newCollection
                                , id :: inserted
                                )

                            _ ->
                                ( newCollection
                                , inserted
                                )
                    )
                    ( uploadsCollection, [] )
    in
    ( State updatedUploadCollection
    , stateReadCmds configRec.ports successfulUploadIds updatedUploadCollection
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
failure : UploadId -> String -> State -> State
failure requestId failureReason (State uploadsCollection) =
    State <|
        UploadId.update requestId
            (Maybe.map
                (\(UploadingFile rawFile _) ->
                    UploadingFile rawFile (Failed [ failureReason ])
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


dispatchSub : Config msg -> State -> PortSubMsg -> msg
dispatchSub ((Config confRec) as conf) state portsMsg =
    case portsMsg.message of
        "encode" ->
            subEncoded confRec state portsMsg

        "upload" ->
            subUploaded confRec state portsMsg

        "progress" ->
            subProgress confRec state portsMsg

        _ ->
            confRec.noOpMsg


subEncoded : ConfigRec msg -> State -> PortSubMsg -> msg
subEncoded { base64EncodedMsg } (State uploadsCollection) portsMsg =
    case portsMsg.data of
        Ok data ->
            case UploadId.get portsMsg.uploadId uploadsCollection of
                Just (UploadingFile rawFile _) ->
                    data
                        |> Decode.decodeValue
                            (Decode.map (\base64 -> Uploading base64 0.0) Base64Encoded.decoder
                                |> Decode.andThen (UploadingFile rawFile >> Tuple.pair portsMsg.uploadId >> Decode.succeed)
                            )
                        |> Result.map (Ok >> base64EncodedMsg)
                        |> Result.withDefault (base64EncodedMsg (Err ( portsMsg.uploadId, "Problem decoding base64 data" )))

                Nothing ->
                    base64EncodedMsg (Err ( portsMsg.uploadId, "Could not find file" ))

        Err err ->
            base64EncodedMsg (Err ( portsMsg.uploadId, err ))


subUploaded : ConfigRec msg -> State -> PortSubMsg -> msg
subUploaded { noOpMsg, uploadedMsg } _ portsMsg =
    case portsMsg.data of
        Ok data ->
            uploadedMsg (Ok ( portsMsg.uploadId, data ))

        Err err ->
            uploadedMsg (Err ( portsMsg.uploadId, err ))


subProgress : ConfigRec msg -> State -> PortSubMsg -> msg
subProgress { noOpMsg, setStateMsg } state portsMsg =
    case portsMsg.data of
        Ok data ->
            data
                |> Decode.decodeValue Decode.float
                |> Result.map (\progressFloat -> setStateMsg <| progress portsMsg.uploadId progressFloat state)
                |> Result.withDefault noOpMsg

        Err _ ->
            noOpMsg
