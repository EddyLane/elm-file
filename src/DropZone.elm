port module File.DropZone
    exposing
        ( Config
        , State
        , config
        , configAttrs
        , configBrowseFiles
        , configContents
        , configInputId
        , configSetState
        , configUploadFiles
        , init
        , isActive
        , openFileBrowser
        , view
        )

{-| This library provides a UI element that acts as a "DropZone"; a place where users can drop files on to upload them
to a server.
-}

import Drag
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Decode


---- PORTS ----


{-| A port used to trigger the onClick event on an actual file input, specified by the String param
-}
port openFileBrowser : String -> Cmd msg



---- STATE ----


type State
    = State StateRec


type alias StateRec =
    { dropActive : Bool }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { uploadFilesMsg : List Drag.File -> msg
    , inputId : String
    , browseClickMsg : String -> msg
    , setStateMsg : State -> msg
    , viewFn : State -> msg -> List (Html msg)
    , attrsFn : State -> List (Attribute msg)
    }


init : State
init =
    State { dropActive = False }



---- CONFIG ----


{-| Init the configuration of this uploader with a no-op msg
-}
config : msg -> Config msg
config noOpMsg =
    Config <|
        { uploadFilesMsg = always noOpMsg
        , inputId = "elm-file-uploader"
        , browseClickMsg = always noOpMsg
        , setStateMsg = always noOpMsg
        , viewFn = always (always [])
        , attrsFn = always []
        }


{-| Set what happens when files are added to the upload queue
-}
configUploadFiles : (List Drag.File -> msg) -> Config msg -> Config msg
configUploadFiles msg (Config configRec) =
    Config <|
        { configRec | uploadFilesMsg = msg }


{-| Set the id of the input element used to upload files. If multiple uploaders this should be unique
-}
configInputId : String -> Config msg -> Config msg
configInputId inputId (Config configRec) =
    Config <|
        { configRec | inputId = inputId }


configSetState : (State -> msg) -> Config msg -> Config msg
configSetState msg (Config configRec) =
    Config <|
        { configRec | setStateMsg = msg }


{-| Set what happens when manually triggering the uploader (i.e. from an anchor tag)
-}
configBrowseFiles : (String -> msg) -> Config msg -> Config msg
configBrowseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


configContents : (State -> msg -> List (Html msg)) -> Config msg -> Config msg
configContents viewFn (Config configRec) =
    Config <|
        { configRec | viewFn = viewFn }


configAttrs : (State -> List (Attribute msg)) -> Config msg -> Config msg
configAttrs attrsFn (Config configRec) =
    Config <|
        { configRec | attrsFn = attrsFn }



---- VIEW ----


isActive : State -> Bool
isActive (State { dropActive }) =
    dropActive


view : State -> Config msg -> Html msg
view (State state) (Config config) =
    div (config.attrsFn (State state))
        [ dropZone state config
        , fileInput config
        ]


dropZone : StateRec -> ConfigRec msg -> Html msg
dropZone state config =
    let
        setDragging dragging =
            config.setStateMsg (State { state | dropActive = dragging })
    in
    div
        (List.concat
            [ config.attrsFn (State state)
            , [ Drag.onOver (always <| setDragging True)
              , Drag.onLeave (always <| setDragging False)
              , Drag.onDrop (.dataTransfer >> .files >> config.uploadFilesMsg)
              ]
            ]
        )
        (config.viewFn (State state) (config.browseClickMsg config.inputId))


fileInput : ConfigRec msg -> Html msg
fileInput { inputId, uploadFilesMsg } =
    input
        [ style [ ( "display", "none" ) ]
        , attribute "multiple" ""
        , type_ "file"
        , id inputId
        , onWithOptions
            "change"
            { stopPropagation = False
            , preventDefault = False
            }
            (fileInputDecoder inputId uploadFilesMsg)
        ]
        []


fileInputDecoder : String -> (List Drag.File -> msg) -> Decode.Decoder msg
fileInputDecoder inputId msg =
    Drag.fileDecoder
        |> Drag.fileListDecoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map msg
