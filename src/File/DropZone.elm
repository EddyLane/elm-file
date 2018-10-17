module File.DropZone exposing
    ( State, init, isActive, view
    , Config, config, configAttrs, configBrowseFiles, configContents, configInputId, configSetState, configUploadFiles
    , configPorts, dropZoneDragAttrs, openFileBrowser, subscriptions
    )

{-| This library provides a UI element that acts as a "DropZone"; a place where users can drop files on to upload them
to a server.


# State

@docs State, init, isActive, view


# Config

@docs Config, config, configAttrs, configBrowseFiles, configContents, configInputId, configSetState, configUploadFiles

-}

import File.Upload exposing (PortCmdMsg, Ports)
import Html.Events exposing (custom, onClick)
import Html.Events.Extra.Drag as Drag
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (attribute, class, id, style, type_)
import Html.Styled.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode as Encode



---- PORTS ----


openFileBrowser : Config msg -> String -> Cmd msg
openFileBrowser (Config confRec) inputId =
    confRec.ports.cmd
        { message = Encode.string "open-file-browser"
        , uploadId = Encode.null
        , data = Encode.string inputId
        }



---- STATE ----


{-| Opaque type holding the state of the DropZone component
-}
type State
    = State StateRec


type alias StateRec =
    { dropActive : Bool }


{-| init the DropZone component
-}
init : State
init =
    State { dropActive = False }


{-| Bool depending on if the component is being hovered over
-}
isActive : State -> Bool
isActive (State { dropActive }) =
    dropActive



---- CONFIG ----


{-| Opaque type holding the configuration of the DropZone component
-}
type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { uploadFilesMsg : List Drag.File -> msg
    , inputId : String
    , browseClickMsg : String -> msg
    , setStateMsg : State -> msg
    , viewFn : State -> msg -> List (Html msg)
    , attrsFn : State -> List (Attribute msg)
    , noOpMsg : msg
    , ports : Ports msg
    }


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
        , noOpMsg = noOpMsg
        , ports =
            { cmd = always Cmd.none
            , sub = always Sub.none
            }
        }


configPorts : { cmd : PortCmdMsg -> Cmd msg, sub : (PortCmdMsg -> msg) -> Sub msg } -> Config msg -> Config msg
configPorts ports (Config configRec) =
    Config <|
        { configRec | ports = ports }


{-| Configure a msg that will update the state of this component. Often you will be forced to fill in the blanks when
you use this library, but sometimes we'll do the leg work for you.
-}
configSetState : (State -> msg) -> Config msg -> Config msg
configSetState msg (Config configRec) =
    Config <|
        { configRec | setStateMsg = msg }


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


{-| Set what happens when manually triggering the uploader (i.e. from an anchor tag)
-}
configBrowseFiles : (String -> msg) -> Config msg -> Config msg
configBrowseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


{-| Configure how to display the contents of the DropZone. Takes a function which takes the state of the dropzone, and
an event to open a file browser to upload more files
-}
configContents : (State -> msg -> List (Html msg)) -> Config msg -> Config msg
configContents viewFn (Config configRec) =
    Config <|
        { configRec | viewFn = viewFn }


{-| Configure the attrs to add to the component
-}
configAttrs : (State -> List (Attribute msg)) -> Config msg -> Config msg
configAttrs attrsFn (Config configRec) =
    Config <|
        { configRec | attrsFn = attrsFn }



---- VIEW ----


{-| Display the component
-}
view : State -> Config msg -> Html msg
view (State state) (Config configRec) =
    div (configRec.attrsFn (State state))
        [ dropZone state configRec
        , fileInput configRec
        ]


dropZone : StateRec -> ConfigRec msg -> Html msg
dropZone state configRec =
    div
        (List.concat
            [ configRec.attrsFn (State state)
            , dropZoneDragAttrs (State state) (Config configRec)
            ]
        )
        (configRec.viewFn (State state) (configRec.browseClickMsg configRec.inputId))


dropZoneDragAttrs : State -> Config msg -> List (Attribute msg)
dropZoneDragAttrs (State stateRec) (Config configRec) =
    let
        setDragging dragging =
            configRec.setStateMsg (State { stateRec | dropActive = dragging })
    in
    List.map Attributes.fromUnstyled
        (Drag.onDropTarget
            { dropEffect = Drag.CopyOnDrop
            , onEnter = Just <| always <| setDragging True
            , onLeave = Just <| always <| setDragging False
            , onDrop = .dataTransfer >> .files >> configRec.uploadFilesMsg
            , onOver = always (always configRec.noOpMsg)
            }
        )


fileInput : ConfigRec msg -> Html msg
fileInput { inputId, uploadFilesMsg, noOpMsg } =
    input
        [ style "display" "none"
        , attribute "multiple" ""
        , type_ "file"
        , id inputId
        , on "change" (fileInputDecoder uploadFilesMsg)
        ]
        []


fileInputDecoder : (List Drag.File -> msg) -> Decode.Decoder msg
fileInputDecoder msg =
    Drag.fileDecoder
        |> Drag.fileListDecoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map msg



---- SUBSCRIPTIONS ----


subscriptions : Config msg -> State -> Sub msg
subscriptions _ _ =
    Sub.none
