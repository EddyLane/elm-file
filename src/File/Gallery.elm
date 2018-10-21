module File.Gallery exposing
    ( Config
    , PortCmdMsg
    , State
    , UploadState
    , config
    , configCancelUploadMsg
    , configContentTypeFn
    , configIdFn
    , configNameFn
    , configPorts
    , configSetState
    , configThumbnailSrcFn
    , getBoundingClientRects
    , init
    , subscriptions
    , view
    )

import Css exposing (..)
import Css.Media
import Css.Transitions exposing (easeInOut, transition)
import Dict exposing (Dict)
import FeatherIcons
import File.Data.Base64Encoded as Base64Encoded
import File.Data.UploadId as UploadId exposing (UploadId)
import File.Upload as Upload exposing (UploadingFile)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id, src)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



---- PORTS ----


type alias PortCmdMsg =
    { message : Encode.Value
    , data : Encode.Value
    }


type alias PortSubMsg =
    { data : Result String Encode.Value
    , message : String
    }


type alias Ports msg =
    { cmd : PortCmdMsg -> Cmd msg
    , sub : (PortCmdMsg -> msg) -> Sub msg
    }


getBoundingClientRects : Config file msg -> List file -> Upload.State -> Cmd msg
getBoundingClientRects (Config confRec) uploaded uploading =
    confRec.ports.cmd
        { message = Encode.string "get-bounding-client-rects"
        , data =
            uploading
                |> combineUploadedAndUploading uploaded
                |> List.map (fileId confRec)
                |> Encode.list Encode.string
        }


decodePortMsg : PortCmdMsg -> Maybe PortSubMsg
decodePortMsg { message, data } =
    Result.toMaybe <|
        Result.map
            (PortSubMsg <| decodePortMsgData data)
            (Decode.decodeValue Decode.string message)


decodePortMsgData : Encode.Value -> Result String Encode.Value
decodePortMsgData data =
    data
        |> Decode.decodeValue (Decode.field "error" Decode.string |> Decode.andThen (Err >> Decode.succeed))
        |> Result.withDefault (Ok data)



---- MODEL ----


type State
    = State StateRec


init : State
init =
    State { current = Dict.empty }


type alias StateRec =
    { current : Dict String Rectangle }


type alias Rectangle =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    , x : Float
    , y : Float
    }


rectDecoder : Decoder Rectangle
rectDecoder =
    Decode.map8 Rectangle
        (Decode.field "bottom" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "left" Decode.float)
        (Decode.field "right" Decode.float)
        (Decode.field "top" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


type UploadState file
    = Uploading UploadId UploadingFile
    | Uploaded file


{-| Opaque type for a Config record
-}
type Config file msg
    = Config (ConfigRec file msg)


type alias ConfigRec file msg =
    { idFn : file -> String
    , nameFn : file -> String
    , contentTypeFn : file -> String
    , cancelUploadMsg : UploadId -> msg
    , thumbnailSrcFn : file -> String
    , ports : Ports msg
    , noOpMsg : msg
    , setStateMsg : State -> msg
    }


{-| Get a default Gallery configuration that you can further customise by calling additional config functions.
The argument to this function should be a NoOp message, this is because the Gallery component has lots of
functionality, much of which you may not require. Anything you don't need will just default to the NoOp message you
have specified.
-}
config : msg -> Config file msg
config noOpMsg =
    Config <|
        { idFn = always "-"
        , nameFn = always "-"
        , contentTypeFn = always "-"
        , cancelUploadMsg = always noOpMsg
        , thumbnailSrcFn = always ""
        , ports =
            { cmd = always Cmd.none
            , sub = always Sub.none
            }
        , noOpMsg = noOpMsg
        , setStateMsg = always noOpMsg
        }


configPorts : { cmd : PortCmdMsg -> Cmd msg, sub : (PortCmdMsg -> msg) -> Sub msg } -> Config file msg -> Config file msg
configPorts ports (Config configRec) =
    Config <|
        { configRec | ports = ports }


{-| Configure a msg that will update the state of this component. Often you will be forced to fill in the blanks when
you use this library, but sometimes we'll do the leg work for you.
-}
configSetState : (State -> msg) -> Config file msg -> Config file msg
configSetState msg (Config configRec) =
    Config <|
        { configRec | setStateMsg = msg }


{-| Configure the way in which we can retrieve a unique id (as a string) from a user defined uploaded file
-}
configIdFn : (file -> String) -> Config file msg -> Config file msg
configIdFn idFn (Config configRec) =
    Config <|
        { configRec | idFn = idFn }


{-| Configure a way in which we can retrieve the original file name from a user defined uploaded file type, this should
match the original filename (including extension).
-}
configNameFn : (file -> String) -> Config file msg -> Config file msg
configNameFn nameFn (Config configRec) =
    Config <|
        { configRec | nameFn = nameFn }


{-| Configure a way in which we can retrieve the content type for a user defined uploaded file, this should match the
original content type
-}
configContentTypeFn : (file -> String) -> Config file msg -> Config file msg
configContentTypeFn contentTypeFn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = contentTypeFn }


{-| Configure a way in which we can get a string for the src value of a file which we determine is an image from the
content type
-}
configThumbnailSrcFn : (file -> String) -> Config file msg -> Config file msg
configThumbnailSrcFn thumbnailSrcFn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = thumbnailSrcFn }


{-| Configure a way in which we can cancel an upload
-}
configCancelUploadMsg : (UploadId -> msg) -> Config file msg -> Config file msg
configCancelUploadMsg cancelUploadMsg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = cancelUploadMsg }


view : Maybe (Html msg) -> State -> List file -> Upload.State -> Config file msg -> Html msg
view maybeDropZone (State stateRec) uploaded uploading (Config configRec) =
    let
        dropZone =
            maybeDropZone
                |> Maybe.map
                    (\html ->
                        div
                            [ css [ viewGalleryItemContainerStyle ] ]
                            [ div [ css [ viewGalleryItemStyle ] ] [ html ] ]
                    )
                |> Maybe.withDefault (text "")
    in
    div
        [ css
            [ maxWidth (px 980)
            , margin2 (px 0) auto
            ]
        ]
        [ div
            [ id "gallery-visible"
            , css [ display block ]
            ]
            [ viewGallery dropZone uploaded uploading configRec ]
        , div
            [ id "gallery-hidden"
            , css [ display block, position absolute, width (pct 100), height (pct 100) ]
            ]
            [ viewHiddenGallery dropZone stateRec ]
        ]


viewGallery : Html msg -> List file -> Upload.State -> ConfigRec file msg -> Html msg
viewGallery dropZone uploaded uploading configRec =
    let
        galleryItems =
            viewItems uploaded uploading configRec
    in
    div [] (dropZone :: galleryItems)


viewHiddenGallery : Html msg -> StateRec -> Html msg
viewHiddenGallery dropZone stateRec =
    let
        galleryItems =
            stateRec.current
                |> Dict.values
                |> List.map
                    (\c ->
                        div
                            [ css
                                [ viewGalleryItemContainerStyle
                                , border3 (px 1) dashed (hex "000")
                                ]
                            ]
                            [ div
                                [ css [ viewGalleryItemStyle ] ]
                                [ text "hi" ]
                            ]
                    )
    in
    div [] (dropZone :: galleryItems)


viewToolbar : Html msg
viewToolbar =
    div []
        [ button
            [ css [ border (px 0) ] ]
            [ text "Button" ]
        ]


viewItems : List file -> Upload.State -> ConfigRec file msg -> List (Html msg)
viewItems uploaded uploading configRec =
    uploading
        |> combineUploadedAndUploading uploaded
        |> List.map (viewItem configRec)


viewItem : ConfigRec file msg -> UploadState file -> Html msg
viewItem configRec upload =
    viewGalleryItemContainer (fileId configRec upload)
        [ viewItemThumbnail configRec upload
        , viewItemUploadOverlay configRec upload
        ]


combineUploadedAndUploading : List file -> Upload.State -> List (UploadState file)
combineUploadedAndUploading files uploadState =
    List.concat
        [ List.map Uploaded files
        , uploadState
            |> Upload.uploads
            |> UploadId.toList
            |> List.map (\( id, uploading ) -> Uploading id uploading)
        ]


viewGalleryItemContainer : String -> List (Html msg) -> Html msg
viewGalleryItemContainer elementId inner =
    div
        [ id elementId
        , css [ viewGalleryItemContainerStyle ]
        ]
        inner


viewGalleryItemContainerStyle : Style
viewGalleryItemContainerStyle =
    let
        media { maxWidth, itemWidth, itemMargin } =
            Css.Media.withMedia
                [ Css.Media.only Css.Media.screen [ Css.Media.maxWidth maxWidth ] ]
                [ width (calc itemWidth minus (px 2))
                , paddingBottom itemWidth
                , margin itemMargin
                ]
    in
    Css.batch
        [ float left
        , position relative
        , width (calc (pct 15) minus (px 2))
        , paddingBottom (pct 15)
        , margin (pct 0.83)
        , overflow hidden
        , hover
            [ boxShadow5 (px 5) (px 5) (px 5) (px 0) (rgba 0 0 0 0.25)
            , border3 (px 1) solid (rgb 249 229 89)
            ]
        , transition
            [ Css.Transitions.boxShadow 500
            , Css.Transitions.border 200
            ]
        , media { maxWidth = px 992, itemWidth = pct 22, itemMargin = pct 1.5 }
        , media { maxWidth = px 720, itemWidth = pct 29, itemMargin = pct 2.16 }
        , media { maxWidth = px 470, itemWidth = pct 44, itemMargin = pct 3.0 }
        , borderRadius4 (pct 5) (pct 5) (pct 5) (pct 5)
        , border3 (px 1) solid (hex "fff")
        , color (hex "fff")
        , overflow hidden
        ]


viewGalleryItemStyle : Style
viewGalleryItemStyle =
    Css.batch
        [ position absolute
        , left (px 0)
        , top (px 0)
        , width (pct 100)
        , height (pct 100)
        ]


viewItemUploadOverlay : ConfigRec file msg -> UploadState file -> Html msg
viewItemUploadOverlay configRec uploadState =
    case uploadState of
        Uploaded _ ->
            div
                [ css
                    [ position absolute
                    , backgroundColor (hex "000000")
                    , opacity (Css.num 0.5)
                    , left (px 0)
                    , zIndex (int 1)
                    ]
                ]
                []

        Uploading _ uploading ->
            div
                [ css
                    [ position absolute
                    , backgroundColor (hex "000000")
                    , opacity (Css.num 0.5)
                    , left (pct (Upload.fileProgress uploading))
                    , zIndex (int 1)
                    ]
                ]
                []


viewItemThumbnail : ConfigRec file msg -> UploadState file -> Html msg
viewItemThumbnail configRec uploadState =
    case thumbnailSrc configRec uploadState of
        Just thumbSrc ->
            img
                [ src thumbSrc
                , css
                    [ viewGalleryItemStyle
                    , property "object-fit" "cover"
                    , zIndex (int 0)
                    ]
                ]
                []

        Nothing ->
            div
                [ css [ viewGalleryItemStyle ]
                ]
                [ FeatherIcons.fileText
                    |> FeatherIcons.withSizeUnit "%"
                    |> FeatherIcons.withSize 50
                    |> FeatherIcons.toHtml []
                    |> fromUnstyled
                , text (fileName configRec uploadState)
                ]


fileName : ConfigRec file msg -> UploadState file -> String
fileName configRec uploadState =
    case uploadState of
        Uploading _ file ->
            Upload.fileFilename file

        Uploaded file ->
            configRec.nameFn file


thumbnailSrc : ConfigRec file msg -> UploadState file -> Maybe String
thumbnailSrc configRec uploadState =
    case ( isImage configRec uploadState, uploadState ) of
        ( True, Uploaded file ) ->
            Just (configRec.thumbnailSrcFn file)

        ( True, Uploading _ file ) ->
            file
                |> Upload.fileData
                |> Maybe.map Base64Encoded.toString

        _ ->
            Nothing


fileId : ConfigRec file msg -> UploadState file -> String
fileId configRec uploadState =
    case uploadState of
        Uploading _ file ->
            Upload.fileFilename file

        Uploaded file ->
            configRec.idFn file


isImage : ConfigRec file msg -> UploadState file -> Bool
isImage configRec uploadState =
    case uploadState of
        Uploaded file ->
            String.startsWith "image" (configRec.contentTypeFn file)

        Uploading _ file ->
            Upload.fileIsImage file



-- SUBSCRIPTIONS ----


{-| Subscriptions needed for the Gallery
-}
subscriptions : Config file msg -> State -> Sub msg
subscriptions ((Config confRec) as conf) (State stateRec) =
    confRec.ports.sub
        (decodePortMsg
            >> Maybe.map (dispatchSub confRec stateRec)
            >> Maybe.withDefault confRec.noOpMsg
        )


dispatchSub : ConfigRec file msg -> StateRec -> PortSubMsg -> msg
dispatchSub confRec stateRec portsMsg =
    case portsMsg.message of
        "get-bounding-client-rects" ->
            subBoundedRects confRec stateRec portsMsg

        _ ->
            confRec.noOpMsg


type alias BoundedRecSubResponse =
    { inputId : String
    , rect : Rectangle
    }


subBoundedRects : ConfigRec file msg -> StateRec -> PortSubMsg -> msg
subBoundedRects confRec stateRec portsMsg =
    case portsMsg.data of
        Ok data ->
            data
                |> Decode.decodeValue
                    (Decode.list
                        (Decode.map2 BoundedRecSubResponse
                            (Decode.field "inputId" Decode.string)
                            (Decode.field "rect" rectDecoder)
                        )
                    )
                |> Result.andThen
                    (\resultList ->
                        let
                            current =
                                resultList
                                    |> List.foldl (\{ inputId, rect } acc -> Dict.insert inputId rect acc) stateRec.current
                        in
                        Ok (confRec.setStateMsg (State { stateRec | current = current }))
                    )
                |> Result.withDefault confRec.noOpMsg

        Err err ->
            confRec.noOpMsg
