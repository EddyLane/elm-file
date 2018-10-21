module File.Gallery exposing
    ( Config
    , UploadState
    , config
    , configCancelUploadMsg
    , configContentTypeFn
    , configIdFn
    , configNameFn
    , configThumbnailSrcFn
    , view
    )

import Css exposing (..)
import Css.Media
import Css.Transitions exposing (easeInOut, transition)
import FeatherIcons
import File.Data.Base64Encoded as Base64Encoded
import File.Data.UploadId as UploadId exposing (UploadId)
import File.Upload as Upload exposing (UploadingFile)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, src)
import Html.Styled.Events exposing (onClick)


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
        }


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


view : Maybe (Html msg) -> List file -> Upload.State -> Config file msg -> Html msg
view maybeDropZone uploaded uploading (Config configRec) =
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

        galleryItems =
            viewItems uploaded uploading configRec
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


combineUploadedAndUploading : List file -> Upload.State -> List (UploadState file)
combineUploadedAndUploading files uploadState =
    List.concat
        [ List.map Uploaded files
        , uploadState
            |> Upload.uploads
            |> UploadId.toList
            |> List.map (\( id, uploading ) -> Uploading id uploading)
        ]


viewGalleryItemContainer : List (Html msg) -> Html msg
viewGalleryItemContainer inner =
    div [ css [ viewGalleryItemContainerStyle ] ] inner


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


viewItem : ConfigRec file msg -> UploadState file -> Html msg
viewItem configRec upload =
    viewGalleryItemContainer
        [ viewItemThumbnail configRec upload
        , viewItemUploadOverlay configRec upload
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


isImage : ConfigRec file msg -> UploadState file -> Bool
isImage configRec uploadState =
    case uploadState of
        Uploaded file ->
            String.startsWith "image" (configRec.contentTypeFn file)

        Uploading _ file ->
            Upload.fileIsImage file
