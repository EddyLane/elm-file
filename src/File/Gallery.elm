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
import FeatherIcons
import File.Data.UploadId exposing (UploadId)
import File.Upload exposing (UploadingFile)
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
    , thumbnailSrcFn : file -> String
    , cancelUploadMsg : UploadId -> msg
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


view : List file -> Config file msg -> Html msg
view uploaded (Config configRec) =
    div
        [ css
            [ maxWidth (px 960)
            , margin2 (px 0) auto
            , textAlign center
            , paddingBottom (px 360)
            ]
        ]
        (List.map (viewItem configRec) uploaded)


viewItem : ConfigRec file msg -> file -> Html msg
viewItem configRec file =
    div
        [ css
            [ float left
            , position relative
            , width (calc (pct 15) minus (px 2))
            , paddingBottom (pct 15)
            , margin (pct 0.83)
            , overflow hidden
            , hover [ boxShadow5 (px 5) (px 5) (px 5) (px 0) (rgba 0 0 0 0.25) ]
            , borderRadius4 (pct 5) (pct 5) (pct 5) (pct 5)
            , border3 (px 1) solid (hex "000000")
            , overflow hidden
            ]
        ]
        [ img
            [ src (configRec.thumbnailSrcFn file)
            , css
                [ position absolute
                , left (px 0)
                , top (px 0)
                , width (pct 100)
                , height (pct 100)
                , property "object-fit" "cover"
                ]
            ]
            []
        ]
