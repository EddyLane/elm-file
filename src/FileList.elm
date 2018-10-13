module FileList
    exposing
        ( Config
        , Sort(..)
        , SortDirection(..)
        , State
        , config
        , configCancelUploadMsg
        , configColumn
        , configContentTypeFn
        , configDisableRow
        , configFileIcon
        , configIdFn
        , configInitialSort
        , configInitialSortDirection
        , configListStateMsg
        , configMultiSelectEnabled
        , configNameFn
        , configRowActions
        , configRowAttrs
        , configThumbnailSrcFn
        , configUploadedRowAttrs
        , init
        , view
        )

{-| Provides a simple easy-to-use List that be used to display uploaded and currently uploading files

This module should work great for really simple uploaders, or for debugging purposes when you are working to hook up
your uploader. If you need complex functionality, than you might need to implement your own version of what is offerered
in this module.


# Config

@docs Config, config, configCancelUploadMsg, configColumn, configContentTypeFn, configDisableRow, configFileIcon
@docs configIdFn, configInitialSort, configInitialSortDirection, configListStateMsg, configMultiSelectEnabled
@docs configNameFn, configRowActions, configRowAttrs, configThumbnailSrcFn, configUploadedRowAttrs


# State

@docs State, Sort, SortDirection, init, view

-}

import Data.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import Data.UploadId as UploadId exposing (Collection, UploadId)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Upload as Upload exposing (UploadingFile)


type UploadState file
    = Uploading UploadId UploadingFile
    | Uploaded file


type TagConfig
    = Taggable (List String)
    | NotTaggable


{-| This is the structure of the column record, used to help a user define custom columns
-}
type alias Column id file msg =
    { id : id
    , label : String
    , html : ( file, Bool ) -> Html msg
    , sorter : Maybe (file -> file -> Order)
    }



---- STATE ----


{-| Opaque type holding the state of the FileList component
-}
type State id
    = State (StateRec id)


type alias StateRec id =
    { direction : SortDirection
    , sortColumn : Sort id
    , selectedIds : Set String
    }


{-| Should we sort the chosen sort column by ascending or descending
-}
type SortDirection
    = Asc
    | Desc


{-| We can either sort by the filename, which is built into this component, or a user-defined custom column, in which
case the sorting must be defined in the configuration
-}
type Sort id
    = SortByFilename
    | SortByCustom id


{-| Init the state of the FileList component
-}
init : Config colId file msg -> State colId
init (Config { defaultCustomSort, defaultSortDir }) =
    State <|
        { direction = defaultSortDir
        , sortColumn = Maybe.withDefault SortByFilename defaultCustomSort
        , selectedIds = Set.empty
        }



---- CONFIG ----


{-| Opaque type for a Config record
-}
type Config colId file msg
    = Config (ConfigRec colId file msg)


{-| Configuration information for describing the behaviour of the FileList
-}
type alias ConfigRec colId file msg =
    { idFn : file -> String
    , nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    , cancelUploadMsg : UploadId -> msg
    , columns : List (Column colId file msg)
    , setListStateMsg : State colId -> msg
    , defaultCustomSort : Maybe (Sort colId)
    , defaultSortDir : SortDirection
    , rowActions : file -> Maybe (Html msg)
    , uploadedRowAttrs : file -> List (Attribute msg)
    , failedRowAttrs : UploadingFile -> List (Attribute msg)
    , disabled : file -> Bool
    , fileIcon : String -> Html msg
    , multiSelectEnabled : Bool
    }


{-| Get a default FileList configuration that you can further customise by calling additional config functions.
The argument to this function should be a NoOp message, this is because the FileList component has lots of
functionality, much of which you may not require. Anything you don't need will just default to the NoOp message you
have specified.
-}
config : msg -> Config colId file msg
config noOpMsg =
    Config <|
        { idFn = always "-"
        , nameFn = always "-"
        , contentTypeFn = always "-"
        , cancelUploadMsg = always noOpMsg
        , thumbnailSrcFn = always ""
        , columns = []
        , setListStateMsg = always noOpMsg
        , defaultCustomSort = Nothing
        , defaultSortDir = Asc
        , rowActions = always Nothing
        , uploadedRowAttrs = always []
        , failedRowAttrs = always []
        , disabled = always False
        , fileIcon = always (text "")
        , multiSelectEnabled = False
        }


{-| Configure a msg that will update the state of this component. Often you will be forced to fill in the blanks when
you use this library, but sometimes we'll do the leg work for you.
-}
configListStateMsg : (State colId -> msg) -> Config colId file msg -> Config colId file msg
configListStateMsg msg (Config configRec) =
    Config <|
        { configRec | setListStateMsg = msg }


{-| Configure the initial sort column of this component
-}
configInitialSort : Sort colId -> Config colId file msg -> Config colId file msg
configInitialSort sort (Config configRec) =
    Config <|
        { configRec | defaultCustomSort = Just sort }


{-| Configure the initial sort direction of this component
-}
configInitialSortDirection : SortDirection -> Config colId file msg -> Config colId file msg
configInitialSortDirection sortDir (Config configRec) =
    Config <|
        { configRec | defaultSortDir = sortDir }


{-| Configure the file icon function, a function which takes the content type string (for example "application/pdf")
and returns some html to display in the "thumbnail" column, given that the file is NOT an image (in which case a
thumbnail of the image will be displayed.)
-}
configFileIcon : (String -> Html msg) -> Config colId file msg -> Config colId file msg
configFileIcon fileIcon (Config configRec) =
    Config <|
        { configRec | fileIcon = fileIcon }


{-| Configure the disabled functionality, this takes a function with the file and returns a bool depending on if the row
should currently be inactive. Unless you have a good use case, this shouldn't be needed
-}
configDisableRow : (file -> Bool) -> Config colId file msg -> Config colId file msg
configDisableRow disabled (Config configRec) =
    Config <|
        { configRec | disabled = disabled }


{-| Configure a list of attributes that will be added to a row of an uploaded file. Takes a function with the uploaded
file as a param
-}
configUploadedRowAttrs : (file -> List (Attribute msg)) -> Config colId file msg -> Config colId file msg
configUploadedRowAttrs uploadedRowAttrs (Config configRec) =
    Config <|
        { configRec | uploadedRowAttrs = uploadedRowAttrs }


{-| Configure a list of attributes that will be added to a row of an uploading file. Takes a function with the uploading
file as a param
-}
configRowAttrs : (UploadingFile -> List (Attribute msg)) -> Config colId file msg -> Config colId file msg
configRowAttrs failedRowAttrs (Config configRec) =
    Config <|
        { configRec | failedRowAttrs = failedRowAttrs }


{-| Configure the way in which we can retrieve a unique id (as a string) from a user defined uploaded file
-}
configIdFn : (file -> String) -> Config colId file msg -> Config colId file msg
configIdFn idFn (Config configRec) =
    Config <|
        { configRec | idFn = idFn }


{-| Configure a way in which we can retrieve the original file name from a user defined uploaded file type, this should
match the original filename (including extension).
-}
configNameFn : (file -> String) -> Config colId file msg -> Config colId file msg
configNameFn nameFn (Config configRec) =
    Config <|
        { configRec | nameFn = nameFn }


{-| Configure a way in which we can retrieve the content type for a user defined uploaded file, this should match the
original content type
-}
configContentTypeFn : (file -> String) -> Config colId file msg -> Config colId file msg
configContentTypeFn contentTypeFn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = contentTypeFn }


{-| Configure a way in which we can get a string for the src value of a file which we determine is an image from the
content type
-}
configThumbnailSrcFn : (file -> String) -> Config colId file msg -> Config colId file msg
configThumbnailSrcFn thumbnailSrcFn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = thumbnailSrcFn }


{-| Configure a way in which we can cancel an upload
-}
configCancelUploadMsg : (UploadId -> msg) -> Config colId file msg -> Config colId file msg
configCancelUploadMsg cancelUploadMsg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = cancelUploadMsg }


{-| Configure a way in which we can add a custom column to the file table
-}
configColumn : Column colId file msg -> Config colId file msg -> Config colId file msg
configColumn col (Config configRec) =
    Config <|
        { configRec | columns = List.append configRec.columns [ col ] }


{-| Configure a way in which we can enable/disable the multiple file select functionality
-}
configMultiSelectEnabled : Bool -> Config colId file msg -> Config colId file msg
configMultiSelectEnabled multiSelectEnabled (Config configRec) =
    Config <|
        { configRec | multiSelectEnabled = multiSelectEnabled }


{-| Configure a list of "row actions", unless you have a real use-case you can probably avoid this
-}
configRowActions : (file -> Maybe (Html msg)) -> Config colId file msg -> Config colId file msg
configRowActions rowActions (Config configRec) =
    Config <|
        { configRec | rowActions = rowActions }



---- VIEW ----


{-| View the FileList
-}
view : State colId -> Upload.State -> List file -> Config colId file msg -> Html msg
view ((State { direction, sortColumn, selectedIds }) as state) upload files ((Config { idFn }) as config) =
    table [ class "table table-sm mb-0" ]
        [ viewTableHeader state config files
        , tbody []
            (upload
                |> Upload.uploads
                |> combineUploadsWithFiles files
                |> sortResults config state
                |> List.concatMap (viewRow config state)
            )
        ]


allUploadedFilesAreSelected : (file -> String) -> Set String -> List file -> Bool
allUploadedFilesAreSelected idFn selectedIds files =
    (files
        |> List.map idFn
        |> List.foldl Set.insert Set.empty
        |> Set.intersect selectedIds
        |> Set.size
    )
        == List.length files


sortResults : Config colId file msg -> State colId -> List (UploadState file) -> List (UploadState file)
sortResults config (State { direction, sortColumn }) =
    case sortColumn of
        SortByFilename ->
            List.sortWith (sortByFilename config direction)

        SortByCustom headerName ->
            List.sortWith (sortByCustom headerName config direction)


sortByCustom : colId -> Config colId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByCustom colId ((Config { columns }) as config) sortDir a b =
    columns
        |> List.filter (\{ id } -> id == colId)
        |> List.head
        |> Maybe.andThen .sorter
        |> Maybe.map
            (\sorter ->
                case sortDirPair sortDir a b of
                    ( Uploaded a, Uploaded b ) ->
                        sorter a b

                    ( Uploading _ a, Uploaded b ) ->
                        Basics.compare 1 2

                    ( Uploaded a, Uploading _ b ) ->
                        Basics.compare 2 1

                    ( Uploading _ a, Uploading _ b ) ->
                        Basics.compare 1 1
            )
        |> Maybe.withDefault (sortByFilename config sortDir a b)


sortByFilename : Config colId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByFilename config sortDir a b =
    case sortDirPair sortDir a b of
        ( a, b ) ->
            Basics.compare (filename config a) (filename config b)


filename : Config colId file msg -> UploadState file -> String
filename (Config { nameFn }) file =
    case file of
        Uploaded file ->
            nameFn file

        Uploading _ file ->
            Upload.fileFilename file


sortDirPair : SortDirection -> a -> a -> ( a, a )
sortDirPair sortDir a b =
    if sortDir == Asc then
        ( a, b )
    else
        ( b, a )


viewTableHeader : State colId -> Config colId file msg -> List file -> Html msg
viewTableHeader ((State stateRec) as state) ((Config { columns, setListStateMsg, idFn, multiSelectEnabled }) as config) files =
    let
        allFilesSelected =
            if List.isEmpty files then
                False
            else
                allUploadedFilesAreSelected idFn stateRec.selectedIds files
    in
    thead [ class "text-left" ]
        [ tr []
            (List.concat
                [ [ if multiSelectEnabled then
                        th
                            [ scope "col" ]
                            [ input
                                [ type_ "checkbox"
                                , checked allFilesSelected
                                , onClick
                                    (setListStateMsg
                                        (if allFilesSelected then
                                            deSelectAllFiles state
                                         else
                                            selectAllFiles config files state
                                        )
                                    )
                                ]
                                []
                            ]
                    else
                        text ""
                  , th [ scope "col" ] []
                  , th
                        [ style [ ( "cursor", "pointer" ) ]
                        , onClick (setListStateMsg (viewListSorterState SortByFilename state))
                        , scope "col"
                        ]
                        [ text "File"
                        , if stateRec.sortColumn == SortByFilename then
                            viewActiveSortArrow stateRec.direction
                          else
                            text ""
                        ]
                  ]
                , viewUserDefinedThs state config
                , [ th [ scope "col" ] [ text "" ] ]
                ]
            )
        ]


selectAllFiles : Config colId file msg -> List file -> State colId -> State colId
selectAllFiles ((Config { idFn }) as config) files (State state) =
    State <|
        { state | selectedIds = List.foldl (idFn >> Set.insert) Set.empty files }


deSelectAllFiles : State colId -> State colId
deSelectAllFiles (State state) =
    State <|
        { state | selectedIds = Set.empty }


viewCustomSortArrow : colId -> State colId -> Html msg
viewCustomSortArrow colId (State { direction, sortColumn }) =
    case sortColumn of
        SortByCustom id ->
            if id == colId then
                viewActiveSortArrow direction
            else
                text ""

        SortByFilename ->
            text ""


viewActiveSortArrow : SortDirection -> Html msg
viewActiveSortArrow dir =
    if dir == Asc then
        text "↑"
    else
        text "↓"


viewUserDefinedThs : State colId -> Config colId file msg -> List (Html msg)
viewUserDefinedThs state (Config { columns, setListStateMsg }) =
    columns
        |> List.map
            (\{ label, id } ->
                th
                    [ style [ ( "cursor", "pointer" ) ]
                    , scope "col"
                    , onClick <|
                        setListStateMsg <|
                            viewListSorterState (SortByCustom id) state
                    ]
                    [ text label
                    , viewCustomSortArrow id state
                    ]
            )


viewListSorterState : Sort colId -> State colId -> State colId
viewListSorterState column (State state) =
    State <|
        { state
            | direction =
                if state.direction == Asc && state.sortColumn == column then
                    Desc
                else
                    Asc
            , sortColumn = column
        }


combineUploadsWithFiles : List file -> UploadId.Collection Upload.UploadingFile -> List (UploadState file)
combineUploadsWithFiles files uploads =
    List.concat
        [ List.map (uncurry Uploading) <| UploadId.toList uploads
        , List.map Uploaded files
        ]


viewRow : Config colId file msg -> State colId -> UploadState file -> List (Html msg)
viewRow ((Config { idFn, rowActions, columns, uploadedRowAttrs }) as config) state file =
    case file of
        Uploading uploadId uploadingFile ->
            [ if Upload.fileIsFailed uploadingFile then
                viewFailedRow config uploadId uploadingFile
              else
                viewUploadingRow config uploadId uploadingFile
            ]

        Uploaded file ->
            let
                actionsRow =
                    file
                        |> rowActions
                        |> Maybe.map
                            (\rowContent ->
                                tr []
                                    [ td [ colspan (List.length columns + 4) ] [ rowContent ]
                                    ]
                            )
                        |> Maybe.withDefault (text "")
            in
            [ viewUploadedRow config state file (uploadedRowAttrs file)
            , actionsRow
            ]


viewUploadingRow : Config colId file msg -> UploadId -> UploadingFile -> Html msg
viewUploadingRow (Config { cancelUploadMsg, columns, uploadedRowAttrs }) uploadId file =
    let
        percentString =
            file
                |> Upload.fileProgress
                |> round
                |> toString
    in
    tr
        []
        [ td [ class "align-center text-left" ] []
        , td [ class "align-center text-left" ] [ viewUploadingThumbnail file ]
        , td [ class "align-center text-left" ] [ text <| Upload.fileFilename file ]
        , td
            [ colspan (List.length columns), class "align-center text-left" ]
            [ div [ class "progress" ]
                [ div
                    [ class "progress-bar progress-bar-striped progress-bar-animated"
                    , style [ ( "width", percentString ++ "%" ) ]
                    ]
                    [ text (percentString ++ "%") ]
                ]
            ]
        , td [ class "align-center text-left" ] [ button [ onClick (cancelUploadMsg uploadId) ] [ text "Cancel" ] ]
        ]


viewFailedRow : Config colId file msg -> UploadId -> UploadingFile -> Html msg
viewFailedRow (Config { cancelUploadMsg, columns, failedRowAttrs }) uploadId file =
    tr
        (failedRowAttrs file)
        [ td [ class "align-center text-left" ] []
        , td [ class "align-center text-left" ] []
        , td [ class "align-center text-left" ] [ text <| Upload.fileFilename file ]
        , td [ class "align-center text-left", colspan (List.length columns) ] []
        , td [ class "align-center text-left" ] [ button [ onClick (cancelUploadMsg uploadId) ] [ text "Remove" ] ]
        ]


viewUploadingThumbnail : UploadingFile -> Html msg
viewUploadingThumbnail file =
    if Upload.fileIsImage file then
        img
            [ style thumbnailStyle
            , class "rounded"
            , src
                (file
                    |> Upload.fileData
                    |> Maybe.map Base64Encoded.toString
                    |> Maybe.withDefault ""
                )
            ]
            []
    else
        span [ class "fas fa-stroopwafel" ] []


viewUploadedRow : Config colId file msg -> State colId -> file -> List (Attribute msg) -> Html msg
viewUploadedRow ((Config configRec) as config) ((State { selectedIds }) as state) file attrs =
    tr attrs
        (List.concat
            [ [ if configRec.multiSelectEnabled then
                    td [ class "align-center text-left" ]
                        [ input
                            [ type_ "checkbox"
                            , checked (Set.member (configRec.idFn file) selectedIds)
                            , onClick (configRec.setListStateMsg <| toggleFile config file state)
                            ]
                            []
                        ]
                else
                    text ""
              , td [ class "align-center text-left" ] [ viewUploadedThumbnail config file ]
              , td [ class "align-center text-left" ] [ text (configRec.nameFn file) ]
              ]
            , viewUserDefinedTds file selectedIds config
            ]
        )


toggleFile : Config colId file msg -> file -> State colId -> State colId
toggleFile (Config { idFn }) file (State state) =
    let
        id =
            idFn file
    in
    State <|
        { state
            | selectedIds =
                if Set.member id state.selectedIds then
                    Set.remove id state.selectedIds
                else
                    Set.insert id state.selectedIds
        }


viewUserDefinedTds : file -> Set String -> Config colId file msg -> List (Html msg)
viewUserDefinedTds file selectedIds (Config { idFn, columns }) =
    List.map (viewUserDefinedTd file (Set.member (idFn file) selectedIds)) columns


viewUserDefinedTd : file -> Bool -> Column id file msg -> Html msg
viewUserDefinedTd file isSelected column =
    td [ class "align-center text-left" ]
        [ column.html ( file, isSelected ) ]


viewUploadedThumbnail : Config colId file msg -> file -> Html msg
viewUploadedThumbnail (Config { thumbnailSrcFn, contentTypeFn, fileIcon }) file =
    if String.startsWith "image" (contentTypeFn file) then
        img
            [ style thumbnailStyle
            , src (thumbnailSrcFn file)
            ]
            []
    else
        fileIcon <| contentTypeFn file


thumbnailStyle : List ( String, String )
thumbnailStyle =
    [ ( "width", "50px" )
    ]


viewIf : Bool -> Html msg -> Html msg
viewIf condition html =
    if condition then
        html
    else
        text ""
