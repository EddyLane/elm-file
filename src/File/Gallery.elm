module File.Gallery exposing (UploadState)

import File.Data.UploadId exposing (UploadId)
import File.Upload exposing (UploadingFile)


type UploadState file
    = Uploading UploadId UploadingFile
    | Uploaded file
