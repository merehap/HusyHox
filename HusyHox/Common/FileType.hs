module HusyHox.Common.FileType (FileType(..), getFileType) where

import System.Directory (doesFileExist)
import System.Posix.Files (FileStatus, getFileStatus, isBlockDevice,
    isCharacterDevice, isDirectory, isRegularFile, isSymbolicLink, isNamedPipe, isSocket)

import HusyHox.Common.Extra (if', selectMatch)

data FileType
    = BlockSpecial | CharacterSpecial | Directory | Regular | SymbolicLink
    | NamedPipe | Socket | Unknown
    deriving (Eq)

getFileType :: FilePath -> IO (Maybe FileType)
getFileType path = do
    exists <- doesFileExist path
    if' exists
        (fmap (Just . fileStatusToFileType) $ getFileStatus path)
        (return Nothing)

fileStatusToFileType :: FileStatus -> FileType
fileStatusToFileType status = selectMatch Unknown ($ status)
        [ (isBlockDevice    , BlockSpecial    )
        , (isCharacterDevice, CharacterSpecial)
        , (isDirectory      , Directory       )
        , (isRegularFile    , Regular         )
        , (isSymbolicLink   , SymbolicLink    )
        , (isNamedPipe      , NamedPipe       )
        , (isSocket         , Socket          )
        ]
