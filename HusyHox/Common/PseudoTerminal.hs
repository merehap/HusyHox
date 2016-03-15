module HusyHox.Common.PseudoTerminal (PtsInfo(..), getPtsInfos) where

import Data.Char (isDigit)
import System.Directory (getDirectoryContents)
import System.FilePath (combine, takeFileName)
import System.Posix.Files (fileOwner, getFileStatus, modificationTime)
import System.Posix.Types (EpochTime) 

import HusyHox.Common.UserInfo (getUserInfoFromID, userName)

ptsDirectory :: String
ptsDirectory = "/dev/pts/"

data PtsInfo = PtsInfo
    { ptsName      :: String
    , ptsPath      :: FilePath
    , ownerName    :: String
    , creationTime :: EpochTime
    }

getPtsInfos :: IO [PtsInfo]
getPtsInfos = do
    paths <- getPtsPaths
    infos <- mapM getPtsInfo paths
    let nonRootInfos = filter ((/= "root") . ownerName) infos
    return nonRootInfos

getPtsInfo :: FilePath -> IO PtsInfo
getPtsInfo path = do
    let name = nameFromPath path
    status <- getFileStatus path
    userInfo <- getUserInfoFromID $ fileOwner status
    let fileOwnerName = userName userInfo
        time = modificationTime status
    return $ PtsInfo name path fileOwnerName time

-- TODO : fix this to use device type rather than arbitrary text processing
-- TODO : replace this with parsing of /var/run/utmp
getPtsPaths :: IO [FilePath]
getPtsPaths = do
    possiblePtsIDs <- getDirectoryContents ptsDirectory
    let ptsIDs = filter (all isDigit) possiblePtsIDs
        ptsPaths = map (combine ptsDirectory) ptsIDs
    return ptsPaths

nameFromPath :: FilePath -> String
nameFromPath = combine "pts/" . takeFileName
