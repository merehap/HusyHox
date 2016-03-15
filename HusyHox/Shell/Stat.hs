module HusyHox.Shell.Stat (main, stat) where

import qualified System.Posix.Files as F (accessTime, deviceID, fileID,
    fileOwner, fileSize, getFileStatus, isDirectory, isRegularFile, linkCount,
    modificationTime, statusChangeTime)
import System.Posix.Types (CDev, EpochTime)
import Text.Printf (printf)

import HusyHox.Common.FilePermissions (FilePermissions, getFilePermissions,
    showOctal, showText)
import HusyHox.Common.UserInfo (UserInfo(..), getUserInfoFromID)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness stat

data StatSwitch = Printf deriving (Bounded, Enum, Eq, Ord, Show)

data FileInfo = FileInfo
    { filePath    :: FilePath
    , fileSize    :: Int
    , blocks      :: Int
    , ioBlock     :: Int
    , fileType    :: String
    , device      :: CDev
    , inode       :: Int
    , links       :: Int
    , permissions :: FilePermissions
    , ownerInfo   :: UserInfo
    , accessTime  :: EpochTime
    , modifyTime  :: EpochTime
    , changeTime  :: EpochTime
    }

data IOArgs = IA [FileInfo]

data CoreArgs = CA [FileInfo]

data FormatArgs = FA [FileInfo]

data OutputArgs = OA String

stat :: Utility StatSwitch IOArgs CoreArgs FormatArgs OutputArgs
stat = Utility "stat" argDescs input parse core format output where
    input = fmap IA . mapM getFileInfo . argsRest

    parse _ (IA fileInfos) = CA fileInfos

    core (CA fileInfos) = FA fileInfos

    format (FA fileInfos) = (OA . unlines . map formatFileInfo) fileInfos

    output (OA text) = putStr text

argDescs :: [Arg (Switch StatSwitch)]
argDescs =
    [ mkNamed L  "dereference" "follow links"
    , mkNamed Fl "file-system"
          "display file system status instead of file status"
    , mkNamedData Cl "format" "FORMAT"
          ("use the specified FORMAT instead of the default;\n" ++
           "output a newline after each use of FORMAT")
    , mkExtData Printf "printf" "FORMAT"
          ("like --format, but interpret backslash escapes,\n" ++
           "  and do not output a mandatory trailing newline.\n" ++
           "  If you want a newline, include \n in FORMAT")
    , mkNamed Tl "terse" "print the information in terse form"
    , mkHelp
    , mkVersion
    ]

-- Input Helpers --------------------------------------------------------------

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
    fs <- F.getFileStatus path
    let fileSize' = fromIntegral $ F.fileSize fs
        fileType' = case (F.isDirectory fs, F.isRegularFile fs) of
            (True , False) -> "directory"
            (False, True ) -> "regular file"
            (_    , _    ) -> "irregular file"
        deviceID' = F.deviceID fs
        inode' = fromIntegral $ F.fileID fs
        links' = fromIntegral $ F.linkCount fs
        access' = F.accessTime fs
        modify' = F.modificationTime fs
        change' = F.statusChangeTime fs
    perms <- getFilePermissions path
    oinfo <- getUserInfoFromID (F.fileOwner fs)
    return $ FileInfo path fileSize' 0 0 fileType' deviceID' inode' links'
        perms oinfo access' modify' change'

-- Format Helpers -------------------------------------------------------------

formatFileInfo :: FileInfo -> String
formatFileInfo fi =  printf (unlines
    [ "  File: `%s'"
    , "  Size: %d\t\tBlocks: %d\tIO Block: %d\t %s"
    , "Device: %s\tInode: %d\t Links: %d"
    , "Access: (%s)\tUid: ( %s/\t%s)\tGid: ( %s/\t%s)"
    , "Access: %s"
    , "Modify: %s"
    , "Change: %s"
    ])
    (filePath fi)
    (fileSize fi) (blocks fi) (ioBlock fi) (fileType fi)
    (show $ device fi) (inode fi) (links fi)
    (formatPermissions $ permissions fi)
    (show $ userID oi) (userName oi) (show $ groupID oi) (groupName oi)
    (show $ accessTime fi)
    (show $ modifyTime fi)
    (show $ changeTime fi)
    where formatPermissions p = printf "%s/%s"
              (showOctal p) (showText p) :: String
          oi = ownerInfo fi
