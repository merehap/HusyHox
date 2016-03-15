module HusyHox.Common.FilePermissions (FilePermissions, getFilePermissions,
    showOctal, showText) where

import System.Directory (Permissions(..), getPermissions)
import qualified System.Posix.Files as F (getFileStatus, isDirectory)

import HusyHox.Common.Extra (if')

data FilePermissions = FilePermissions
    { isDirectory :: Bool
    , rwxs        :: Permissions -- Read/Write/Execute/Search
    }

getFilePermissions :: FilePath -> IO FilePermissions
getFilePermissions path = do
    fileStatus <- F.getFileStatus path
    let isDir = F.isDirectory fileStatus
    rwxsPerms <- getPermissions path
    return $ FilePermissions isDir rwxsPerms

showOctal :: FilePermissions -> String
showOctal _ = "0000"

showText :: FilePermissions -> String
showText fp = map (\(f,c) -> if' (f fp) c '-')
     [ (isDirectory      , 'd')
     , (readable . rwxs  , 'r')
     , (writable . rwxs  , 'w')
     , (executable . rwxs, 'x')
     ]
