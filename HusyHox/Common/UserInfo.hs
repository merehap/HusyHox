module HusyHox.Common.UserInfo (UserInfo(..), getUserInfoFromID,
    getUserInfoFromName) where

import System.Posix.Types (GroupID, UserID)
import System.Posix.User (UserEntry, getGroupEntryForID, getUserEntryForID,
    getUserEntryForName, userGroupID)
import qualified System.Posix.User as U (groupName, userID, userName)

data UserInfo = UserInfo
    { userID    :: UserID
    , userName  :: String
    , groupID   :: GroupID
    , groupName :: String
    }

getUserInfoFromID :: UserID -> IO UserInfo
getUserInfoFromID = (getUserInfo =<<) . getUserEntryForID

getUserInfoFromName :: String -> IO UserInfo
getUserInfoFromName = (getUserInfo =<<) . getUserEntryForName 

getUserInfo :: UserEntry -> IO UserInfo
getUserInfo userEntry = do
    let uid = U.userID userEntry
        uname = U.userName userEntry
        gid = userGroupID userEntry
    gname <- fmap U.groupName (getGroupEntryForID gid)
    return $ UserInfo uid uname gid gname
