module HusyHox.Shell.ID (main, id) where

import Prelude hiding (id)
import qualified Prelude as P (id)

import Data.List (intercalate)
import System.Posix.Types (GroupID)
import System.Posix.User (getGroupEntryForID, getLoginName,  getGroups)
import qualified System.Posix.User as U (groupName)
import Text.Printf (printf)

import HusyHox.Common.UserInfo (UserInfo(..), getUserInfoFromName)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness id

data IDSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

type IOArgs = FormatArgs

type CoreArgs = FormatArgs

data FormatArgs = FA UserInfo [(GroupID, String)]

data OutputArgs = OA String

id :: Utility IDSwitch IOArgs CoreArgs FormatArgs OutputArgs
id = Utility "id" argDescs input parse core format output where
    input args = do
        uname <- maybe getLoginName return (getArg args $ Std Unnamed)
        userInfo <- getUserInfoFromName uname
        gids <- getGroups
        gentries <- mapM getGroupEntryForID gids
        let groups = zip gids (map U.groupName gentries)
        return $ FA userInfo groups

    parse _ = P.id

    core = P.id

    format (FA ui groups) = OA $ printf "uid=%s gid=%s groups=%s"
        (showPair (userID ui) (userName ui))
        (showPair (groupID ui) (groupName ui))
        groupsText
        where groupsText = intercalate "," $ map (uncurry showPair) groups
              showPair x y = printf "%s(%s)" (show x) y :: String

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch IDSwitch)]
argDescs =
    [ mkAbbrOnly Al "ignore, for compatibility with other versions"
    , mkNamed Z  "context"
          "print only the security context of the current user"
    , mkNamed Gl "group"  "print only the effective group ID"
    , mkNamed G  "groups" "print all group IDs"
    , mkNamed Nl "name"   "print a name instead of a number, for -ugG"
    , mkNamed Rl "real"
          "print the real ID instead of the effective ID, with -ugG"
    , mkNamed Ul "user" "print only the effective user ID"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "USERNAME" "USERNAME"
    ]
