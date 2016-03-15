module HusyHox.Shell.Groups (main, groups) where

import Data.List (sort)
import Data.Maybe (fromMaybe, isNothing)
import System.Posix.User (getAllGroupEntries, getEffectiveUserName
       , getGroupEntryForID, getUserEntryForName, GroupEntry, groupMembers
       , groupName, userGroupID)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness groups

data GroupsSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String GroupEntry [GroupEntry]

data CoreArgs = CA Bool String GroupEntry [GroupEntry]

data FormatArgs = FA [String]

data OutputArgs = OA String

groups :: Utility GroupsSwitch IOArgs CoreArgs FormatArgs OutputArgs
groups = Utility "groups" argDescs input parse core format output where
    input _ = do
        currentUser  <- getEffectiveUserName
        userEntry <- getUserEntryForName currentUser
        userGroupEntry <- getGroupEntryForID (userGroupID userEntry)
        groupEntries <- getAllGroupEntries
        return $ IA currentUser userGroupEntry groupEntries

    parse args (IA currentUser userGroupEntry groupEntries) =
        CA shouldSort user userGroupEntry groupEntries
        where maybeUser  = getStdArg args Unnamed
              shouldSort = isNothing maybeUser
              user       = fromMaybe currentUser maybeUser

    core (CA shouldSort user userGroupEntry entries) = FA $
        groupName userGroupEntry :
            (if' shouldSort sort id . map groupName) userEntries
        where userEntries = filter (elem user . groupMembers) entries

    format (FA xs) = OA (unwords xs ++ "\n")

    output (OA text) = putStr text

argDescs :: [Arg (Switch GroupsSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkUnnamed Unnamed "USER" "USER"
    ]
