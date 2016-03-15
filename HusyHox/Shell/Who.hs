module HusyHox.Shell.Who (main, who) where

import Data.List (intercalate)
import Text.Printf (printf)

import HusyHox.Common.PseudoTerminal (PtsInfo(..), getPtsInfos)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness who

data WhoSwitch = Lookup | Message | Writeable
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [PtsInfo]

data CoreArgs = CA [PtsInfo]

data FormatArgs = FA [PtsInfo]

data OutputArgs = OA String

who :: Utility WhoSwitch IOArgs CoreArgs FormatArgs OutputArgs
who = Utility "who" argDescs input parse core format output where
    input _ = fmap IA getPtsInfos

    parse _ (IA ptsInfos) = CA ptsInfos

    core (CA ptsInfos) = FA ptsInfos

    format (FA ptsInfos) = (OA . intercalate "\n" . map showPtsInfo) ptsInfos

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch WhoSwitch)]
argDescs =
    [ mkNamed Al "all" "same as -b -d --login -p -r -t -T -u"
    , mkNamed Bl "boot" "time of last system boot"
    , mkNamed Dl "dead" "print dead processes"
    , mkNamed H  "heading" "print line of column headings"
    , mkNamed Ll "login" "print system login processes"
    , mkExt   Lookup "lookup"
          ("attempt to canonicalize hostnames via DNS\n" ++
           "only hostname and user associated with stdin")
    , mkAbbrOnly Ml "only hostname and user associated with stdin"
    , mkNamed Pl "process" "print active processes spwaned by init"
    , mkNamed Ql "count" "all login names and number of users logged on"
    , mkNamed Rl "runlevel" "print current runlevel"
    , mkNamed Sl "short" "print only name, line, and time (default)"
    , mkNamed Tl "time" "print last system clock change"
    , mkNamed T  "mesg" "add user's message status as +, - or ?"
    , mkNamed Ul "users" "list users logged in"
    , mkExt Message   "message"   "same as -T"
    , mkExt Writeable "writeable" "same as -T"
    , mkHelp
    , mkVersion
    ]

-- Format Helpers -------------------------------------------------------------

showPtsInfo :: PtsInfo -> String
showPtsInfo info = printf "%s\t%s\t%s"
    (ownerName info) (ptsName info) (show $ creationTime info)
