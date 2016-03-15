module HusyHox.Shell.Pinky (main, pinky) where

import Data.List (intercalate)

import HusyHox.Common.PseudoTerminal (PtsInfo(..), getPtsInfos)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness pinky

data PinkySwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [PtsInfo]

data CoreArgs =
    CA Bool Bool Bool Bool Bool Bool Bool Bool Bool [String] [PtsInfo]

data FormatArgs = FA [PtsInfo]

data OutputArgs = OA String

pinky :: Utility PinkySwitch IOArgs CoreArgs FormatArgs OutputArgs
pinky = Utility "pinky" argDescs input parse core format output where
    input _ =  fmap IA getPtsInfos

    parse args (IA ptsInfos) = CA l b h p s f w i q (argsRest args) ptsInfos
        where [l, b, h, p, s, f, w, i, q] = map (gotStdArg args)
                  [Ll, Bl, Hl, Pl, Sl, Fl, Wl, Il, Ql]

    core (CA _ _ _ _ _ _ _ _ _ _ ptsInfos) = FA ptsInfos

    format (FA ptsInfos) = OA $ showPtsInfos ptsInfos

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch PinkySwitch)]
argDescs =
    [ mkAbbrOnly Ll "produce long format output for the specified USERs"
    , mkAbbrOnly Bl "omit the user's home directory and shell in long format"
    , mkAbbrOnly Hl "omit the user's project file in long format"
    , mkAbbrOnly Pl "omit the user's plan file in long format"
    , mkAbbrOnly Sl "do short format output, this is the default"
    , mkAbbrOnly Fl "omit the line of column heading in short format"
    , mkAbbrOnly Wl "omit the user's full name in short format"
    , mkAbbrOnly Il "omit the user's full name and remote host in short format"
    , mkAbbrOnly Ql "omit the user's full name, remote host and idle time"
    , mkHelp
    , mkVersion
    ]

-- Format Helpers -------------------------------------------------------------

showPtsInfos :: [PtsInfo] -> String
showPtsInfos = intercalate "\n" . map (intercalate "\t") . ptsInfoTable

ptsInfoTable :: [PtsInfo] -> [[String]]
ptsInfoTable ptsInfos = headers : map ptsInfoRow ptsInfos
    where headers = ["Login", "Name", "TTY", "Idle", "When", "Where"]


ptsInfoRow :: PtsInfo -> [String]
ptsInfoRow info =
    [ ownerName info
    , ownerName info
    , ptsName info
    , show $ creationTime info
    ]
