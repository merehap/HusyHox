module HusyHox.Text.TR (main, tr) where

import Data.List (group)
import Data.List.Split (splitOneOf)

import HusyHox.Common.Extra (if', replace)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tr

type TRSwitch = StdSwitch

data IOArgs = IA String

data CoreArgs = CA Mode Bool String

data Mode = Delete String (Maybe String)
          | Squeeze String
          | Translate Bool Bool String String

data FormatArgs = FA String

data OutputArgs = OA String

tr :: Utility TRSwitch IOArgs CoreArgs FormatArgs OutputArgs
tr = Utility "tr" argDescs input parse core format output where
    input _ = fmap IA getContents

    parse args (IA text) = CA mode c text
        where set1 = getRequiredStdArg args Unnamed
              maybeSet2 = getStdArg args Unnamed2
              mode = getMode set1 c t d s maybeSet2
              [c, d, s, t] = map (gotStdArg args) [Cl, Dl, Sl, Tl]

    core (CA (Delete delChars maybeSqzChars) _ text) = FA $
        maybe id squeeze maybeSqzChars . concat $ splitOneOf delChars text
    core (CA (Squeeze sqzChars) _ text) = FA $ squeeze sqzChars text
    core (CA (Translate ss st source target) _ text) = FA $
        if' ss (squeeze target) id $ replace truncatedSet target text
        where truncatedSet = if' st (take (length target)) id source

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch TRSwitch)]
argDescs =
    [ mkNamed    Cl "complement" "use the complement of SET1"
    , mkAbbrOnly C               "same as -c"
    , mkNamed    Dl "delete"
          "delete characters in SET1, do not translate"
    , mkNamed    Sl "squeeze-repeats"
          ("replace each input sequence of a repeated character that is" ++
           "listed in SET1 with a single occurrence of that character")
    , mkNamed    Tl "truncate-set1"
          "first truncate SET1 to length of SET2"
    , mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed  "SET1" "SET1"
    , mkUnnamed         Unnamed2 "SET2" "SET2"
    ]

-- Parse Helpers --------------------------------------------------------------

getMode :: String -> Bool -> Bool -> Bool -> Bool -> Maybe String -> Mode
getMode set1 sc st = m
    where m True  True  Nothing     = error ds
          m True  False (Just _)    = error d
          m False True  (Just _)    = error s
          m False False Nothing     = error t
          m True  _     maybeSet2   = Delete          set1 maybeSet2
          m False True  Nothing     = Squeeze         set1
          m False False (Just set2) = Translate sc st set1 set2

          ds = "-d with -s must specify set1 and set2"
          d  = "-d without -s must only specify set1"
          s  = "-s alone only takes set1"
          t  = "set1 and set2 must both be specified for translation to occur"

-- Core Helpers ---------------------------------------------------------------

squeeze :: String -> String -> String
squeeze squeezeChars = concatMap squeezeIf . group
    where squeezeIf t = if' (head t `elem` squeezeChars) (take 1) id t
