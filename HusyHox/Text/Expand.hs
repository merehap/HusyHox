module HusyHox.Text.Expand (main, expand) where

import Data.List.Split (unintercalate)

import HusyHox.Common.Extra (if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness expand

data ExpandSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA Bool [Int] [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

expand :: Utility ExpandSwitch IOArgs CoreArgs FormatArgs OutputArgs
expand = Utility "expand" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA texts) = if' (all (>= 0) stops)
                     (CA ignoreAfterNonBlanks stops . map lines)
                     (error "Tab sizes must be ascending")
                     texts
        where ignoreAfterNonBlanks = gotStdArg args Il
              stops = zipWith (-) (tail tabs) tabs
              tabs  = maybe (repeat 8) getTabs (getStdArg args Tl)

    core (CA _ stops lns) = FA $ map (map (formatLine stops '\t')) lns

    format (FA lns) = OA $ concatMap unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch ExpandSwitch)]
argDescs =
    [ mkNamed Il "initial" "do not convert tabs after non blanks"
    , mkNamedData       Tl "tabs" "NUMBER"
          ("have tabs NUMBER characters apart, not 8 or " ++
           "use comma-separated list of explicit tab positions")
    , mkHelp
    , mkVersion
    ]

-- Parse Helpers --------------------------------------------------------------

getTabs :: String -> [Int]
getTabs text = case (map read . unintercalate ",") text of
    []  -> error "No tabs specified."
    [x] -> repeat x
    xs  -> xs

-- Core Helpers ---------------------------------------------------------------

formatLine :: [Int] -> Char -> String -> String
formatLine _     _ [] = []
formatLine []    _ xs = xs
formatLine stops x xs =
    concat [front, replicate count ' ', formatLine nss x (drop 1 back)]
    where (front, back) = span (/= x) xs
          position  = length front
          count     = ns - position
          (ns, nss) = findStop position stops

findStop :: Int -> [Int] -> (Int, [Int])
findStop _ []     = (0, [])
findStop n (s:ss)
    | n < s     = (s, ss)
    | otherwise = (s + ns, nss)
          where (ns, nss) = findStop (n - s) ss
