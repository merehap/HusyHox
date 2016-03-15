module HusyHox.Common.Arg
    ( Arg, Args, argsRest, getArg, getRequiredArg, gotArg
    , Switch(..), StdSwitch(..)
    , gotStdArg, gotExtArg, getStdArg, getExtArg, getRequiredStdArg
    , mkUnnamed, mkNamed, mkNamedData, mkHelp, mkVersion, mkExt, mkExtData
    , mkAbbrOnly, mkAbbrOnlyData, mkRequiredUnnamed, mkRequiredNamedData
    )
    where

import Data.Char (chr)
import System.Console.ParseArgs hiding (args)

data StdSwitch =
     A | B | C | D | E | F | G | H | I | J | K | L | M |
     N | O | P | Q | R | S | T | U | V | W | X | Y | Z |
     Al | Bl | Cl | Dl | El | Fl | Gl | Hl | Il | Jl | Kl | Ll | Ml |
     Nl | Ol | Pl | Ql | Rl | Sl | Tl | Ul | Vl | Wl | Xl | Yl | Zl |
     Zero | One | Two | Three | Unnamed | Unnamed2 | Unnamed3 | Unnamed4 |
     Help | Version
     deriving (Bounded, Enum, Eq, Ord)

instance Show StdSwitch where
    show One      = "1"
    show Two      = "2"
    show Three    = "3"
    show Unnamed  = "unnamed"
    show Unnamed2 = "unnamed2"
    show Unnamed3 = "unnamed3"
    show Unnamed4 = "unnamed4"
    show Help     = "help"
    show Version  = "version"
    show s = [chr (index + offset)]
        where index = fromEnum s
              offset = if index < 26 then 65 else 71 

data (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Switch a = Std StdSwitch | Ext a
    deriving (Eq, Ord, Show)

getAbbr :: StdSwitch -> Char
getAbbr s =
    if null as
    then a
    else error ("No abbreviation available for " ++ show s)
    where (a:as) = show s

gotStdArg
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> StdSwitch -> Bool
gotStdArg args = gotArg args . Std

gotExtArg :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> a -> Bool
gotExtArg args = gotArg args . Ext

getStdArg
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> StdSwitch -> Maybe String
getStdArg args = getArg args . Std

getExtArg
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> a -> Maybe String
getExtArg args = getArg args . Ext

getRequiredStdArg
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> StdSwitch -> String
getRequiredStdArg args = getRequiredArg args . Std

mkUnnamed
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> Arg (Switch a)
mkUnnamed switch = mkOptionalSwitch (Std switch) Nothing Nothing . Just

mkRequiredUnnamed
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> Arg (Switch a)
mkRequiredUnnamed switch = mkRequiredSwitch (Std switch) Nothing Nothing . Just

mkRequiredNamedData
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> String -> Arg (Switch a)
mkRequiredNamedData s name dpn =
    mkRequiredSwitch (Std s) (Just $ getAbbr s) (Just name) (Just dpn)

mkNamed
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> Arg (Switch a)
mkNamed s name = mkStdAbbr s (Just name) Nothing

mkNamedData
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> String -> Arg (Switch a)
mkNamedData s name dpn = mkStdAbbr s (Just name) (Just dpn)

mkAbbrOnly
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> Arg (Switch a)
mkAbbrOnly s = mkStdAbbr s Nothing Nothing

mkAbbrOnlyData
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> Arg (Switch a)
mkAbbrOnlyData s = mkStdAbbr s Nothing . Just

mkHelp
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Arg (Switch a)
mkHelp = mkStdSpecial Help "help" "HELP"

mkVersion
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Arg (Switch a)
mkVersion = mkStdSpecial Version "version" "VERSION"

mkExt
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => a -> String -> String -> Arg (Switch a)
mkExt e n = mkOptionalSwitch (Ext e) Nothing (Just n) Nothing

mkExtData
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => a -> String -> String -> String -> Arg (Switch a)
mkExtData e n dpn = mkOptionalSwitch (Ext e) Nothing (Just n) (Just dpn)

mkStdAbbr
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> Maybe String -> Maybe String -> String -> Arg (Switch a)
mkStdAbbr s = mkOptionalSwitch (Std s) (Just $ getAbbr s)

mkStdSpecial
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => StdSwitch -> String -> String -> Arg (Switch a)
mkStdSpecial s n = mkOptionalSwitch (Std s) Nothing (Just n) Nothing

mkRequiredSwitch
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Switch a
    -> Maybe Char
    -> Maybe String
    -> Maybe String
    -> String
    -> Arg (Switch a)
mkRequiredSwitch = mkSwitch argDataRequired

mkOptionalSwitch
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Switch a
    -> Maybe Char
    -> Maybe String
    -> Maybe String
    -> String
    -> Arg (Switch a)
mkOptionalSwitch = mkSwitch argDataOptional

mkSwitch
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => (String -> (Maybe String  -> Argtype) -> Maybe DataArg)
    -> Switch a
    -> Maybe Char
    -> Maybe String
    -> Maybe String
    -> String
    -> Arg (Switch a)
mkSwitch argType switch char name dataPrintName desc = Arg
    { argIndex = switch
    , argAbbr = char
    , argName = name
    , argData = dataPrintName >>= flip argType ArgtypeString
    , argDesc = desc
    }
