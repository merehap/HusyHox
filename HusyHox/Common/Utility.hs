module HusyHox.Common.Utility
    ( Utility(..), UtilityHarness, UtilityHook, runUtilityHarness, runUtility
    , Arg, Args, StdSwitch(..), Switch(..), GeneralUtility(..)
    , getArg, getRequiredArg, gotArg, argsRest
    , gotStdArg, gotExtArg, getStdArg, getExtArg, getRequiredStdArg
    , mkHelp, mkVersion, mkUnnamed, mkNamed, mkExt, mkExtData, mkAbbrOnlyData
    , mkNamedData, mkAbbrOnly, mkRequiredUnnamed, mkRequiredNamedData
    , noInput, noParse, noCore, noFormat, noOutput
    ) where

import Data.List (intercalate)
import System.Console.ParseArgs hiding (args)
import System.Environment (getArgs, getProgName)

import HusyHox.Common.Arg

version :: [Int]
version = [0, 0]

-- s: Switch
-- i: IOArgs
-- c: CoreArgs
-- f: FormatArgs
-- o: OutputArgs
data (Eq s, Ord s, Show s) => Utility s i c f o = Utility
    { uName     :: String
    , uArgDescs :: [Arg (Switch s)]
    , uInput    :: Args (Switch s)      -> IO i
    , uParse    :: Args (Switch s) -> i -> c
    , uCore     :: c                    -> f
    , uFormat   :: f                    -> o
    , uOutput   :: o                    -> IO ()
    }

type UtilityHarness = String -> [String] -> IO Int

type UtilityHook a = Args a -> IO Int

data GeneralUtility = forall s i c f o.
    (Bounded s, Enum s, Eq s, Ord s, Show s)
    => GU (Utility s i c f o)

runUtilityHarness
    :: (Bounded s, Enum s, Eq s, Ord s, Show s)
    => Utility s i c f o  -> IO ()
runUtilityHarness util = do
    progName <- getProgName
    args     <- getArgs
    _        <- runUtility (GU util) progName args
    return ()

runUtility :: GeneralUtility -> String -> [String] -> IO Int
runUtility (GU utility) progName rawArgs =
    maybe (sequenceUtility args utility)
        (\m -> putStrLn m >> return 0) helpOrVersion
    where [hasHelp, hasVersion] = map (gotStdArg args) [Help, Version]
          helpOrVersion = case (hasHelp, hasVersion) of
              (False, False) -> Nothing
              (False, True ) -> Just versionText
              (_    , _    ) -> Just (argsUsage args)
          args = parseArgs (ArgsTrailing "INPUTS") (uArgDescs utility)
              progName rawArgs
          versionText = (("HusyHox " ++) . intercalate "." . map show) version

sequenceUtility :: (Eq s, Ord s, Show s) =>
    Args (Switch s) -> Utility s i c f o -> IO Int
sequenceUtility args utility = do
    ia <- uInput utility args
    let ca = uParse utility args ia
        fa = uCore utility ca
        oa = uFormat utility fa
    _ <- uOutput utility oa
    return 0

noInput :: a -> IO ()
noInput _ = return ()

noParse :: a -> b -> b
noParse = flip const

noCore :: a -> a
noCore = id

noFormat :: a -> a
noFormat = id

noOutput :: a -> IO ()
noOutput _ = return ()
