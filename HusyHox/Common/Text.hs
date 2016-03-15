module HusyHox.Common.Text (lineApply, getCountAndSign) where

import System.Console.ParseArgs (Args, getArg)

import HusyHox.Common.Arg (Switch)
import HusyHox.Common.Extra (if', parseIntegral)

lineApply
    :: Bool
    -> (Int -> [String] -> [String])
    -> (Int -> [String] -> [String])
    -> Int
    -> [[String]]
    -> [[String]]
lineApply b f1 f2 = map . if' b f1 f2

getCountAndSign
    :: (Bounded s, Enum s, Eq s, Ord s, Show s)
    => Int             -- Default count
    -> Bool            -- Default sign
    -> Switch s        -- Arg containing
    -> Args (Switch s) -- Collection of parse args
    -> (Int, Bool)     -- Actual count and sign
getCountAndSign c b a args = maybe (c, b) parseIntegral (getArg args a)
