module HusyHox.Shell.Test (main, test) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import HusyHox.Common.Extra (if', selectMatch, syntaxError)
import HusyHox.Common.FileType (FileType(..), getFileType)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness test

data TestSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA (Maybe FileType)

data Expression
    = StringTest String        (String -> Bool)
    | StringComp String String (String -> String -> Bool)
    | IntComp    Int    Int    (Int    -> Int    -> Bool)

data CoreArgs = CA (Either (FileType, FileType) Expression)

data FormatArgs = FA Bool

data OutputArgs = OA Int

test :: Utility TestSwitch IOArgs CoreArgs FormatArgs OutputArgs
test = Utility "test" argDescs input parse core format output where
    input args = do
        let path = fromMaybe "." $ getStdArg args Unnamed
        fileType <- getFileType path
        return $ IA fileType

    parse args (IA maybeFileType) = CA $ case maybeFileType of
        Nothing -> Right (parseExpression $ argsRest args)
        Just fileType -> Left (fileType, targetFileType)
        where targetFileType = getTargetFileType args

    core (CA (Left (fileType, targetFileType))) =
        FA (fileType == targetFileType)
    core (CA (Right expression)) = FA (evaluateExpression expression)

    format (FA result) = OA (if' result 0 1)

    -- TODO: actually return this code, rather than print it
    output (OA returnCode) = print returnCode

argDescs :: [Arg (Switch TestSwitch)]
argDescs =
    [ mkAbbrOnlyData Bl "FILE" "FILE exists and is block special"
    , mkAbbrOnlyData Cl "FILE" "FILE exists and is character special"
    , mkAbbrOnlyData Dl "FILE" "FILE exists and is a directory"
    , mkAbbrOnlyData El "FILE" "FILE exists"
    , mkAbbrOnlyData Fl "FILE" "File exists and is a regular file"
    , mkAbbrOnlyData Gl "FILE" "FILE exists and is set-group-ID"
    , mkAbbrOnlyData G  "FILE"
          "FILE exists and is owned by the effective group ID"
    , mkAbbrOnlyData Hl "FILE"
          "FILE exists and is a symbolic link (same as -L)"
    , mkAbbrOnlyData Kl "FILE" "FILE exists and has its sticky bit set"
    , mkAbbrOnlyData L  "FILE"
          "FILE exists and is a symbolic link (same as -h)"
    , mkAbbrOnlyData O  "FILE"
          "FILE exists and is owned by the effective user ID"
    , mkAbbrOnlyData Pl "FILE" "FILE exists and is a named pipe"
    , mkAbbrOnlyData Rl "FILE" "FILE exists and read permission is granted"
    , mkAbbrOnlyData Sl "FILE" "FILE exists and has a size greater than zero"
    , mkAbbrOnlyData S  "FILE" "FILE exists and is a socket"
    , mkAbbrOnlyData Tl "FD"   "file descriptor FD is opened on a terminal"
    , mkAbbrOnlyData Ul "FILE" "FILE exists and its set-user-ID bit is set"
    , mkAbbrOnlyData Wl "FILE" "FILE exists and write permission is granted"
    , mkAbbrOnlyData Xl "FILE"
          "FILE exists and execute (or search permission is granted"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE" "FILE"
    ]

-- IO Helpers -----------------------------------------------------------------

getTargetFileType :: Args (Switch TestSwitch) -> FileType
getTargetFileType args = selectMatch Unknown (gotArg args . Std)
    [ (Bl, BlockSpecial)
    , (Cl, CharacterSpecial)
    , (Dl, Directory)
    , (Fl, Regular)
    , (Hl, SymbolicLink)
    , (L , SymbolicLink)
    , (Pl, NamedPipe)
    , (S , Socket)
    ]

-- Parse Helpers --------------------------------------------------------------

parseExpression :: [String] -> Expression
parseExpression [] = StringTest "" (not . null)
parseExpression [x] = StringTest x (not . null)
parseExpression [['-', op], x] = StringTest x $ case op of
    'n' -> not . null
    'z' -> null
    _ -> syntaxError
parseExpression [x, op, y]
    | all isDigit x && all isDigit y = IntComp (read x) (read y) $ case op of
          "-eq" -> (==)
          "-ge" -> (>=)
          "-gt" -> (>)
          "-le" -> (<=)
          "-lt" -> (<)
          "-ne" -> (/=)
          _ -> syntaxError
    | otherwise = StringComp x y $ case op of
          "="  -> (==)
          "!=" -> (/=)
          _ -> syntaxError
parseExpression _ = syntaxError

-- Core Helpers ---------------------------------------------------------------

evaluateExpression :: Expression -> Bool
evaluateExpression (StringTest x t) = t x
evaluateExpression (StringComp x y t) = t x y
evaluateExpression (IntComp x y t) = t x y
