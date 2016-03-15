module HusyHox.Shell.Expr (main, expr) where

import Data.Int (Int64)
import Data.List (findIndices)
import Data.Maybe (fromMaybe, listToMaybe)

import HusyHox.Common.Extra (if', syntaxError)
import HusyHox.Common.Utility hiding (StdSwitch(..))

main :: IO ()
main = runUtilityHarness expr

data ExprSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data Expression
    = Identity String
    | Match String String
    | Substr String Int Int
    | Index String String
    | Length String
    | StringOP String (String -> String -> String) String
    | CompareOP Int64 (Int64 -> Int64 -> Bool) Int64
    | Int64OP Int64 (Int64 -> Int64 -> Int64) Int64

data CoreArgs = CA Expression

data Result = String String | Bool Bool | Int64 Int64

instance  Show Result where
    show (String s) = s
    show (Bool b)   = if' b "1" "0"
    show (Int64 i)  = show i

data FormatArgs = FA Result

data OutputArgs = OA String

expr :: Utility ExprSwitch IOArgs CoreArgs FormatArgs OutputArgs
expr = Utility "expr" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = (CA . tokenize . argsRest) args

    core (CA expression) = FA $ evaluate expression

    format (FA result) = OA $ show result

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch ExprSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]

-- Parse Helpers --------------------------------------------------------------

data OP
    = S (String -> String -> String)
    | C (Int64 -> Int64 -> Bool)
    | I (Int64 -> Int64 -> Int64)

-- TODO: figure out a clean way to not repeat prefix ops for syntaxError case
tokenize :: [String] -> Expression
tokenize [] = error "missing operand"
tokenize ["match", text, regex]     = Match text regex
tokenize ["substr", text, pos, len] = Substr text (read pos) (read len)
tokenize ["index", text, chars]     = Index text chars
tokenize ["length", text]           = Length text
tokenize (x:_) | x `elem` ["match", "substr", "index", "length"] = syntaxError
tokenize [x]           = Identity x
tokenize ["+", x]      = Identity x
tokenize ["(", x, ")"] = Identity x
tokenize [x, opText, y] = case opType of
    S op -> StringOP x op y
    C op -> CompareOP (read x) op (read y)
    I op -> Int64OP (read x) op (read y)
    where opType = case opText of
              "|" ->  S (\a b -> if' (a /= "0") a b)
              "&" ->  S (\a b -> if' (a /= "0" && b /= "0") a "0")
              "<"  -> C (<)
              "<=" -> C (<=)
              "="  -> C (==)
              "!=" -> C (/=)
              ">=" -> C (>=)
              ">"  -> C (>)
              "+"  -> I (+)
              "-"  -> I (-)
              "*"  -> I (*)
              "/"  -> I div
              "%"  -> I mod
              _    -> syntaxError
tokenize _ = syntaxError

-- Core Helpers ---------------------------------------------------------------

evaluate :: Expression -> Result
evaluate expression = case expression of
    (Identity x)          -> String x
    (Match text regex)    -> Int64 $
        if' (and $ zipWith (==) text regex) (fromIntegral $ length regex) 0
    (Substr text pos len) -> (String . take len . drop (pos-1)) text
    (Index text chars)    -> (Int64 . fromIntegral .
        fromMaybe 0 . listToMaybe . findIndices (`elem` chars)) text
    (Length text)         -> (Int64 . fromIntegral . length) text
    (StringOP s1 op s2)   -> String (s1 `op` s2)
    (CompareOP i1 op i2)  -> Bool   (i1 `op` i2)
    (Int64OP  i1 op i2)   -> Int64  (i1 `op` i2)
