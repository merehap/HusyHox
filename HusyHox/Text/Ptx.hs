module HusyHox.Text.Ptx (main, ptx) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness ptx

data PtxSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

ptx :: Utility PtxSwitch IOArgs CoreArgs FormatArgs OutputArgs
ptx = Utility "ptx" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) =  CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch PtxSwitch)]
argDescs =
    [ mkNamed     A  "auto-reference"
          "output automatically generated references"
    , mkNamed     G  "traditional"     "behave more like System V `ptx'"
    , mkNamedData F  "flag-truncation" "STRING"
          "use STRING for flagging line truncations"
    , mkNamedData M  "macro-name" "STRING" "macro name to use instead of `xx'"
    , mkNamedData O  "format" "roff" "generate output as roff directives"
    , mkNamed     R  "right-side-refs"
          "put references at right, not counted in -w"
    , mkNamedData S  "sentence-regexp" "REGEXP"
          "use REGEXP to match each keyword"
    , mkNamedData T  "format" "tex" "generated output as TeX directives"
    , mkNamedData W  "word-regexp" "REGEXP" "use REGEXP to match each keyword"
    , mkNamedData Bl "break-file" "FILE" "word break characters in this FILE"
    , mkNamed     Fl "ignore-case" "fold lower case to upper case for sorting"
    , mkNamedData Gl "gap-size" "NUMBER"
          "gap size in columns between output fields"
    , mkNamedData Il "ignore-file" "FILE" "read ignore word list from FILE"
    , mkNamedData Ol "only-file" "FILE" "read only word list from this FILE"
    , mkNamed     Rl "references" "first field of each line is a reference"
    , mkNamed     Tl "typeset-mode" "  - not implemented -  "
    , mkNamedData Wl "width" "NUMBER"
          "output width in columns, reference excluded"
    , mkHelp
    , mkVersion
    ]
