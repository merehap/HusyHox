module HusyHox.File.Chcon (main, chcon) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness chcon

data ChconSwitch = Reference deriving (Bounded, Enum, Eq, Ord, Show)

chcon :: Utility ChconSwitch () () () ()
chcon = Utility "chcon" argDescs noInput noParse noCore noFormat noOutput

argDescs :: [Arg (Switch ChconSwitch)]
argDescs =
    [ mkNamed Hl "no-dereference"
          "affect symbolic links instead of any referenced file"
    , mkExtData Reference "reference" "RFILE"
          ("use RFILE's security context rather than specifying\n" ++
           "a CONTEXT value")
    , mkNamed R  "recursive" "operate on files and directories recursively"
    , mkNamed Vl "verbose" "output a diagnostic for every file processed"
    , mkNamedData Ul "user" "USER"
          "set user USER in the target security context"
    , mkNamedData Rl "role" "ROLE"
          "set role ROLE in the target security context"
    , mkNamedData Tl "type" "TYPE"
          "set type TYPE in the target security context"
    , mkNamedData Ll "range" "RANGE"
          "set range RANGE in the target security context"
    , mkAbbrOnly H
          ("if a command line argument is a symbolic link\n" ++
           "to a directory, traverse it")
    , mkAbbrOnly L "traverse every symbolic link to a directory encountered"
    , mkAbbrOnly P "do not traverse any symbolic links (default)"
    , mkHelp
    , mkVersion
    ]
