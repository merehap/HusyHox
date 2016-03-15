module HusyHox.Common.Environment (formatEnvVar) where

formatEnvVar :: String -> String -> String
formatEnvVar n v =  n ++ "=" ++ v
