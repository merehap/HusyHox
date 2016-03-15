module HusyHox.Common.File where

import HusyHox.Common.Extra (atLeastLength, if')

manyToOnePath :: [String] -> ([String], String)
manyToOnePath paths =
    if' (atLeastLength 2 paths)
        (init paths, last paths)
        (error "At least one input and only one output must be specified.")
