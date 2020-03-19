module Cli.Output (
    usage
  , noArgsErr
  , invalidOptsErr
  ) where

{- This module provides messages (e.g., errors) to print as output. -}

import Data.List (intercalate)

{- For convenience/clarity. -}
type Argument = String

{- The help/usage message. -}
usage :: String
usage =
  unlines [
      "USAGE: hcat [OPTIONS] [ARGUMENTS]"
    , ""
    , "  A simple cat program."
    , ""
    , "EXAMPLES:"
    , "  hcat --help"
    , "  hcat /path/to/file1 /path/to/file2 ..."
    , ""
    , "OPTIONS:"
    , "  -h, --help       Display this help."
    , ""
    , "ARGUMENTS:"
    , "  /path/to/file1   A path to a file."
    , "  /path/to/file2   A path to a file."
    , "  ...              Ditto."
    ]

{- Message if no arguments/filepaths were provided to the command line. -}
noArgsErr :: String
noArgsErr = "No file paths were specified.\nSee: hcat --help"

{- Message if any command line arguments are invalid options. -} 
invalidOptsErr :: [Argument] -> String
invalidOptsErr args =
     "Unrecognized option(s): "
  ++ (intercalate ", " args)
  ++ "\nSee: hcat --help"
