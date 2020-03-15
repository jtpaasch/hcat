module Cli.Args (
    parse
  ) where

{- This module parses arguments from the command line. -}

import Data.List (isPrefixOf)
import qualified Cli.Exit as Exit
import qualified Cli.Output as Output

{- For convenience/clarity. -}
type Argument = String

{- Checks if a list of arguments is empty. -}
noArgs :: [Argument] -> Bool
noArgs [] = True
noArgs _ = False

{- Checks if a list of arguments contains -h or --help. -}
containsHelp :: [Argument] -> Bool
containsHelp [] = False
containsHelp args = elem "-h" args || elem "--help" args

{- Filters a list of arguments down to any that begin with
   a dash, which are not -h or --help. The only options
   for this program are -h and --help, so anything else
   is an invalid argument/option. -}
invalidOpts :: [Argument] -> [Argument]
invalidOpts args = 
  let isInvalid arg = isPrefixOf "-" arg && arg /= "-h" && arg /= "--help"
  in filter isInvalid args

{- Inspects a list of command line arguments.
   If the arguments have -h or --help in them, exit with the help/usage.
   If there are other problems, exit with an appropriate message. 
   Otherwise, assume the arguments are fine, as a list of filepaths,
   and return them. -}
parse :: [Argument] -> IO [FilePath]
parse args =
  case noArgs args of
    True -> Exit.exitWithErr Output.noArgsErr
    False -> case containsHelp args of
      True -> Exit.exitWithErr Output.usage
      False -> 
        let badOpts = invalidOpts args
        in case (length badOpts) > 0 of
          True -> Exit.exitWithErr $ Output.invalidOptsErr badOpts
          False -> return (args)
