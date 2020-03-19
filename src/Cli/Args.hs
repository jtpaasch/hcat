module Cli.Args (
    Error (..)
  , Config
  , get_filepaths
  , parse
  ) where

{- This module parses arguments from the command line. -}

import Data.List (isPrefixOf)
import qualified Cli.Exit as Exit
import qualified Cli.Output as Output

{- For convenience/clarity. -}
type Argument = String

{- Explicit errors we handle. -}
data Error =
    Help String
  | InvalidOpts String
  | NoArgs String

{- Raw command line args get parsed into this. -}
data Config = Config {
  filepaths :: [FilePath]
}

{- Constructs a 'Config' record. -}
make_config :: [FilePath] -> Config
make_config filepaths = Config { filepaths = filepaths }

{- Get the filepaths from a 'Config' record. -}
get_filepaths :: Config -> [FilePath]
get_filepaths conf = filepaths conf

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
   If there are errors, returns the appropriate 'Error'.
   Otherwise, the arguments are parsed into a 'Config' record. -}
parse :: [Argument] -> IO (Either Error Config)
parse args =
  case noArgs args of
    True -> return $ Left (NoArgs Output.noArgsErr)
    False -> case containsHelp args of
      True -> return $ Left (Help Output.usage)
      False -> 
        let badOpts = invalidOpts args
        in case (length badOpts) > 0 of
          True -> return $ Left (InvalidOpts $ Output.invalidOptsErr badOpts)
          False -> return $ Right (make_config args)
