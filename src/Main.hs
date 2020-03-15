module Main (main) where

{- The main module of the program. -}

import qualified System.Environment as Environ
import qualified Cli.Args as CliArgs
import qualified App.Handler as Handler

{- The main entry point into the program. -}
main :: IO ()
main = do

  -- Get the arguments provided at the command line.
  args <- Environ.getArgs

  -- Parse those args. If errors, this will exit with a message.
  -- Otherwise, we get back the list of args, which we assume are filepaths.
  filepaths <- CliArgs.parse args

  -- Hand the filepaths off to the application to handle them.
  -- Then print the results.
  result <- Handler.run filepaths
  putStr result
