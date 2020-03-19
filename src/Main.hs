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

  -- Parse those args.
  -- We'll either get a 'CliArgs.Error', or 'CliArgs.Config'.
  result <- CliArgs.parse args

  -- If we have errors, print the message.
  -- Otherwise, run the app and print the result.
  case result of
    Left e -> do
      case e of
        CliArgs.Help msg -> putStrLn msg
        CliArgs.NoArgs msg -> putStrLn msg
        CliArgs.InvalidOpts msg -> putStrLn msg
    Right config -> do
      let filepaths = CliArgs.get_filepaths config
      result <- Handler.run filepaths
      putStr result
