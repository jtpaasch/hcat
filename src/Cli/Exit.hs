module Cli.Exit (
    exitWithErr
  ) where

{- This module handles exiting the program. -}

import qualified System.Exit as Exit
import qualified System.IO as SysIO

{- Exit the program with a message and exit code 1. -}
exitWithErr :: String -> IO a
exitWithErr msg =
  let writeToStderr = SysIO.hPutStrLn SysIO.stderr
      failure = Exit.ExitFailure 1
  in writeToStderr msg >> Exit.exitWith failure
