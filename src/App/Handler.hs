module App.Handler (
    run
  ) where

{- The main application handler.

To run the application, pass a list of file paths to the 'run' function. -}

import qualified App.Cmd as Cmd

{-- The program on the system we'll shell the work out to. -}
prog :: String
prog = "cat"

{-- Take a list of file paths, send them to 'prog', and return the result. -}
run :: [FilePath] -> IO String
run paths = do
  result <- Cmd.exec prog paths
  case result of
    Left e ->
      case e of
        Cmd.NoProg ->
          return $ "No '" ++ prog ++ "' program found on your machine.\n"
        Cmd.NoFile msg -> return msg
        Cmd.NoPerm msg -> return msg
        Cmd.Other msg -> return msg 
    Right msg -> return msg
