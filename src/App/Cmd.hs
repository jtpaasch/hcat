module App.Cmd (
    Error (..)
  , exec
  ) where

{- This module provides utilities for running a system call.

To execute a program with arguments, use the 'exec' function. -}

import Data.List (isInfixOf)
import qualified Control.Exception as Exc
import qualified System.Exit as Exit
import qualified System.IO.Error as SysIOError
import qualified System.Process as P

{- For convenience/clarity. -}
type StderrorString = String
type StdoutString = String
type Program = String
type Argument = String

{- Specific errors we can encounter when executing a system call. -}
data Error =
    NoProg        -- ^If the called program (cat) can't be found.
  | NoFile String -- ^If a file doesn't exist.
  | NoPerm String -- ^If no permission to read a file.
  | Other String  -- ^Any other error.
  deriving (Show)

{- Check for certain IO errors. -}
handleError :: SysIOError.IOError -> Maybe Error
handleError e
  | SysIOError.isDoesNotExistError e = Just NoProg
  | otherwise = Just $ Other (show e)

{- Takes an error string, checks if it indicates that a file doesn't exist. -}
isNoFileError :: StderrorString -> Bool
isNoFileError e = isInfixOf "No such file" e

{- Takes an error string, checks if it indicates permission was denied. -}
isPermissionDeniedError :: StderrorString -> Bool
isPermissionDeniedError e = isInfixOf "Permission denied" e

{- Takes a program name and a list of args, and executes it.
   Return either an error, or the stdout. -}
exec :: Program -> [Argument] -> IO (Either Error StdoutString)
exec prog args = do
  result <- Exc.tryJust handleError $ P.readProcessWithExitCode prog args ""
  case result of
    Left e -> return (Left e)
    Right (code, out, err) -> 
      case code of
        Exit.ExitSuccess -> return $ Right out
        Exit.ExitFailure i
          | isNoFileError err ->
            return $ Left (NoFile err)
          | isPermissionDeniedError err ->
            return $ Left (NoPerm err)
          | otherwise -> 
            return $ Left (Other ("Exit " ++ (show i) ++ ": " ++ err))
