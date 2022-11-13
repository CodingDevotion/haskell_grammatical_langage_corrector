module ConnectionHelper where

import qualified Data.Map as Map
-- Custom imports

import TerminalMenuHelper (printToTerminal)
import Types (Color (..), PasswordHash, Username)

-- We do not save the user password. Only the hash of these password are saved in the database.
userPasswordHashDB :: Map.Map Username PasswordHash
-- For testing purposes: here are some user credentials:
userPasswordHashDB =
  Map.fromList
    [ ("alex", "ifmmpifmmp"), -- username: alex    password : hello
      ("john", "epfepfepfe"),
      ("iburzynski", "ufbdifsufb") -- username: john    passord : doe)
    ]

-- This is a very very simple hashing method. It is not secure at all. The purpose is just to
-- practice foldr and Functor while retreiving user credentials.
-- We buff the password to 10 characters using take and cycle, and add 1 to each character (a -> b, e -> f)
hashPassword :: String -> PasswordHash
hashPassword password = succ <$> (take 10 . cycle) password

canUserConnect :: Username -> String -> Maybe Bool
canUserConnect username password = (==) <$> Map.lookup username userPasswordHashDB <*> pure (hashPassword password)

connection :: IO Bool
connection = do
  printToTerminal Blue "\nPlease enter your user name:"
  userName <- getLine

  printToTerminal Blue "\nPlease enter your password:"
  passwordInput <- getLine

  let isUserConnected = canUserConnect userName passwordInput
  case isUserConnected of
    Just True -> do
      printToTerminal Green "\n\nYou are successfully connected"
      return True
    Just False -> do
      printToTerminal Red "\n\nWrong password, please try again"
      connection
    Nothing -> do
      printToTerminal Yellow "\n\nThis username does not exists"
      connection
