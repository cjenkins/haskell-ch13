import Control.Monad

import System.Exit (exitSuccess)

import Data.Char

removeNonAscii :: String -> String
removeNonAscii = filter (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

toLowerString :: String -> String
toLowerString = map toLower

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((toLowerString $ removeNonAscii line1) == (toLowerString $ removeNonAscii $ reverse line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess
