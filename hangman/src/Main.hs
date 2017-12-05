module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Test.Hspec

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 8

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in    l >= minWordLength
             && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
  deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered )
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ ) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          let zd = (zipper c)
          in zipWith zd word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was : " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, fill in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) =
  if (length guessed - (length (filter isJust filledInSoFar))) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

runHspec :: IO ()
runHspec = hspec $ do
  describe "fillInCharacter" $ do
    it "Adding already guessed character results in same Puzzle" $ do
      fillInCharacter (Puzzle "cat" [Just 'c', Nothing, Nothing] ['c']) 'c' `shouldBe`
                      (Puzzle "cat" [Just 'c', Nothing, Nothing] ['c', 'c'])
    it "Adding new guess character gets filled in Puzzle" $ do
      fillInCharacter (Puzzle "cat" [Just 'c', Nothing, Nothing] ['c']) 'a' `shouldBe`
                      (Puzzle "cat" [Just 'c', Just 'a', Nothing] ['a', 'c'])

  describe "handleGuess" $ do
    it "Adding already guessed character results in same Puzzle" $ do
      newPuzzle <- handleGuess (Puzzle "cat" [Nothing, Nothing, Nothing] ['k']) 'k'
      newPuzzle `shouldBe` (Puzzle "cat" [Nothing, Nothing, Nothing] ['k'])
    it "Adding new guess character gets filled in Puzzle" $ do
      newPuzzle <- handleGuess (Puzzle "cat" [Nothing, Nothing, Nothing] []) 'c'
      newPuzzle `shouldBe` (Puzzle "cat" [Just 'c', Nothing, Nothing] ['c'])

main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
