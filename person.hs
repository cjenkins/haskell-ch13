module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Input name:"
  personName <- getLine
  putStrLn "Input age:"
  ageString <- getLine
  case mkPerson personName (read ageString) of
    Right p -> putStrLn ("Yay! Successfully got a person: " ++ show p)
    Left piv -> putStrLn ("Boo!  Not a person: " ++ show piv)
  return ()
  
