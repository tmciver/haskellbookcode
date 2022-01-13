module Chapter12.MonadThrowExample
(ValidAge, validateAge)
where

import Control.Monad.Catch (MonadThrow(..), Exception)

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

data ValidAge = ValidAge Integer

validateAge :: Integer -> Maybe ValidAge
validateAge age = if age > 0 && age < 50
                  then Just (ValidAge age)
                  else Nothing


-- Maybe

mkPersonMaybe :: Name -> Age -> Maybe Person
mkPersonMaybe name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing













-- Either

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

-- Compiles fine without Eq
toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

mkPersonEither :: Name
               -> Age
               -> Either PersonInvalid Person
mkPersonEither name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow



















-- MonadThrow

mkPersonM :: MonadThrow m
          => Name
          -> Age
          -> m Person
mkPersonM name age
  | name /= "" && age >= 0 = pure $ Person name age
  | name == "" = throwM NameEmpty
  | otherwise = throwM AgeTooLow

instance Exception PersonInvalid
