module Main where

import Data.List (foldl')

-- Conceptually a parser is a value of type String -> Maybe a,
-- but we want to consume strings incrementally, and combine results
-- as we go
data Result a = Partial a String | Fail deriving (Eq, Show)

-- A parser takes a string and returns a decision about how the parse
-- is going
newtype Parser a = Parser { runParser :: String -> Result a }

-- Parsers have an obvious Functor instance
instance Functor Parser where
  fmap f p = Parser $ \s -> case runParser p $ s of
    Partial x s -> Partial (f x) s
    Fail -> Fail

-- A parse is successful if and only if we parsed something
-- and have no input left to consume
parse :: Parser a -> String -> Maybe a
parse p s = case runParser p $ s of
  Partial a [] -> Just a
  _ -> Nothing

-- If we can parse an a, and then parse a b, and have a way
-- of combining a's and b's, we ought to be able to build a new
-- parser which can parse our a and b, then provide us with a c
joinWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
joinWith f p q = Parser $ \s ->
  case runParser p $ s of
    Partial x t -> case runParser q $ t of
      Partial y u -> Partial (f x y) u
      Fail -> Fail
    _ -> Fail

-- inject consumes no input, and returns a value
inject :: a -> Parser a
inject x = Parser $ \s -> Partial x s

-- character parses out a single character from a string
character :: Char -> Parser Char
character c = Parser $ \s -> case s of
  "" -> Fail
  c':s' -> if c == c' then Partial c s' else Fail

-- tag parses a matching string of characters out of a String
-- TODO make this efficient (the current implementation is crap)
tag :: String -> Parser String
tag =  let eatchars = joinWith $ flip (:) in
         fmap reverse . foldl' eatchars (inject "") . map character

-- Consume as many matching characters as possible
chars :: Char -> Parser String
chars c = Parser $ \s ->
  let (cs,ss) = (takeWhile (==c) s, dropWhile (==c) s) in
    Partial cs ss

-- Consumes as many matching characters as possible, but at least 1
chars1 :: Char -> Parser String
chars1 c = joinWith (:) (character c) (chars c)

-- rest matches the rest of the input
rest :: Parser String
rest = Parser $ \s -> Partial s ""

-- try one parser, and if that fails, try another
orTry :: Parser a -> Parser b -> Parser (Either a b)
orTry p q = Parser $ \s ->
  case runParser p $ s of
    Partial x s' -> Partial (Left x) s'
    Fail -> case runParser q $ s of
      Partial y s' -> Partial (Right y) s'
      Fail -> Fail

-- like orTry, but for when both parsers provide the same type
-- and we have no reason to keep them distinguished
orTry' :: Parser a -> Parser a -> Parser a
orTry' p q = fmap merge $ orTry p q
  where
    merge :: Either a a -> a
    merge (Left x) = x
    merge (Right y) = y


-- Markdown types
data Format = Bold | Italic | Strikethrough | Code deriving (Eq, Show)
data FormattedText = Header Int String | Text [Format] String deriving (Eq, Show)

-- Parse a header line
header :: Parser FormattedText
header = let removeSpaces = dropWhile (==' ') in
  joinWith Header (fmap length $ chars1 '#') (fmap removeSpaces rest)

markdown :: Parser FormattedText
markdown = header `orTry'` (fmap (Text []) rest)

example :: String
example = "# This is a title\nSome text that introduces a section\n##Subsection\nBlah"


main :: IO ()
main = do
  print $ parse (character 'f') "f"
  print $ parse (character 'o') "f"
  print $ parse (tag "foo") "foo"
  print $ parse (tag "foo") "foobar"
  print $ parse (tag "foo") "oof"
  print $ runParser (chars '#') "#### Header"
  print $ runParser header "#### Header"
  print $ runParser header "Header"
  print . map (parse markdown) $ lines example
