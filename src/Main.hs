module Main where

import Data.List (foldl')
import System.IO (readFile)

-- Conceptually a parser is a value of type String -> Maybe a,
-- but we want to consume strings incrementally, and combine results
-- as we go
data Result a = Partial a String | Fail deriving (Eq, Show)

-- Result has an obvious Functor instance
instance Functor Result where
  fmap f (Partial x s) = Partial (f x) s
  fmap _ Fail = Fail

-- A parser takes a string and returns a decision about how the parse
-- is going
newtype Parser a = Parser { runParser :: String -> Result a }

-- Parsers have an obvious Functor instance
instance Functor Parser where
  fmap f p = Parser $ \s -> case runParser p $ s of
    Partial x s -> Partial (f x) s
    Fail -> Fail

-- Parsers also have an applicative instance
instance Applicative Parser where
  pure v = Parser $ \s -> Partial v s
  af <*> av = Parser $ \s ->
    case runParser af s of
      Fail -> Fail
      Partial f s -> fmap f $ runParser av s

-- Parsers can be sequenced, with subsequent parsers using the values
-- of previous parses - we get a Monad instance
instance Monad Parser where
  return = pure
  ma >>= f = Parser $ \s ->
    case runParser ma s of
      Fail -> Fail
      Partial x s -> runParser (f x) s


-- A parse is successful if and only if we parsed something
-- and have no input left to consume
parse :: Parser a -> String -> Maybe a
parse p s = case runParser p $ s of
  Partial a [] -> Just a
  _ -> Nothing

-- This parser always fails
failure :: Parser a
failure = Parser $ const Fail

-- This parser fails only when there is still data left on
-- the stream
eof :: Parser ()
eof = Parser $ \s -> if s == "" then Partial () "" else Fail

-- character matches a single character from a string
character :: Char -> Parser Char
character c = Parser $ \s -> case s of
  "" -> Fail
  c':s' -> if c == c' then Partial c s' else Fail

-- Matches any character
anyChar :: Parser Char
anyChar = Parser $ \s ->
  case s of
    "" -> Fail
    c:s' -> Partial c s'

-- tag parses a matching string of characters out of a String
tag :: String -> Parser String
tag v = Parser $ \s -> go v s
  where
    go [] s = Partial v s
    go _ [] = Fail
    go (c:cs) (d:ds) = if c == d then go cs ds else Fail

-- try one parser, and if that fails, try another
orTry' :: Parser a -> Parser b -> Parser (Either a b)
orTry' p q = Parser $ \s ->
  case runParser p $ s of
    Partial x s' -> Partial (Left x) s'
    Fail -> case runParser q $ s of
      Partial y s' -> Partial (Right y) s'
      Fail -> Fail

-- like orTry', but for when both parsers provide the same type
-- and we have no reason to keep them distinguished. I use this one
-- more here, so I give it the nicer name
orTry :: Parser a -> Parser a -> Parser a
orTry p q = fmap merge $ orTry' p q
  where
    merge :: Either a a -> a
    merge (Left x) = x
    merge (Right y) = y

-- Consumes the input with a parser until end of input, or match
-- on a different parser
--
-- TODO Try and remove the reverse
takeUntil :: Parser a -> Parser b -> Parser [b]
takeUntil p q = fmap reverse $ Parser $ \s -> go [] s
  where
    go acc s
      | s == "" = Partial acc ""
      | otherwise =
          case runParser p $ s of
            Partial _ _ -> Partial acc s
            Fail -> case runParser q $ s of
              Fail -> Fail
              Partial x s' -> go (x:acc) s'

-- Take a parser, parse, but don't consume the stream
peek :: Parser a -> Parser a
peek p = Parser $ \s -> case runParser p $ s of
  Partial x _ -> Partial x s
  Fail -> Fail

-- Markdown types
data FormattedText = Section Int String [FormattedText] | Text String deriving (Eq, Show)

-- Consumes a line and bins the newline at the end if it exists.
line :: Parser String
line = untilNewline `orTry` takeUntil failure anyChar
  where
    newline = character '\n'
    untilNewline = takeUntil newline anyChar <* newline

-- Parser for a section of given depth.
section :: Int -> Parser FormattedText
section n = Section n <$> headerTitle <*> subsections
  where
    headerTag = tag $ replicate n '#' ++ " "
    headerTitle = headerTag *> line
    subsections = takeUntil (parseSectionIfDepthSat (<=n)) $ markdown' n

-- parseSectionIfDepthSat matches headers whose depth satisfies the passed predicate
parseSectionIfDepthSat :: (Int -> Bool) -> Parser FormattedText
parseSectionIfDepthSat p = do
  l <- peek headerLength
  if p l then section l else failure
  where
    -- headerLength looks ahead to see the length of the header coming up
    headerLength :: Parser Int
    headerLength = do
      _ <- character '#'
      tail <- takeUntil (character ' ') (character '#')
      let l = length tail + 1  -- + 1 for the character we consumed at the start
      return l

markdown' :: Int -> Parser FormattedText
markdown' n = parseSectionIfDepthSat (n<) `orTry` fmap Text line

markdown :: Parser [FormattedText]
markdown = takeUntil eof $ markdown' 0

example :: String
example = "# This is a title\nSome text that introduces a section\n## Subsection\nBlah\n# Another title\nFoo"

main :: IO ()
main = do
  print $ parse (character 'f') "f"
  print $ parse (character 'o') "f"
  print $ parse (tag "foo") "foo"
  print $ parse (tag "foo") "foobar"
  print $ parse (tag "foo") "oof"
  print $ runParser line "This is a line\nFoo"
  print $ runParser (section 1) "# Title"
  print $ runParser (section 1) example
  print $ runParser (parseSectionIfDepthSat (<=3)) "### foo"
  print $ runParser (parseSectionIfDepthSat (<=3)) "#### foo"
  print $ runParser markdown example
  putStrLn "\nParsing file.md...\n"
  fmap (parse markdown) (readFile "file.md") >>= print

