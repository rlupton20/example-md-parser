module Main where

import Data.List (foldl')
import System.IO (readFile)

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

-- This parser always fails
failure :: Parser a
failure = Parser $ const Fail

-- This parser fails only when there is still data left on
-- the stream
eof :: Parser ()
eof = Parser $ \s -> if s == "" then Partial () "" else Fail

-- If we can parse an a, and then parse a b, and have a way
-- of combining a's and b's, we ought to be able to build a new
-- parser which can parse our a and b, then provide us with a c
--
-- Really, we ought to use an Applicative instance
joinWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
joinWith f p q = Parser $ \s ->
  case runParser p $ s of
    Partial x t -> case runParser q $ t of
      Partial y u -> Partial (f x y) u
      Fail -> Fail
    _ -> Fail

-- This is the more flexible version of joinWith - I only
-- really wrote this because I know applicative functors...
--
-- I avoid using this below, because I don't think it's an
-- obvious primitive to form when just learning Haskell
-- This is <*> for an Applicative.
app :: Parser (a -> b) -> Parser a -> Parser b
app p q = Parser $ \s ->
  case runParser p $ s of
    Partial f t -> case runParser q $ t of
      Partial x u -> Partial (f x) u
      Fail -> Fail
    Fail -> Fail

-- inject consumes no input, and returns a value. Otherwise
-- known as pure (Applicative) or return (Monad)
inject :: a -> Parser a
inject x = Parser $ \s -> Partial x s

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
-- TODO make this efficient (the current implementation is crap)
tag :: String -> Parser String
tag =  let eatchars = joinWith $ flip (:) in
         fmap reverse . foldl' eatchars (inject "") . map character

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

-- Make a decision about what parser to run next based on the
-- result of the previous parse.
--
-- Did anyone mention Monads (this is (>>=))?
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  case runParser p $ s of
    Partial x s' -> runParser (f x) s'
    Fail -> Fail

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
line = let newline = character '\n' in
  joinWith const (takeUntil newline anyChar) newline
  `orTry` takeUntil failure anyChar

-- Parser for a header of given depth
header' :: Int -> Parser FormattedText
header' n = joinWith (Section n) 
  (bind headerTag $ \_ -> line)
  (takeUntil (headerSat (<=n)) (markdown' n))
  where
    headerTag = tag $ replicate n '#' ++ " "

-- headerSat matches headers whose depth satisfies the passed predicate
headerSat :: (Int -> Bool) -> Parser FormattedText
headerSat t = bind (peek headerLength) (\n -> if t n then header' n else failure)
  where
    -- headerLength looks ahead to see the length of the header coming up
    headerLength :: Parser Int
    headerLength = fmap length $
      joinWith (:) (character '#') (takeUntil (character ' ') (character '#'))

markdown' :: Int -> Parser FormattedText
markdown' n = headerSat (n<) `orTry` (fmap Text line)

markdown :: Parser [FormattedText]
markdown = takeUntil eof (markdown' 0)


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
  print $ runParser (header' 1) "# Title"
  print $ runParser (header' 1) example
  print $ runParser (headerSat (<=3)) "### foo"
  print $ runParser (headerSat (<=3)) "#### foo"
  print $ runParser markdown example
  putStrLn "\nParsing file.md...\n"
  fmap (parse markdown) (readFile "file.md") >>= print

