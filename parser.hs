module Parser where

import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
import Data.Char           (isSpace, isDigit, isAlpha, isLower, isUpper)
import Data.Maybe          (listToMaybe)

---------------------------------------------------------------------------
newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser parser) = parser

maybeParse :: Parser a -> String -> Maybe a
maybeParse (Parser p) s = listToMaybe $ fst <$> p s
---------------------------------------------------------------------------
instance Functor Parser where
    fmap f p = Parser (\inp -> case parse p inp of 
                       []         -> []
                       [(v, out)] -> [(f v, out)])

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    pf <*> px = Parser (\inp -> case parse pf inp of
                        []         -> []
                        [(f, out)] -> parse (f <$> px) out) 

instance Monad Parser where
    p >>= f = Parser (\inp -> case parse p inp of
                        []         -> []
                        [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    empty   = Parser $ \s -> []
    p <|> q = Parser (\inp -> case parse p inp of
                        []         -> parse q inp
                        [(v, out)] -> [(v, out)]) 
---------------------------------------------------------------------------
item :: Parser Char
item = Parser(\inp -> case inp of
              []     -> []
              (x:xs) -> [(x,xs)])
---------------------------------------------------------------------------
nil :: Parser ()
nil = Parser(\inp -> case inp of
              []     -> [((), "")]
              (x:xs) -> [])
---------------------------------------------------------------------------
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= (\x -> if p x then return x else empty)
---------------------------------------------------------------------------
space :: Parser ()
space = many (satisfy isSpace) >> return ()
---------------------------------------------------------------------------
token p = space >> p >>= (\v -> space >> return v)
