{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Turtle
import Lang
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()



{-
Parser for Float
-}

flonat :: Parser Float
flonat = do xs <- many1 digit
            return (read xs)

floating :: Parser Float
floating = do xs <- many1 digit
              return (read ("0." ++ xs))

num :: Parser Float
num = do s <- flonat
         char '.'
         xs <- floating
         return (s + xs)
        ||| flonat

{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

varIdent :: Parser String
varIdent = do x <- alphanum
              xs <- many alphanum
              return (x:xs)

variable :: Parser String
variable = token varIdent

expr :: Parser Expr
expr = do t <- term
          space
          do char '+'
             space
             e <- expr
             return (Add t e)
           ||| do char '-'
                  space
                  e <- expr
                  return (Sub t e)
                 ||| return t

term :: Parser Expr
term = do f <- factor
          space
          do char '*'
             space
             t <- term
             return (Mul f t)
           ||| do char '/'
                  space
                  t <- term
                  return (Div f t)
                ||| return f

factor :: Parser Expr
factor = do d <- num
            return (Val (Num d))
          ||| do char '('
                 space
                 e <- expr
                 space
                 char ')'
                 return e
            ||| do s <- ident
                   return (Var s)

cmd :: Parser TurtleCmd
cmd = do cmd <- identifier
         do v <- expr
            case cmd of
                "fd"     -> return (Fd v)
                "rt"     -> return (Rt v)
                "lt"     -> return (Lt v)
                "moveTo" -> do v2 <- expr                      -- extensions
                               return (MoveTo (Point (v, v2))) -- extensions
                _        -> failure
           ||| do s <- identifier
                  case cmd of
                     "changeCol" -> return (ChangeCol s)       -- extensions
                     _           -> failure
                ||| case cmd of
                       "penDown" -> return (PenDown)           -- extensions
                       "penUp"   -> return (PenUp)             -- extensions
                       _         -> failure                    -- extensions

vars :: Parser [String]
vars = do v <- variable
          do char ','
             space
             do vs <- vars
                return (v:vs)
           ||| return [v]
       ||| return []

vEx :: Parser [Expr]
vEx = do v <- expr
         do char ','
            space
            do vs <- vEx
               return (v:vs)
          ||| return [v]

call :: Parser Program
call = do name <- variable
          case name of
            "if" -> do if' <- ifState    -- extensions
                       return if'
            "for" -> do for' <- forLoop  -- extensions
                        return for'
            "repeat" -> do re <- repeat' -- extensions
                           return re
            "while" -> do re <- repeat'  -- extensions
                          return re
            _-> do char '('
                   do v <- vEx
                      char ')'
                      return (Call name v)
                    ||| do char ')'
                           return (Call name [])

-- seq function is already implemented in the Haskell (so added ')
seq' :: Parser [Program]
seq' = do c <- cmd
          do char ';'
             do s <- seq'
                return ((T c):s)
           ||| return [T c]
        ||| do ca <- call
               do char ';'
                  do s <- seq'
                     return (ca:s)
                ||| return [ca]
        ||| do var <- assign
               do char ';'
                  do s <- seq'
                     return (var:s)
                ||| return [var]
        ||| return [Empty]

func :: Parser (String, Function)
func = do name <- variable
          char '('
          do var <- vars
             char ')'
             space
             char '{'
             space
             do c <- cmd
                space
                char '}'
                return (name, (Fn var (T c)))
              ||| do ca <- call
                     space
                     char '}'
                     return (name, Fn var (ca))
                   ||| do sequ <- seq'
                          space
                          char '}'
                          return (name, (Fn var (Seq (sequ))))
           ||| failure
         ||| failure
         
toFunc :: String -> [(String, Function)]
toFunc xs = fst (head (parse (many func) xs))

{-
  Extension Parsers
-}

-- "variable = <Expr>"
assign :: Parser Program
assign = do var <- variable
            space
            char '='
            space
            e <- expr
            return (Assign (Var var) (e))

-- Assign value for "for-loop" (could be empty)
forAssign :: Parser Program
forAssign = do var <- variable
               space
               char '='
               space
               e <- expr
               return (Assign (Var var) (e))
             ||| return Empty

{- 
if (x > 5) then { doSomething() } else { doAnything() }
-}
ifState :: Parser Program
ifState = do space
             char '('
             c <- condition
             space
             char ')'
             space
             do t <- thenState
                space
                do e <- elseState
                   return (If c t e)

-- "x > 0"
operator :: Parser Expr
operator = do s <- expr
              space
              do char '>'
                 space
                 ss <- expr
                 return (Greater s ss)
               ||| do
                 string ">="
                 space
                 ss <- expr
                 return (GE s ss)
               ||| do
                 string "=="
                 space
                 ss <- expr
                 return (Eq s ss)
               ||| do
                 char '<'
                 space
                 ss <- expr
                 return (Less s ss)
               ||| do
                 string "<="
                 space
                 ss <- expr
                 return (LE s ss)

-- "x > 0 && y > 0"
condition :: Parser Expr
condition = do c1 <- operator
               space
               do and <- string "&&"
                  space
                  do c2 <- condition
                     return (And c1 c2)
                ||| do or <- string "||"
                       space
                       do c2 <- condition
                          return (Or c1 c2)
                     ||| do not <- char '!'
                            space
                            return (Not c1)
                          ||| return c1


-- "then { doSomething() }"
thenState :: Parser Program
thenState = do t <- string "then"
               space
               char '{'
               space
               do c <- cmd
                  space
                  char '}'
                  return (T c)
                ||| do ca <- call
                       space
                       char '}'
                       return (ca)
                     ||| do sequ <- seq'
                            space
                            char '}'
                            return (Seq sequ)
                          ||| return Empty

-- "else { doAnything() }"
elseState :: Parser Program
elseState = do t <- string "else"
               space
               char '{'
               space
               do c <- cmd
                  space
                  char '}'
                  return (T c)
                ||| do ca <- call
                       space
                       char '}'
                       return (ca)
                     ||| do sequ <- seq'
                            space
                            char '}'
                            return ((Seq sequ))
                          ||| do space
                                 char '}'
                                 return (Empty)
{-
  for (i = 0; i < 3; i=i+1) {
    sayHello(i);
    sayGoodBye(i+1)
  }
-}
forLoop :: Parser Program
forLoop = do space
             char '('
             space
             do as1 <- forAssign
                char ';'
                space
                do con <- condition
                   char ';'
                   space
                   do as2 <- forAssign
                      do space
                         char ')'
                         space
                         char '{'
                         space
                         do se <- seq'
                            space
                            char '}'
                            return (Loop as1 con as2 (Seq se))
                          -- ||| do char '}'
                          --        return (Loop as1 con as2 Empty)
                          --        ||| failure

                ||| failure
{-
repeat 5 {
  commenting(100);
  bluescreen()
}

or

while x < 5 {
   coding()
}
-}
repeat' :: Parser Program
repeat' = do v <- expr
             space
             char '{'
             do seq <- seq'
                space
                char '}'
                return (Repeat v (Seq seq))
           ||| do char '('
                  do c <- condition
                     char ')'
                     space
                     char '{'
                     do seq <- seq'
                        space
                        char '}'
                        return (Loop Empty c Empty (Seq seq))
