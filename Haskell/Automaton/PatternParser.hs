
module PatternParser where

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Pattern

term :: Parser Pattern
term = buildExpressionParser ops atom 
    where ops = [
                    [Postfix (Repeat <$ char '*')],
                    [Infix (return Concatenate) AssocRight],
                    [Infix (Choose <$ char '|') AssocRight]
                ]
          atom = msum [Literal <$> lit, parens term, Empty <$ eps]

lit = noneOf "ε*|()"
parens = between (char '(') (char ')')
eps = char 'ε'

pattern = helper . parse term "(unkownerror)" 
    where helper (Right n) = n
          helper (Left n) = error . show $ n

matchStr :: String -> String -> Bool
matchStr pstr mstr = matches (pattern pstr) mstr
