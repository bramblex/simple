
module Parser (parseLam, parseProgram, parseBinding) where

import Lambda
import Text.Parsec
import Text.Parsec.String

id_char = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

lambda :: Parser Expr
lambda = do many (char ' ')
            char '\\' <|> char 'λ'
            many (char ' ')
            var <- many1 (oneOf id_char)
            many (char ' ')
            char '.'
            many (char ' ')
            expr <- lambda
            many (char ' ')
            return $ Lam var expr
     <|> do many (char ' ')
            apps <- many1 term
            many (char ' ')
            return $ foldl1 App apps

term :: Parser Expr
term = do many (char ' ')
          var <- many1 (oneOf id_char)
          many (char ' ')
          return $ Var var 
   <|> do many (char ' ')
          char '('
          many (char ' ')
          expr <- lambda      
          many (char ' ')
          char ')'
          many (char ' ')
          return expr

binding :: Parser Binding
binding = do many (char ' ')
             name <- many1 (oneOf id_char)
             many (char ' ')
             char '='
             many (char ' ')
             expr <- lambda
             many (char ' ')
             return $ Binding name expr

program :: Parser Program
program = do pro <- binding `endBy` (char '\n')
             eof
             return pro

parseLam :: String -> Either ParseError Expr
parseLam = parse lambda "(unknown)"

parseBinding :: String -> Either ParseError Binding
parseBinding = parse binding "(unknown)"

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "(unknown)" . unlines . filter (any (/=' ')) . lines
