module Parser (parseExpression) where

import Data.Foldable (fold)
import Types (CExp(..))
import Text.Parsec
import Control.Applicative ((<*),(*>),(<$>),(<$))

parseExpression :: String -> Either ParseError CExp
parseExpression = runParser (cExp <* eof) () ""

type Parser a = Parsec String () a

cExp :: Parser CExp
cExp = fold <$> inner `sepBy` many1 space
  where inner = word <|> literal

word :: Parser CExp
word = Term <$> many1 letter

literal :: Parser CExp
literal = choice [bool_,int_,char_,string_, quote_]
  where bool_ = char '#' *> (Bool <$> choice [True <$ char 'T', False <$ char 'F'])
        int_ = Int . read <$> many1 digit
        char_ = Char <$> between (char '\'') (char '\'') anyChar
        string_ = String <$> between (char '"') (char '"') (many inner)
          where inner = escaped <|> noneOf "\""
                escaped = char '\\' *> anyChar
        quote_ = Quote <$> between (char '[') (char ']') cExp
