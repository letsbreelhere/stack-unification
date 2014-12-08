module Parser (parseExpression) where

import Data.Foldable (fold)
import Types (CExp(..))
import Text.Parsec
import Text.Parsec.Token
import Control.Applicative ((<*),(*>),(<$>),(<$))

parseExpression :: String -> Either ParseError CExp
parseExpression = runParser (cExp <* eof) () ""

type Parser a = Parsec String () a

languageDef :: LanguageDef ()
languageDef =
  LanguageDef { commentStart = "{{"
              , commentEnd = "}}"
              , commentLine = "--"
              , nestedComments = False
              , identStart = letter <|> oneOf "~!@$%^&*_+`-=,./<>?:"
              , identLetter = alphaNum <|> oneOf "~!@#$%^&*_+`-=,./<>?:"
              , opStart = oneOf []
              , opLetter = oneOf []
              , reservedNames = ["#t", "#f"]
              , reservedOpNames = ["[", "]"]
              , caseSensitive = True
              }

tokenParser :: TokenParser ()
tokenParser = makeTokenParser languageDef

cExp :: Parser CExp
cExp = fold <$> many inner
  where inner = word <|> literal

word :: Parser CExp
word = Term <$> identifier tokenParser

literal :: Parser CExp
literal = choice [bool_,int_,char_,string_, quote_]
  where bool_ = choice [ Bool True  <$ reserved tokenParser "#t"
                       , Bool False <$ reserved tokenParser "#f"
                       ]
        int_ = Int . fromInteger <$> integer tokenParser
        char_ = Char <$> charLiteral tokenParser
        string_ = String <$> stringLiteral tokenParser
        quote_ = Quote <$> brackets tokenParser cExp
