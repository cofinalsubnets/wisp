module Wisp.Reader (parseWisp) where

import Wisp.Types
import Text.ParserCombinators.Parsec hiding (count)
import Control.Applicative hiding ((<|>), many, optional)

parseWisp :: String -> Either ParseError (Value s)
parseWisp = parse wisp ""

wisp :: GenParser Char st (Value s)
wisp = optional whitespace *> expr <* optional whitespace
  where

    whitespace = many1 $ oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr <|> quasiquotedExpr <|> splicedExpr <|> mergedExpr

    nakedExpr = sexp <|> atom

    quotedExpr = (\v -> Lst [SF Quote, v]) `fmap` (quote *> expr)
      where quote = char '\''

    quasiquotedExpr = (\v -> Lst [SF Quasiquote, v]) `fmap` (qquote *> expr)
      where qquote = char '`'

    splicedExpr = (\v -> Lst [SF Splice, v]) `fmap` (splice *> expr)
      where splice = char ','

    mergedExpr = (\v -> Lst [SF Merge, v]) `fmap` (splice *> expr)
      where splice = char '@'

    sexp = fmap Lst $ char '(' *> optional (whitespace <|> comment) *> (fm <|> ls) <* char ')'
    fm = (:) <$> specialForm <*> ls
    ls = expr `sepEndBy` many (whitespace <|> comment)

    comment = char ';' *> many (noneOf "\n") <* char '\n'

    atom = str <|> number <|> symbol <|> true <|> false

    escaped c r = try $ string ['\\', c] >> return r

    str = Str `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents =  escaped '"' '"'
                          <|> escaped 'n' '\n'
                          <|> escaped 'r' '\r'
                          <|> escaped 't' '\t'
                          <|> escaped '\\' '\\'
                          <|> noneOf "\\\""

    specialForm =  sf "if" If
               <|> sf "do" Do
               <|> sf "fn" Lambda
               <|> sf "def" Def
               <|> sf "set" Set
               <|> sf "macro" Macro
               <|> sf "quote" Quote
               <|> sf "quasiquote" Quasiquote
               <|> sf "splice" Splice
               <|> sf "merge" Merge
               <|> sf "undef" Undef
               <|> sf "catch" Catch

    sf s f = try $ string s >> whitespace >> return (SF f)

    symbol = (Sym . pack) `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!?:<>&$^|{}[]%~")

    number =  (Flt . read) `fmap` try dec
          <|> (Int . read) `fmap` try neg
          <|> (Int . read) `fmap` pos

      where pos = many1 digit
            neg = (:) <$> char '-' <*> pos
            dec = (++) <$> (pos <|> neg) <*> ((:) <$> char '.' <*> pos)

    true  = fmap Bln $ try (string "#t") >> return True
    false = fmap Bln $ try (string "#f") >> return False

