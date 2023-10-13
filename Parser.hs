module Parser where
import Control.Applicative
import Numeric

newtype Parser f a = Parser { runParser :: String -> f (a, String) }

parseMap :: (a -> b) -> (a, String) -> (b, String)
parseMap fn (val, str) = (fn val, str)

instance Functor f => Functor (Parser f) where
    fmap :: (a -> b) -> Parser f a -> Parser f b
    fmap fn (Parser pa) = 
        Parser (\str -> parseMap fn <$> pa str)

instance Monad f => Applicative (Parser f) where
    pure :: a -> Parser f a
    pure val = 
        Parser (\str -> pure (val, str))
    (<*>) :: Parser f (a -> b) -> Parser f a -> Parser f b
    Parser p1 <*> Parser p2 =
        Parser (\str -> p1 str >>= \(atob, str') -> parseMap atob <$> p2 str')

instance (Monad f, Alternative f) => Alternative (Parser f) where
    empty :: Parser f a
    empty =
      Parser (\str -> empty)
    (<|>) :: Parser f a -> Parser f a -> Parser f a
    Parser p1 <|> Parser p2 = 
      Parser (\str -> p1 str <|> p2 str)

instance Monad f => Monad (Parser f) where
    (>>=) :: Parser f a -> (a -> Parser f b) -> Parser f b
    Parser pa >>= fn = Parser ps
      where
        ps str =
            do (a, str') <- pa str
               runParser (fn a) str'

{- Examples -}

type MaybeParser a = Parser Maybe a

charP :: Char -> MaybeParser Char
charP c = Parser p
  where
    p (c':rest) | c == c' = Just (c, rest)
    p _ = Nothing

charsP :: String -> MaybeParser Char
charsP cs = foldl (<|>) empty $ map charP cs

stringP :: String -> MaybeParser String
stringP str = sequenceA $ map charP str

data JsonValue
    = JsonObject [(String, JsonValue)]
    | JsonArray [JsonValue]
    | JsonNumber Double
    | JsonString String
    | JsonBool Bool
    | JsonNull
    deriving (Show)

parseJson :: String -> Maybe JsonValue
parseJson str =
    case runParser jsonElementP str of
        Just (value, []) -> Just value
        _ -> Nothing

jsonValueP :: MaybeParser JsonValue
jsonValueP =
    const JsonNull <$> stringP "null" <|>
    const (JsonBool False) <$> stringP "false" <|>
    const (JsonBool True) <$> stringP "true" <|>
    JsonNumber <$> jsonNumberP <|>
    JsonString <$> jsonStringP <|>
    JsonArray <$> jsonArrayP <|>
    JsonObject <$> jsonObjectP

jsonObjectP :: MaybeParser [(String, JsonValue)]
jsonObjectP =
  charP '{' *> jsonMembersP <* charP '}' <|>
  const [] <$> (charP '{' *> jsonWsP <* charP '}')

jsonMembersP :: MaybeParser [(String, JsonValue)]
jsonMembersP =
    (:) <$> (jsonMemberP <* charP ',') <*> jsonMembersP <|>
    (:[]) <$> jsonMemberP

jsonMemberP :: MaybeParser (String, JsonValue)
jsonMemberP = 
    (,) <$> (jsonWsP *> jsonStringP <* jsonWsP <* charP ':') <*> jsonElementP

jsonArrayP :: MaybeParser [JsonValue]
jsonArrayP =
  charP '[' *> jsonElementsP <* charP ']' <|>
  const [] <$> (charP '[' *> jsonWsP <* charP ']')

jsonElementsP :: MaybeParser [JsonValue]
jsonElementsP = 
  (:) <$> (jsonElementP <* charP ',') <*> jsonElementsP <|>
  (:[]) <$> jsonElementP 

jsonElementP :: MaybeParser JsonValue
jsonElementP =
  jsonWsP *> jsonValueP <* jsonWsP

jsonStringP :: MaybeParser String
jsonStringP =
  charP '"' *> jsonCharactersP <* charP '"'

jsonCharactersP :: MaybeParser String
jsonCharactersP =
  (:) <$> jsonCharacterP <*> jsonCharactersP <|>
  pure ""

jsonCharacterP :: MaybeParser Char
jsonCharacterP =
    charP '\\' *> jsonEscapeP <|>
    regularCharP
  where
    regularCharP = Parser p
    p (c:rest) | 0x20 <= fromEnum c && fromEnum c <= 0x10FFFF && c /= '"' && c /= '\\' =
        Just (c, rest)
    p _ = Nothing

jsonEscapeP :: MaybeParser Char
jsonEscapeP =
    charP 'u' *> readHexChars <|>
    const '\9' <$> charP 't' <|>
    const '\13' <$> charP 'r' <|>
    const '\10' <$> charP 'n' <|>
    const '\12' <$> charP 'f' <|>
    const '\8' <$> charP 'b' <|>
    charP '/' <|>
    charP '\\' <|>
    charP '"'

  where
    readHexChars :: MaybeParser Char
    readHexChars = parseHexChar <$> jsonHexP <*> jsonHexP <*> jsonHexP <*> jsonHexP
    parseHexChar :: Char -> Char -> Char -> Char -> Char
    parseHexChar a b c d =
        toEnum ((((toHexNum a) * 16 + toHexNum b) * 16 + toHexNum c) * 16 + toHexNum d)
    toHexNum :: Char -> Int
    toHexNum '0' = 0
    toHexNum '1' = 1
    toHexNum '2' = 2
    toHexNum '3' = 3
    toHexNum '4' = 4
    toHexNum '5' = 5
    toHexNum '6' = 6
    toHexNum '7' = 7
    toHexNum '8' = 8
    toHexNum '9' = 9
    toHexNum 'A' = 10
    toHexNum 'B' = 11
    toHexNum 'C' = 12
    toHexNum 'D' = 13
    toHexNum 'E' = 14
    toHexNum 'F' = 15

jsonHexP :: MaybeParser Char
jsonHexP =
    charsP "abcdef" <|>
    charsP "ABCDEF" <|>
    jsonDigitP

jsonNumberP :: MaybeParser Double
jsonNumberP =
    (\x y z -> read (x++y++z)) <$> jsonIntegerP <*> jsonFractionP <*> jsonExponentP

jsonIntegerP :: MaybeParser String
jsonIntegerP = 
    (\x y z -> x:y:z) <$> charP '-' <*> jsonOneNineP <*> jsonDigitsP <|>
    (\x y -> [x,y]) <$> charP '-' <*> jsonDigitP <|>
    (:) <$> jsonOneNineP <*> jsonDigitsP <|>
    (:[]) <$> jsonDigitP

jsonDigitsP :: MaybeParser String
jsonDigitsP = 
    (:) <$> jsonDigitP <*> jsonDigitsP <|>
    (:[]) <$> jsonDigitP

jsonDigitP :: MaybeParser Char
jsonDigitP = 
    jsonOneNineP <|>
    charP '0'

jsonOneNineP :: MaybeParser Char
jsonOneNineP = 
    charsP "123456789"

jsonFractionP :: MaybeParser String
jsonFractionP =
    (:) <$> charP '.' <*> jsonDigitsP <|>
    pure ""

jsonExponentP :: MaybeParser String
jsonExponentP = 
    (\x y z -> x:y++z) <$> charP 'e' <*> jsonSignP <*> jsonDigitsP <|>
    (\x y z -> x:y++z) <$> charP 'E' <*> jsonSignP <*> jsonDigitsP <|>
    pure ""

jsonSignP :: MaybeParser String
jsonSignP = 
    (:[]) <$> charP '-' <|>
    (:[]) <$> charP '+' <|>
    pure ""

jsonWsP :: MaybeParser ()
jsonWsP =
    (charP '\9' *> jsonWsP) <|> 
    (charP '\10' *> jsonWsP) <|>
    (charP '\13' *> jsonWsP) <|>
    (charP '\32' *> jsonWsP) <|>
    pure ()

main :: IO ()
main =
    do input <- getContents
       print (parseJson input)
