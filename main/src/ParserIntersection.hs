import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (space)

-- Define the data types for types
data Type = TypeVar String | TypeIntersection Type Type deriving Show

-- Parser for type variables
typeVarParser :: Parser Type
typeVarParser = TypeVar <$> many1 letter

-- Parser for intersection types
intersectionParser :: Parser Type
intersectionParser = do
    -- Parse the left side of the intersection
    t1 <- typeParser
    -- Parse spaces, the "/\\", and more spaces
    spaces >> string "/\\" >> spaces
    -- Parse the right side of the intersection
    t2 <- typeParser
    -- Combine the parsed types into an intersection type
    return (TypeIntersection t1 t2)

-- Main parser for types
typeParser :: Parser Type
typeParser = intersectionParser <|> typeVarParser

-- Example usage
main :: IO ()
main = do
    let input = "A /\\ B"
    -- Parse the input using the typeParser
    case parse typeParser "" input of
        Left err -> print err
        Right result -> print result
