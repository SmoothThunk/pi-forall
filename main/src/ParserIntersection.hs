module IntersectionTypeParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (space)

data Type = TypeVar String | TypeIntersection Type Type deriving Show

-- Parser for type variables
typeVarParser :: Parser Type
typeVarParser = TypeVar <$> many1 letter

-- Parser for intersection types
intersectionParser :: Parser Type
intersectionParser = do
    t1 <- typeParser
    spaces >> string "/\" >> spaces
    t2 <- typeParser
    return (TypeIntersection t1 t2)

-- Main parser for types
typeParser :: Parser Type
typeParser = intersectionParser <|> typeVarParser

parseIntersectionTypes :: String -> IO ()
parseIntersectionTypes input = do
    putStrLn "Parsing intersection types:"
    mapM_ testAndPrint (lines input)
  where
    testAndPrint line =
        case parse typeParser "" line of
            Left err -> putStrLn $ "Error parsing '" ++ line ++ "': " ++ show err
            Right result -> putStrLn $ "Parsed '" ++ line ++ "': " ++ show result

main :: IO ()
main = do
    input <- readFile "intersectionTestCases.txt"  -- Adjust the file name as needed
    parseIntersectionTypes input
