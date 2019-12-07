import Text.ParserCombinators.Parsec

type Position = Int
data Operation = Add Position Position Position
  | Multiply Position Position Position
  | End
  deriving (Eq, Show)

intP :: GenParser Char st Int
intP = read <$> many1 digit

addP :: GenParser Char st Operation
addP = do
  string "1,"
  pos1 <- intP
  char ','
  pos2 <- intP
  char ','
  pos3 <- intP
  return $ Add pos1 pos2 pos3

operationParser :: GenParser Char st Operation
operationParser = do
  addP

main :: IO ()
main = do
  let testInput = "1,0,0,3,99"
  print $ Add 1 2 3
