import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V
import Data.Either
import Debug.Trace

type Position = Int
data Operation = Add Position Position Position
  | Multiply Position Position Position
  | End
  deriving (Eq, Show)

intLit :: Int -> GenParser Int st Int
intLit x = token showTok posFromTok testTok
  where showTok t = show t
        posFromTok _ = initialPos "(unknown)"
        testTok t = if x == t then Just t else Nothing

addP :: GenParser Int st Operation
addP = do
  intLit 1
  pos1 <- anyToken
  pos2 <- anyToken
  pos3 <- anyToken
  return $ Add pos1 pos2 pos3

multiplyP :: GenParser Int st Operation
multiplyP = do
  intLit 2
  pos1 <- anyToken
  pos2 <- anyToken
  pos3 <- anyToken
  return $ Multiply pos1 pos2 pos3

endP :: GenParser Int st Operation
endP = intLit 99 >> return End

operationP = choice [addP, multiplyP, endP]

parseOperation :: [Int] -> Either ParseError Operation
parseOperation = parse operationP "(unknown)"

parseOperationM :: (Monad m) => [Int] -> m Operation
parseOperationM input =
  case parseOperation input of
    Left err -> fail $ show err
    Right success -> return success
  
data World = World (Vector Int) Int
  deriving (Show)

newWorld :: [Int] -> World
newWorld opcodes = World (V.fromList opcodes) 0

step :: World -> Maybe World
step (World opcodes currentPos) =
  fmap (\x -> World x $ currentPos + 4) newOpcodes
  where
    currentOpcodes = V.toList $ V.drop currentPos opcodes
    currentOp = fromRight undefined $ parseOperation currentOpcodes
    newOpcodes = case currentOp of
      End -> Nothing
      Add src1 src2 dest -> Just $ opcodes // [(dest, opcodes ! src1 + opcodes ! src2)]
      Multiply src1 src2 dest -> Just $ opcodes // [(dest, opcodes ! src1 * opcodes ! src2)]

main :: IO ()
main = do
  let testInput = [1, 0, 0, 3, 99]
  let testWorld = newWorld testInput
  print $ step testWorld
  -- let result = parseOperation testInput
  -- print result
  -- let testInput = "1,0,0,3,99"
  -- print $ Add 1 2 3
