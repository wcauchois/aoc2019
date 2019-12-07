import Text.ParserCombinators.Parsec
import Data.List.Split
import Data.List (find)
import Text.ParserCombinators.Parsec.Pos
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V
import Data.Either
import Debug.Trace
import Data.Maybe

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
  
data World = World { getOpcodes :: (Vector Int), getCurrentPos :: Int }
  deriving (Show)

newWorld :: [Int] -> World
newWorld opcodes = World (V.fromList opcodes) 0

parseWorld :: String -> World
parseWorld = newWorld . map read . splitOn ","

modifyOpcodes :: (Vector Int -> Vector Int) -> World -> World
modifyOpcodes f (World opcodes currentPos) = World (f opcodes) currentPos

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

fullySimulate :: World -> World
fullySimulate world =
  case step world of
    Nothing -> world
    Just world' -> fullySimulate world'

cartesian xs ys = [(x,y) | x <- xs, y <- ys]

outputFromNounVerb :: World -> Int -> Int -> Int
outputFromNounVerb world noun verb = getOpcodes (fullySimulate world') ! 0
  where world' = modifyOpcodes (\opcodes -> opcodes // [(1, noun), (2, verb)]) world

main :: IO ()
main = do
  input <- readFile "input.txt"
  let world = parseWorld input
  let nounVerbs = cartesian [0..99] [0..99]
  let desiredOutput = 19690720
  let Just (answerNoun, answerVerb) = find (\(noun, verb) -> outputFromNounVerb world noun verb == desiredOutput) nounVerbs
  print $ 100 * answerNoun + answerVerb
