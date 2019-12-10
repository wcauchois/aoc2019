import Data.List
import Data.Either
import Debug.Trace

data InProgress = InProgress {
  currentDigitValue :: Int,
  remainingDigits :: Int,
  hasConsecutive :: Bool,
  soFar :: [Int]
} deriving (Eq, Show)

type Password = [Int]

-- Return a list of: Left InProgress if there's more progress to be made, or a concrete Password
next :: InProgress -> [Either InProgress Password]
next (InProgress currentDigitValue remainingDigits hasConsecutive soFar)
  | remainingDigits == 0 = if hasConsecutive then [Right soFar] else []
  | otherwise = map (\d -> Left $ InProgress {
        currentDigitValue = d
      , remainingDigits = (remainingDigits - 1)
      , hasConsecutive = (hasConsecutive || (d == currentDigitValue))
      , soFar = (soFar ++ [d])
    }) [currentDigitValue..9]

generatePasswords :: Int -> [Password]
generatePasswords numDigits = helper $ map (\d -> InProgress d (numDigits - 1) False [d]) [0..9]
  where
    helper :: [InProgress] -> [Password]
    helper [] = []
    helper inProgresses =
      let nexts = concatMap next inProgresses
      in helper (lefts nexts) ++ (rights nexts) -- Recursively apply helper to in progresses, adding in the concrete passwords

showPassword :: Password -> String
showPassword = concatMap show

main :: IO ()
main = do
  let allPasswords = map showPassword $ generatePasswords 6
  let lowerBound = "265275"
  let upperBound = "781584"
  print $ length $ filter (\x -> x >= lowerBound && x <= upperBound) allPasswords
