#!/usr/bin/env runghc

main :: IO ()
main = do
  inputString <- readFile "input.txt"
  let inputLines = lines inputString
  let inputInts = map read inputLines :: [Int]
  let fuelValues = map (\x -> x `div` 3 - 2) inputInts
  print $ sum fuelValues
