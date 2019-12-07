#!/usr/bin/env runghc

getFuel :: Int -> Int
getFuel x = x `div` 3 - 2

totalFuel :: Int -> Int
totalFuel = sum . tail . takeWhile (>0) . iterate getFuel

main :: IO ()
main = do
  inputString <- readFile "input.txt"
  let inputLines = lines inputString
  let inputInts = map read inputLines :: [Int]
  let fuelValues = map totalFuel inputInts
  print $ sum fuelValues
