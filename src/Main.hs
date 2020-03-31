module Main where

import Data.List.Split

main :: IO ()
main = do
       file <- readFile "data.csv"
       let rowsOfStrings = lines file
       let datafyIt list = map (splitOn "," ) list
       let countryList = datafyIt rowsOfStrings
       mapM_ print $ generateDataFromCSV countryList

generateDataFromCSV :: [[String]] -> [([Char], [(String, [Char])])]
generateDataFromCSV countryList =
  let
  concatFirstTwo record =
    let
      state = head record
      country = head $ tail record
      theRest = drop 2 record
      combinedElement =
        if state /= "" then
          state ++ " " ++ country
        else
          country
    in
      combinedElement : theRest

  dropCoords record =
    let
      location = head record
      withoutCoords = location : (drop 2 $ tail record)
    in
      withoutCoords

  tupleIt record =
    (head record, tail record)

  titles = drop 4 $ head (take 1 countryList)

  zipIt listForZipping record  =
    (fst record , zip listForZipping (snd record) )
  in
   map (zipIt titles)
  $ map tupleIt
  $ map dropCoords
  $ map concatFirstTwo
  $ drop 1 countryList
