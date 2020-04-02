module Main where

import Data.List.Split

type CSVFile = String
type CSVTable = [[String]]
type Date = String
type NumOfCases = String
type DateCaseInfo = (Date, NumOfCases)
type Country = String
type FormattedRecord = (Country, [DateCaseInfo])

main :: IO ()
main = do
       csvFile <- readFile "data.csv"
       mapM_ print $ getDataFromCSVFile $ makeCsvTable csvFile

makeCsvTable :: CSVFile -> CSVTable
makeCsvTable csvFile =
  let
    breakRowsIntoCells :: [String] -> CSVTable
    breakRowsIntoCells listOfStrings = map (splitOn "," ) listOfStrings

    fileAsRowsOfStrings = lines csvFile
    csvTable = breakRowsIntoCells fileAsRowsOfStrings
  in
    csvTable

getDataFromCSVFile :: CSVTable -> [FormattedRecord]
getDataFromCSVFile countryList =
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
