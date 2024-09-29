module Lib
    ( runReport
    ) where

import PolarisMaxDaily (calculatePolarisMaxDaily)
import PolarisMax (calculatePolarisMax)
import Data.List (transpose, intercalate)
import Text.Printf (printf)


runReport :: IO ()
runReport = do
             let 
                pmData = calculatePolarisMax
                (header,finalOutput,conditionals) = pmData
                pmtable = createTable header finalOutput conditionals [[]] [[]]

                pbdData = calculatePolarisMaxDaily
                (headerpmd,strs,ints,outs,bools)=pbdData
                pmdtable = createTable headerpmd outs bools strs ints
             putStrLn "--------------------------------------------------------"
             putStrLn "----------------------Polaris Max-----------------------"
             putStrLn "--------------------------------------------------------"
             putStrLn pmtable
             putStrLn "--------------------------------------------------------"
             putStrLn "-------------------Polaris Max Daily--------------------"
             putStrLn "--------------------------------------------------------"
             putStrLn pmdtable
                

-------------------------------------------------------
-------------------------------------------------------
----------------- FORMATTING FUNCTIONS ----------------
-- Function to format a number with commas and two decimal places
formatInt :: Int -> String
formatInt = printf "%d"

formatMoney :: Double -> String
formatMoney = printf "%.2f"

-- Function to format a boolean as "Yes" or "No"
formatBool :: Bool -> String
formatBool True  = "Yes"
formatBool False = "No"

-- Function to pad strings to ensure alignment
padString :: Int -> String -> String
padString width str = replicate (width - length str) ' ' ++ str

-- Function to determine the maximum width needed for each column
columnWidths :: [[String]] -> [Int]
columnWidths rows = map (maximum . map length) (transpose rows)

-- Function to format each row with padding
formatRow :: [String] -> [Int] -> String
formatRow row widths = intercalate " | " $ zipWith padString widths row

createTable :: [String] -> [[Double]] -> [[Bool]] -> [[String]] ->[[Int]] -> String
createTable header numlists bools [[]] [[]] = let
                        formattedBools = map (map formatBool) bools
                        formattedNums = map (map formatMoney) numlists
                        lists = formattedBools++ formattedNums
                        table = createTableBody header lists
                    in table
createTable header numlists bools strings ints = let
                        formattedBools = map (map formatBool) bools
                        formattedNums = map (map formatMoney) numlists
                        formmattedInts = map (map formatInt) ints
                        lists = strings ++ formattedBools ++ formmattedInts ++ formattedNums
                        table = createTableBody header lists
                    in table

createTableBody :: [String] -> [[String]] -> String
createTableBody header lists =
    let
         hlists = zipWith (:) header lists
         rows = transpose hlists
        -- Determine the column widths
         widths = columnWidths rows
        -- Format each row
         formattedRows = map (`formatRow` widths) rows
        -- Join all rows with new lines
         table = unlines formattedRows
    in table