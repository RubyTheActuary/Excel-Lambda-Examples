module Lib
    ( runReport
    ) where

import VA
import Data.List (transpose, intercalate)
import Text.Printf (printf)


runReport :: IO ()
runReport = do
             let 
                pmData = calculatePolarisMax
                (header,finalOutput,conditionals) = pmData
                table = createTable header finalOutput conditionals
             putStrLn table

-------------------------------------------------------
-------------------------------------------------------
----------------- FORMATTING FUNCTIONS ----------------
-- Function to format a number with commas and two decimal places
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

createTable :: [String] -> [[Double]] -> [[Bool]] -> String
createTable header numlists bools = let
                        formattedBools = map (map formatBool) bools
                        formattedNums = map (map formatMoney) numlists
                        -- Combine the lists into rows of tuples
                        lists = formattedNums ++ formattedBools
                        hlists = zipWith (:) header lists
                        rows = transpose hlists
                        -- Determine the column widths
                        widths = columnWidths rows
                        -- Format each row
                        formattedRows = map (`formatRow` widths) rows
                        -- Join all rows with new lines
                        table = unlines formattedRows
                    in table
