module Lib
    ( calculatePolarisMax
    ) where

import VA
import Data.List (transpose, intercalate,zip6,unzip7)
import Text.Printf (printf)


calculatePolarisMax :: IO ()
calculatePolarisMax = do
             --Inputs----------
             let mawRate = 0.0625
                 incomeCreditRate = 0.0525
                 pipRate = 0.04
                 --initialPremium = 100000.00
                 activationPeriod  :: Double
                 activationPeriod = 4.5
                 payPeriod :: [Double]
                 payPeriod = [0.0, 0.5 .. 11.0]
                 purchasePayments = map getPurchasePayments payPeriod
                 wdTaken = map getWdTaken payPeriod
                 contractValue :: [Double]
                 contractValue = [100000,165000,170000,255000,287000,287000,300000,305000,312000,302000,305000,280000,290000,260000,230000,230000,150000,150000,100000,100000,50000,50000,0]

                 -- Calculate Columns -------
                 isAnniversary :: [Bool]
                 isAnniversary = map (\p -> (p - fromIntegral (floor p))==0.0) payPeriod
                 isActivated = map (>= activationPeriod) payPeriod
                 initialValues = (0.0,0.0,0.0,0.0,0.0,0.0,0.0)
                 workTuples = (payPeriod,wdTaken,contractValue,isAnniversary,isActivated,purchasePayments)
                 workValues = tupleOf6ListsToListOfTuples workTuples
                 mawStackx me pp wt cv ann act pay = mawStack me pp wt cv ann act pay mawRate incomeCreditRate
                 mawTuples = scanl (\me (pp,wt,cv,ann,act,pay) -> mawStackx me pp wt cv ann act pay ) initialValues workValues
                 protectedIncomePayment = map (\(av,ib,_,_,_,_,_) -> if av<0.001 then pipRate * ib else 0 ) mawTuples

                 --output results ----
                 mawLists = unzip7 mawTuples
                 prepareForOutput (a,b,c,d,e,f,g) = [a,b,c,d,e,f,g]
                 mawOutput = prepareForOutput mawLists
                 header = ["Period","Pays","Wd Taken","CV","Income Base", "income Credit Base", "MAW Amount","Wd Penalty", "Adjustment", "Income Credit","PIP Amount","Is Anniversary","Is Activated"]
                 --the scanl puts the starting values in the first row of the output, which I do not whant to display. It also impacts the protectedIncomePayment.
                 finalOutput = [payPeriod,purchasePayments,wdTaken] ++ map (drop 1) mawOutput ++ [drop 1 protectedIncomePayment]
                 table = createTable header finalOutput [isAnniversary,isActivated]
             putStrLn table

getPurchasePayments :: Double -> Double
getPurchasePayments 0.0 = 100000.00
getPurchasePayments 0.5 = 60000.00
getPurchasePayments 1.5 = 90000.00
getPurchasePayments _ = 0.0

getWdTaken :: Double -> Double
getWdTaken 3.5 = 5000.00
getWdTaken 4.5 = 19500.00
getWdTaken 5.5 = 24960.00
getWdTaken 6.5 = 24483.00
getWdTaken _ = 0

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

tupleOf6ListsToListOfTuples :: ([a], [b], [c], [d], [e], [f]) -> [(a, b, c, d, e, f)]
tupleOf6ListsToListOfTuples (xs, ys, zs, us, vs, ws) =
    zip6 xs ys zs us vs ws
