module PolarisMaxDaily
    ( calculatePolarisMaxDaily
    ) where

--This conversion is in bad shape. Need to go through Excel and redo.
import Data.List --(isInfixOf, unzip7,zip5,zip4)
import Text.Regex ( matchRegex, mkRegex )
import GHC.Float (int2Double)

-- Define type aliases for better clarity
type Year = Int
type Amount = Double
type Result = (Year, Amount, Amount, Amount, Amount, Amount, Amount)

-- MaxStepValuePreviousYear function
maxStepValuePreviousYear :: Year -> Amount -> Amount -> Year -> Amount -> (Year, Amount, Amount)
maxStepValuePreviousYear prevYear prevMaxValue prevYearMaxValue year sUpValue =
    let mv = if prevYear /= year then sUpValue else max prevMaxValue sUpValue
        pymv = if prevYear /= year then prevMaxValue else prevYearMaxValue
    in (year, mv, pymv)

-- InterestOnPayments function
interestOnPayments :: Year -> Amount -> Amount -> Year -> Amount -> Bool -> Amount -> (Year, Amount, Amount)
interestOnPayments prevYear accumPay fvTotal year pay hasWdOccurrAfterAct mibPercent =
    let ap = accumPay + pay
        interestPay = if prevYear /= year then ap * mibPercent else 0
        fv = if hasWdOccurrAfterAct then 0 else fvTotal + interestPay + pay
    in (year, ap, fv)

-- StepUpValue function
stepUpValue :: Amount -> Amount -> Amount -> Amount -> Amount
stepUpValue prevStepUp prevIncomeBase contractValue ppInvested =
    if contractValue - ppInvested > max prevStepUp prevIncomeBase then contractValue else 0

-- PreActivationIncomeBase function
preActivationIncomeBase :: Amount -> Amount -> Amount -> Amount -> Bool -> Amount -> Amount -> Amount -> (Amount, Amount, Amount)
preActivationIncomeBase accumPenalty prevIncomeBase prevStepUp pay hasWdOccurrAfterAct wdPenalty cv fv =
    let awp = accumPenalty * wdPenalty
        minIb = fv * awp
        sUpValue = stepUpValue prevStepUp prevIncomeBase cv pay
        preActIB = if hasWdOccurrAfterAct then 0 else max (pay + prevIncomeBase) (max sUpValue minIb) * wdPenalty
    in (awp, preActIB, sUpValue)

-- PostActivationIncomeBase function
postActivationIncomeBase :: Amount -> Amount -> Bool -> Bool -> Amount -> Bool -> Amount -> Amount -> Amount
postActivationIncomeBase prevIB timeFromVal hasWdOccurredAfterActivation isAnniversary cv isActivated wdPenalty maxStepPY
  | timeFromVal <= 0.001 = 0
  | hasWdOccurredAfterActivation && isAnniversary = max cv (max prevIB maxStepPY)
  | isActivated = prevIB * wdPenalty
  | otherwise = 0


-- WithdrawalPenaltyDaily function
withdrawalPenaltyDaily :: Bool -> Amount -> Bool -> Amount -> Amount -> Amount
withdrawalPenaltyDaily hasLifetimeIncomeStarted wdTaken isActivation cv prevMawa =
    if not hasLifetimeIncomeStarted && wdTaken > 0 then
      1 -(  wdHaircutBeforeActivation isActivation wdTaken cv 
         +  wdHaircutAfterActivation prevMawa isActivation wdTaken cv)
    else
        1.0
-- WdHaircutBeforeActivation function
wdHaircutBeforeActivation :: Bool -> Amount -> Amount -> Amount
wdHaircutBeforeActivation isActivation wdTaken cv =
    if not isActivation then wdTaken / cv else 0

-- WdHaircutAfterActivation function
wdHaircutAfterActivation :: Amount -> Bool -> Amount -> Amount -> Amount
wdHaircutAfterActivation prevMawa isActivated wdTaken cv =
    let haircut = (wdTaken - prevMawa) / (cv - prevMawa)
    in if isActivated && wdTaken > prevMawa then haircut else 0


-- MaxAllowWdAmountDaily function
maxAllowWdAmountDaily :: Bool -> Amount -> Amount -> Amount
maxAllowWdAmountDaily hasLifetimeIncomeStarted iB mawd =
    if not hasLifetimeIncomeStarted then iB * mawd else 0

-- MawStackDaily function
mawStackDaily :: Result -> Year -> Amount -> Bool -> Amount -> Amount -> Bool -> Bool -> Bool -> Amount -> Amount -> Amount -> Result
mawStackDaily r year pay hasWdOccurrAfterAct cv timeFromVal isAnniversary isActivated hasLifetimeIncomeStarted wdTaken fv mawd =
    let (prevYear, accumPenalty, prevIncomeBase, prevStepUp, prevMaxValue, prevYearMaxValue, prevMawa) = r
        wdPenalty = withdrawalPenaltyDaily hasLifetimeIncomeStarted wdTaken isActivated cv prevMawa
        preActIBAll = preActivationIncomeBase accumPenalty prevIncomeBase prevStepUp pay hasWdOccurrAfterAct wdPenalty cv fv
        (awp,preActIB,sUpValue)=preActIBAll
        maxStepValues = maxStepValuePreviousYear prevYear prevMaxValue prevYearMaxValue year sUpValue
        (_,mv,maxStepValuePY) = maxStepValues
        postActIB = postActivationIncomeBase prevIncomeBase timeFromVal hasWdOccurrAfterAct isAnniversary cv isActivated wdPenalty maxStepValuePY
        incomeBase = preActIB + postActIB
        mawa = maxAllowWdAmountDaily hasLifetimeIncomeStarted incomeBase mawd
    in (year, awp, incomeBase, sUpValue, mv, maxStepValuePY, mawa)

-- ProtectedIncomeDaily function
protectedIncomeDaily :: Bool -> Bool -> Amount -> Amount -> Amount
protectedIncomeDaily hasLifetimeIncomeStarted isAnn incomeBase protectedIncPer =
    if hasLifetimeIncomeStarted && isAnn
    then protectedIncPer * incomeBase
    else 0


calculatePolarisMaxDaily :: ([String],[[String]],[[Int]], [[Double]], [[Bool]])
calculatePolarisMaxDaily =
    --starting values
    let activationTime :: Double
        activationTime = 3.2
        mawRate :: Double
        mawRate = 0.06
        incBaseRate :: Double
        incBaseRate = 0.05
        protectedIncomePaymentPercent :: Double
        protectedIncomePaymentPercent = 0.04
        contractValue :: [Double]
        contractValue = [100000.00,102000.00,105000.00,162000.00,166000.00,167000.00,250000.00,280000.00,279000.00,290000.00,285000.00,300000.00,310000.00,315000.00,312000.00,320000.00,311000.00,325000.00,322000.00,317000.00,330000.00,329000.00,321000.00,325000.00,317000.00,307000.00,270000.00,150000.00,100000.00,50000.00,0.00,0.00]
        wdTaken :: [Double]
        wdTaken = [0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,5000.00,0.00,0.00,0.00,10000.00,0.00,0.00,0.00,19200.00,0.00,0.00,0.00,26400.00,0.00,0.00,26000.00,0.00,19059.00,19059.00,19059.00,19059.00,0.00]
        valuesAsOf = ["0th Anniversary","Year 1, Day 25","Year 1, Day 105","Year 1 , Day 200","Year 1 , Day 300","1st Anniversary","Year 2 , Day 180","Year 2 , Day 250","2nd Anniversary","Year 3 , Day 45","Year 3 , Day 155","Year 3 , Day 275","3rd Anniversary","Year 4 , Day 65","Year 4 , Day 92","Year 4 , Day 350","4th Anniversary","Year 5 , Day 75","Year 5 , Day 80","5th Anniversary","Year 6 , Day 155","6th Anniversary","Year 7 , Day 37","Year 7 , Day 362","7th Anniversary","Year 8 , Day 46","8th Anniversary","9th Anniversary","10th Anniversary","11th Anniversary","Year 12 , Day 81","12th Anniversary"]
        year :: [Int]
        year = drop 1 $ scanl (\acc v -> if "Anniversary" `isInfixOf` v  then acc + 1 else acc ) 0 valuesAsOf
        day :: [Int]
        day = map extractDay valuesAsOf 
        timeFromValuation :: [Double]
        timeFromValuation = zipWith (\y d -> y + d / 365.0 - 1.0) (map int2Double year) (map int2Double day)
        isActivated = map (>=activationTime) timeFromValuation
        purchasePaymentInvested = zipWith purchasePayment year day
        hasLifetimeIncomeStarted = map (<0.01) contractValue 
        hasWdOccurredAfterActivation = zipWith (&&) isActivated $ scanl (\acc wd -> acc || wd > 0.0 ) False wdTaken
        isAnniversary = map (==0) day
        interestOnPremiumx a b c d e f = interestOnPayments a b c d e f incBaseRate
        workTuplesInt = (year,purchasePaymentInvested,hasWdOccurredAfterActivation)
        workValuesInt = tupleOf3ListsToListOfTuples workTuplesInt
        paysFv = scanl (\(y ,p,fv) (a,b,c) -> interestOnPremiumx y p fv a b c) (1,0.0,0.0) workValuesInt
        fvx = map (\(_,_,c) -> c) $ drop 1 paysFv
        --Grunt Work
        workTuples = (year, purchasePaymentInvested, hasWdOccurredAfterActivation, contractValue, timeFromValuation, isAnniversary, isActivated, hasLifetimeIncomeStarted, wdTaken, fvx)
        workValues = tupleOf10ListsToListOfTuples workTuples
        
        mawStackDailyx de a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = mawStackDaily de a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 mawRate
        mawaTuples = scanl (\de (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> mawStackDailyx de a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 ) (1,1.0,0.0,0.0,0.0,0.0,0.0) workValues
        cleanMawaTuples = drop 1 mawaTuples
       
       --debug extra results ---------------------
        mawaTuplesLessYear = map (\(_,a4,a5,a6,a7,a8,a9) -> (a4,a5,a6,a7,a8,a9)) cleanMawaTuples
        incomeBase = map (\(_,_,a3,_,_,_,_)->a3) cleanMawaTuples
        pip = map (\(a,b,c)-> protectedIncomeDaily a b c protectedIncomePaymentPercent) $ tupleOf3ListsToListOfTuples (hasLifetimeIncomeStarted,isAnniversary,incomeBase)
        mawLists = unzip6 mawaTuplesLessYear
        prepareForOutput (a,b,c,d,e,f) = [a,b,c,d,e,f]
        mawOutput = prepareForOutput mawLists
        header = ["Value As Of","Ann?","Act?","LI?","Wd?","Year","Day","Val","Payments","Wd Taken", "FV", "Acc Wd Pen","Income Base","Step Up Value","Max Value","MV Prev Year","MAW Amount","PIP Amount"]
        finalOutput = timeFromValuation : purchasePaymentInvested : wdTaken: fvx: mawOutput ++ [pip]
    in (header,[valuesAsOf],[year,day], finalOutput,[isAnniversary,isActivated,hasLifetimeIncomeStarted,hasWdOccurredAfterActivation])

extractDay :: String -> Int
extractDay str =
    let regex = mkRegex "Day ([0-9]+)"  -- Regex pattern to match "Day" followed by a number
    in case matchRegex regex str of
        Just [dayStr] -> read dayStr  -- Read the matched string as an Int
        _             -> 0              -- Return Nothing if no match

purchasePayment :: Int -> Int -> Double
purchasePayment 1 0 = 100000.00
purchasePayment 1 200 = 60000.00
purchasePayment 2 180 = 90000.00
purchasePayment _ _ = 0.0

tupleOf3ListsToListOfTuples :: ([a], [b], [c]) -> [(a, b, c)]
tupleOf3ListsToListOfTuples (xs, ys, zs) =
    zip3 xs ys zs

tupleOf10ListsToListOfTuples :: ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j]) -> [(a, b, c, d, e, f, g, h, i, j)]
tupleOf10ListsToListOfTuples (as, bs, cs, ds, es, fs, gs, hs, is,js) =
    zip10 as bs cs ds es fs gs hs is js

zipWith10                :: (a->b->c->d->e->f->g->h->i->j->k) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]
zipWith10 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (i:is) (j:js) (k:ks)
                   =  z a b c d e f g i j k : zipWith10 z as bs cs ds es fs gs is js ks
zipWith10 _ _ _ _ _ _ _ _ _ _ _ = []

zip10                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] ->[i] -> [j] ->[k]->
                             [(a,b,c,d,e,f,g,i,j,k)]
zip10                    =  zipWith10 (,,,,,,,,,)

