module VA
    ( mawStack
    ) where

--Activation Date: The date on which your Lifetime Income is activated. Upon activation of Lifetime Income, changes cannot be made to the Covered Person(s) or Income Options. 
--Anniversary Value: The contract value on any Benefit Year Anniversary. The Continuation Contribution, if applicable, is included in the calculation of Anniversary Values. Please see SPOUSAL CONTINUATION below.
--Benefit Effective Date: The date the Living Benefit is elected. The Benefit Effective Date is the same as the Contract Issue Date.
--Benefit Quarter: Each consecutive 3 month period starting on the Benefit Effective Date.
--Benefit Quarter Anniversary :The date following each consecutive 3 month period starting on the Benefit Effective Date. If the next Benefit Quarter Anniversary has no corresponding date, then the Benefit Quarter Anniversary will be deemed to be the following day.
--For example, if a Benefit Quarter Anniversary is November 29, the next Benefit Quarter Anniversary would be February 29 of the following year; however, in a non-Leap Year, there is no corresponding date. Therefore, the next Benefit Quarter Anniversary would be March 1.
--Benefit Year: Each consecutive one year period starting on the Benefit Effective Date.
--Benefit Year Anniversary: The date on which each Benefit Year begins.
--Contract Year: Each consecutive one year period starting on the Contract Issue Date.
--Covered Person(s): The person, or persons, whose lifetime withdrawals are guaranteed under the Living Benefit. 
--Covered Person Changes: The Covered Person(s) may be changed in the event of Life Change Event prior to or on the Activation Date. No further changes may be made to the Covered Person(s) after the Activation Date.



--Excess Withdrawal: Any withdrawal, or portion of a withdrawal, that is taken in a Benefit Year after the Activation Date and
--exceeds the greater of the maximum amount that may be withdrawn each Benefit Year without reducing the Income Base 
--or the Required Minimum Distribution amount as calculated by the Annuity Service Center. 
--An Excess Withdrawal will cause the Income Base and the Maximum Annual Withdrawal Amount to be recalculated.
withdrawalPenalty :: Bool -> Double -> Double -> Double -> Double -> Double
withdrawalPenalty isAct wdT pCv cv pMaw =
    let noWd = 1
        preActivationWd = 1 - max 0 ((wdT- pMaw) / (pCv - pMaw))
        postActivationWd = 1 - (wdT / cv)
    in if wdT /= 0.0
       then if isAct then preActivationWd else postActivationWd
       else noWd

--Higher Anniversary Value: For Polaris Income Max, the current Anniversary Value that is greater than the current Income Base.
--Income Base: The Income Base is a value used to determine the Living Benefit fee and the maximum amount that may be withdrawn each Benefit Year after the Activation Date without reducing the Income Base.
--The Income Base is also used to determine the amount paid each year over the lifetime of the Covered Person(s), if and when the contract value is reduced to zero, but the Income Base is still greater than zero, or upon the Latest Annuity Date.
incomeCreditBase :: Bool -> Bool -> Bool -> Double -> Double -> Double -> Double -> Double
incomeCreditBase isAct isAnn hasIncCred annCv purchasePayments wdPenaltyMult prevIncomeCreditBase =
    if not isAct
    then onIncomeCredit isAnn hasIncCred annCv purchasePayments prevIncomeCreditBase * wdPenaltyMult
    else 0

onIncomeCredit :: Bool -> Bool -> Double -> Double -> Double -> Double
onIncomeCredit isAnn hasIncCred annCv purchasePayments prevIncomeCreditBase
  | hasIncCred = prevIncomeCreditBase
  | isAnn = max prevIncomeCreditBase (annCv + purchasePayments)
  | otherwise = prevIncomeCreditBase + purchasePayments

--Income Credit: Applicable to Polaris Income Max only, the Income Credit is an amount that may be added to the 
--Income Base prior to the Activation Date as shown in the following table:
--Income Credit Base: Applicable to Polaris Income Max only, the Income Credit Base is used solely as a basis for calculating the Income Credit prior to the Activation Date.
--Income Credit Percentage: Applicable to Polaris Income Max only, a percentage of the Income Credit Base used to determine the Income Credit amount prior to the Activation Date.
--Income Option: The Income Option is elected by You at contract issue. The Maximum Annual Withdrawal Amounts and Protected Income Payments offered in each Income Option vary by age and whether you elect one or two Covered Persons.
incomeCredit :: Double -> Double -> Bool -> Bool -> Double -> Double
incomeCredit payPeriod prevIncomeCreditBase isAct isAnn incCredRate =
    let picb = if payPeriod > 0.0 then prevIncomeCreditBase else 0
        isPreSysWdOnAnniversary = not isAct && isAnn
    in if isPreSysWdOnAnniversary then picb * incCredRate else 0

anniversaryCv :: Bool -> Double -> Double -> Double
anniversaryCv isAnn payPeriod cv =
    if not isAnn || payPeriod == 0.0
    then 0
    else cv

initialCurrentIncomeBase :: Bool -> Double -> Double -> Double -> Double -> Double
initialCurrentIncomeBase isAnniversary prevIB annCv purchasePayments wdPenaltyMult =
    let base = if isAnniversary
               then max prevIB (annCv + purchasePayments)
               else prevIB + purchasePayments
    in base * wdPenaltyMult

incomeBaseAdjustment :: Double -> Double -> Bool -> Bool -> Double -> Double
incomeBaseAdjustment initialCurrIB prevIB isAnn isAct incCred =
    let delta = initialCurrIB - prevIB
    in if isAnn && not isAct
       then max 0.0 (incCred - delta)
       else 0

incomeBase :: Double -> Double -> Double
incomeBase initialCurrIB adjustment = initialCurrIB + adjustment

maxAllowableWdAmount :: Double -> Double -> Double -> Double
maxAllowableWdAmount cv iB mawdRate =
    if cv > 0.0
    then iB * mawdRate
    else 0
                    --cv,   ib,    icb,   mawa
type MawaElements = (Double,Double,Double,Double,Double,Double,Double)
--Life Change Event: A change to the Covered Person(s) upon marriage, divorce or death if prior to the Activation Date.
--Lifetime Income: Any withdrawal taken on or after the Activation Date that is all or part of the Maximum Annual Withdrawal Amount or Protected Income Payment.
--Maximum Annual Withdrawal Amount: The maximum amount that may be withdrawn each Benefit Year on or after activating Lifetime Income and while the contract value is greater than zero without reducing the Income Base.
--Maximum Annual Withdrawal Percentage: The percentage used to determine the Maximum Annual Withdrawal Amount available for withdrawal each Benefit Year after activating Lifetime Income and while the contract value is greater than zero.
mawStack :: MawaElements -> Double -> Double -> Double -> Bool -> Bool -> Double -> Double -> Double -> MawaElements --(Double, Double, Double, Double, Double, Double, Double)
mawStack (prevCv,prevIB, prevICB, prevMawa,_,_,_) payPeriod wdT cv isAnn isAct purchasePayments mawdRate incCredRate =
    let
        annCv = anniversaryCv isAnn payPeriod cv
        wdPenaltyMult = withdrawalPenalty isAct wdT prevCv cv prevMawa
        incCredit = incomeCredit payPeriod prevICB isAct isAnn incCredRate
        initialCurrIB = initialCurrentIncomeBase isAnn prevIB annCv purchasePayments wdPenaltyMult
        adjustment = incomeBaseAdjustment initialCurrIB prevIB isAnn isAct incCredit
        hasIncomeCredit = adjustment > 0.0
        incCreditBase = incomeCreditBase isAct isAnn hasIncomeCredit annCv purchasePayments wdPenaltyMult prevICB
        incBase = incomeBase initialCurrIB adjustment
        mawa = maxAllowableWdAmount cv incBase mawdRate
    in (cv, incBase, incCreditBase, mawa, wdPenaltyMult, adjustment, incCredit)
