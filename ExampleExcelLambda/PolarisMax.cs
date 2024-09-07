using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Examplea
{
    
        internal class PolarisMax
        {
            //Straight from Chat GPT Interpretation


            // Withdrawal Penalty Calculation
            public static double WithdrawalPenalty(bool isActivated, double wdTaken, double pCv, double cv, double pMaw)
            {
                double noWd = 1;
                double preActivationWd = 1 - Math.Max(0, wdTaken - pMaw) / (pCv - pMaw);
                double postActivationWd = 1 - wdTaken / cv;
                return wdTaken != 0
                    ? (isActivated ? preActivationWd : postActivationWd)
                    : noWd;
            }

            // Income Credit Base Calculation
            public static double IncomeCreditBase(bool isActivated, bool isAnniversary, bool hasIncomeCredit, double anniversaryCv, double purchasePayments, double wdPenaltyMult, double prevIncomeCreditBase)
            {
                if (!isActivated)
                {
                    if (hasIncomeCredit)
                        return prevIncomeCreditBase;
                    else
                        return (isAnniversary
                            ? Math.Max(prevIncomeCreditBase, anniversaryCv + purchasePayments)
                            : prevIncomeCreditBase + purchasePayments) * wdPenaltyMult;
                }
                return 0;
            }

            // Income Credit Calculation
            public static double IncomeCredit(double payPeriod, double prevIncomeCreditBase, bool isActivated, bool isAnniversary, double incCredRate)
            {
                double picb = payPeriod > 0 ? prevIncomeCreditBase : 0;
                bool isPreSysWdOnAnniversary = !isActivated && isAnniversary;
                return isPreSysWdOnAnniversary ? picb * incCredRate : 0;
            }

            // Anniversary Cv Calculation
            public static double AnniversaryCv(bool isAnniversary, double payPeriod, double cv)
            {
                return !isAnniversary || payPeriod == 0 ? 0 : cv;
            }

            // Initial Current Income Base Calculation
            public static double InitialCurrentIncomeBase(bool isAnniversary, double prevIB, double anniversaryCv, double purchasePayments, double wdPenaltyMult)
            {
                return (isAnniversary
                    ? Math.Max(prevIB, anniversaryCv + purchasePayments)
                    : prevIB + purchasePayments) * wdPenaltyMult;
            }

            // Income Base Adjustment Calculation
            public static double IncomeBaseAdjustment(double initialCurrIB, double prevIB, bool isAnniversary, bool isActivated, double incomeCredit)
            {
                double delta = initialCurrIB - prevIB;
                return isAnniversary && !isActivated ? Math.Max(0, incomeCredit - delta) : 0;
            }

            // Income Base Calculation
            public static double IncomeBase(double initialCurrIB, double adjustment)
            {
                return initialCurrIB + adjustment;
            }

            // Max Allowable Withdrawal Amount Calculation
            public static double MaxAllowableWdAmount(double cv, double incomeBase, double mawdRate)
            {
                return cv > 0 ? incomeBase * mawdRate : 0;
            }

            // MAW Stack Calculation
            public static double[] MawStack(double prevCv, double prevIB, double prevICB, double prevMawa, double payPeriod, double wdTaken, double cv, bool isAnniversary, bool isActivated, double purchasePayments, double mawdRate, double incCredRate)
            {
                double anniversaryCv = AnniversaryCv(isAnniversary, payPeriod, cv);
                double wdPenaltyMult = WithdrawalPenalty(isActivated, wdTaken, prevCv, cv, prevMawa);
                double incomeCredit = IncomeCredit(payPeriod, prevICB, isActivated, isAnniversary, incCredRate);
                double initialCurrIB = InitialCurrentIncomeBase(isAnniversary, prevIB, anniversaryCv, purchasePayments, wdPenaltyMult);
                double adjustment = IncomeBaseAdjustment(initialCurrIB, prevIB, isAnniversary, isActivated, incomeCredit);
                bool hasIncomeCredit = adjustment > 0;
                double incomeCreditBase = IncomeCreditBase(isActivated, isAnniversary, hasIncomeCredit, anniversaryCv, purchasePayments, wdPenaltyMult, prevICB);
                double incomeBase = IncomeBase(initialCurrIB, adjustment);
                double mawa = MaxAllowableWdAmount(cv, incomeBase, mawdRate);

                return new double[] { cv, incomeBase, incomeCreditBase, mawa, wdPenaltyMult, adjustment, incomeCredit };
            }

            // Protected Income Payment Calculation
            public static double ProtectedIncomePayment(double cv, double incomeBase, double pipRate)
            {
                return cv == 0 ? pipRate * incomeBase : 0;
            }

            // Is Anniversary Calculation
            public static bool IsAnniversary(double payPeriod)
            {
                return payPeriod % 1 == 0;
            }

            // Is Activated Calculation
            public static bool IsActivated(double payPeriod, double activationPeriod)
            {
                return payPeriod >= activationPeriod;
            }
        }
    }


