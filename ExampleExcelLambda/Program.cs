using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Examplea
{
    class Program
    {
        static void Main(string[] args)
        {
            DoPolicyIllustration();
            Console.WriteLine("Done");
        }

        private static void DoPolicyIllustration()
        {
            #region  Inputs         

            const int periods = 23;
            const double activationTime = 4.5;
            const double incomeCreditRate = 0.0525;
            const double mawRate = 0.0625;
            const double pipRate = 0.04;
            var payPeriod = from p in Enumerable.Range(0, periods * 2) select p * 0.5;
            double[] pp = { 100000.00, 60000.00, 0, 90000.00 };
            var purchasePayments = Enumerable.Concat(pp, Enumerable.Repeat(0.0, periods - 4));
            double[] wds = { 5000.00, 0, 19500.00, 0, 24960.00, 0, 24483.00 };
            var withdrawalsTaken = Enumerable.Concat(Enumerable.Repeat(0.0, 7), Enumerable.Concat(wds, Enumerable.Repeat(0.0, periods - 7 - 5)));
            double[] accountValue = { 100000.00, 165000.00, 170000.00, 255000.00, 287000.00, 287000.00, 300000.00, 305000.00, 312000.00, 302000.00, 305000.00, 280000.00, 290000.00, 260000.00, 230000.00, 230000.00, 150000.00, 150000.00, 100000.00, 100000.00, 50000.00, 50000.00, 0.00 };

            var inputs = from z in payPeriod.Zip(purchasePayments.Zip(withdrawalsTaken.Zip(accountValue, (first, second) => new Tuple<double, double>(first, second)), (x, y) => new Tuple<double, double, double>(x, y.Item1, y.Item2)), (f, s) => new Tuple<double, double, double, double>(f, s.Item1, s.Item2, s.Item3))
                         select new { period = z.Item1, purchase = z.Item2, wdTaken = z.Item3, cv = z.Item4 };

            #endregion
            #region Initialize Program
            double prevCv = pp[0];
            double prevIB = pp[0];
            double prevICB = pp[0];
            double prevMawa = 0;
            #endregion
            #region Write Header
            ////////////// *********************************** ////////////////////////////////////////////////////
            const int padWidth = 16;
            string[] header = { "CV|", "INCOME BASE|", "INC. CREDIT BASE|", "MAWA|", "Wd Penalty|", "Income Base Adj|", "Income Credit|", "PIP" };
            foreach (var val in header)
            {
                Console.Write(val.PadLeft(padWidth));
            }
            Console.WriteLine();
            #endregion

            foreach (var input in inputs)
            {
                #region Program By Numbers
                bool isAnniversary = PolarisMax.IsAnniversary(input.period);
                bool isAcitivated = PolarisMax.IsActivated(input.period, activationTime);

                double[] result = PolarisMax.MawStack(prevCv, prevIB, prevICB, prevMawa, input.period, input.wdTaken, input.cv, isAnniversary, isAcitivated, input.purchase, mawRate, incomeCreditRate);
                var protectedIncomePayment = PolarisMax.ProtectedIncomePayment(input.cv, result[1], pipRate);

                prevCv = result[0];
                prevIB = result[1];
                prevICB = result[2];
                prevMawa = result[3];
                #endregion
                #region Write Body
                foreach (double val in result)
                {
                    string data = Math.Round(val,2).ToString();
                    Console.Write(data.PadLeft(padWidth));
                }
                Console.Write(Math.Round(protectedIncomePayment,2).ToString().PadLeft(padWidth));
                Console.WriteLine();
                #endregion
            }
        }
    }
}
