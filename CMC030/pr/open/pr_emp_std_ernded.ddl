DEFINE RECORD CDD$TOP.PR.PR_EMP_STD_ERNDED

        DESCRIPTION IS /*Payroll Employee Standard ERNDED File*/.

        PR_EMP_STD_ERNDED_CDD STRUCTURE.

        /* Element =
        Description = Employee Number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Payment,Deduction,noncompensaTion,Memo */
        RTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Earnings/Deduction code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Rate or amount of Earn/Ded */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Limit to pay or deduction */
        LIMIT                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount earn/ded to date */
        CTDBAL                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount accrued */
        ACCRUED                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Date to stop paying/ded */
        ENDDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Earn/Ded frequency */
        FREQ                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = 1-hourly,2-mile,3-gross,4-net,5-per pay */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = User defined */
        USERDEF                 DATATYPE IS TEXT SIZE IS 30.

        END PR_EMP_STD_ERNDED_CDD STRUCTURE.

END PR_EMP_STD_ERNDED.
