DEFINE RECORD CDD$TOP.PR.PR_EMP_ACCRUAL_RATE

        DESCRIPTION IS /*Accrual Definition Rates*/.

        PR_EMP_ACCRUAL_RATE_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Accrual (Pay) Type */
        ATYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Start Date (YYYYMMDD) */
        SDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Minimum hours worked to get */
        MINHOUR                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Maximum hours that get accrued for */
        MAXHOUR                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Rate per hour worked */
        HOURRATE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Maximum unused to allow */
        MAXACCRUE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Rate Code if we ever need it */
        RATECODE                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Periods to accrue for (12345S) */
        FREQ                    DATATYPE IS TEXT SIZE IS 6.

        END PR_EMP_ACCRUAL_RATE_CDD STRUCTURE.

END PR_EMP_ACCRUAL_RATE.
