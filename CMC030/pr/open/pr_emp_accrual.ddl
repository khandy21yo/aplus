DEFINE RECORD CDD$TOP.PR.PR_EMP_ACCRUAL

        DESCRIPTION IS /*Accrual Definition Header*/.

        PR_EMP_ACCRUAL_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Accrual (Pay) Code */
        ATYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Hours Unavailable */
        HOURSUNA                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hours Available */
        HOURSAVA                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Dollars Unavailable */
        DOLLARUNA               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Dollars Available */
        DOLLARAVA               DATATYPE IS G_FLOATING.

        /* Element =
        Description = When to make available */
        AVAILFLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Date (YYYYMMDD) just in case */
        AVAILDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = YESNO
        Description = Post Accrual to GL? */
        GLFLAG                  DATATYPE IS TEXT SIZE IS 1.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END PR_EMP_ACCRUAL_CDD STRUCTURE.

END PR_EMP_ACCRUAL.
