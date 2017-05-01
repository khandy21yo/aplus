DEFINE RECORD CDD$TOP.GL.GL_35CHART

        DESCRIPTION IS /*CHART OF ACCOUNTS*/.

        GL_35CHART_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = DESCRIPTION */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Account type */
        ACCTYPE                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Cash flow */
        FLOW                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Work Capitol */
        WORK                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Financial type */
        FINTYPE                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Summary flag */
        SUMMARY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = System id */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = Accrual Account */
        ACCRUAL_ACCT            DATATYPE IS TEXT SIZE IS 18.

        END GL_35CHART_CDD STRUCTURE.

END GL_35CHART.
