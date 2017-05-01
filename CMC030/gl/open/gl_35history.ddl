DEFINE RECORD CDD$TOP.GL.GL_35HISTORY

        DESCRIPTION IS /*General Ledger summary by period*/.

        GL_35HISTORY_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Beginning Balance for period */
        DOLLARBEGIN             DATATYPE IS G_FLOATING.

        /* Element =
        Description = Change for period */
        DOLLARCHANGE            DATATYPE IS G_FLOATING.

        /* Element =
        Description = Budget for period */
        DOLLARBUDGET            DATATYPE IS G_FLOATING.

        /* Element =
        Description = Beginning balance */
        HOURBEGIN               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Change for period */
        HOURCHANGE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Budget for period */
        HOURBUDGET              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Beginning Balance */
        UNITBEGIN               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Chenge for period */
        UNITCHANGE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Budget for period */
        UNITBUDGET              DATATYPE IS G_FLOATING.

        END GL_35HISTORY_CDD STRUCTURE.

END GL_35HISTORY.
