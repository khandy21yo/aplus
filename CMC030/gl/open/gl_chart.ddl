DEFINE RECORD CDD$TOP.GL.GL_CHART

        DESCRIPTION IS /*Chart of Accounts*/.

        GL_CHART_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Account type */
        ACCTYPE                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Cash flow */
        FLOW                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Work capitol */
        WORK                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Financial type */
        FINTYPE                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Summary flag 1 - Detail 2 - By date 3 - */
        SUMMARY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Period dollar totals */
        DOLLAR                  ARRAY 0:20 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Period unit totals */
        UNIT                    ARRAY 0:20 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Period hour totals */
        HOUR                    ARRAY 0:20 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Current period */
        CPERIOD                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Running dollars */
        RUNDOL                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Running units */
        RUNUNIT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Running hours */
        RUNHOUR                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Current dollars */
        CURDOL                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Current units */
        CURUNIT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Current hours */
        CURHOUR                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Last batch updated */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END GL_CHART_CDD STRUCTURE.

END GL_CHART.
