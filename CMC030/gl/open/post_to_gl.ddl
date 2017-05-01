DEFINE RECORD CDD$TOP.GL.POST_TO_GL

        DESCRIPTION IS /*Batch Control File*/.

        POST_TO_GL_CDD STRUCTURE.

        /* Element =
        Description = Account number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Description from chart */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Account type */
        ACCTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Real account balance */
        BEGBAL                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount of credits */
        CREDIT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount of debits */
        DEBIT                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Number of units */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Number of hours */
        HOURS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Undefined Account (Y/N) */
        UNDEFINED_ACCT          DATATYPE IS TEXT SIZE IS 1.

        END POST_TO_GL_CDD STRUCTURE.

END POST_TO_GL.
