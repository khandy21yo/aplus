DEFINE RECORD CDD$TOP.GL.GL_BUD_YYYY

        DESCRIPTION IS /*Budget File for Fiscal Year*/.

        GL_BUD_YYYY_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger account number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element = AMOUNT
        Description = Dollar amount */
        DOLLAR                  ARRAY 0:13 DATATYPE IS G_FLOATING.

        /* Element = UNIT
        Description = Unit amount */
        UNIT                    ARRAY 0:13 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Budget hours */
        HOUR                    ARRAY 0:13 DATATYPE IS G_FLOATING.

        END GL_BUD_YYYY_CDD STRUCTURE.

END GL_BUD_YYYY.
