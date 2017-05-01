DEFINE RECORD CDD$TOP.GL.GL_VALID_ACCT

        DESCRIPTION IS /*Valid Account by System Id*/.

        GL_VALID_ACCT_CDD STRUCTURE.

        /* Element =
        Description = System id */
        SYSTEMID                DATATYPE IS TEXT SIZE IS 6.

        /* Element = ACCOUNT
        Description = General Ledger account number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        END GL_VALID_ACCT_CDD STRUCTURE.

END GL_VALID_ACCT.
