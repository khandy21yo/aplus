DEFINE RECORD CDD$TOP.GL.GL_CHARTEX

        DESCRIPTION IS /*System GL Acoounts Numbers*/.

        GL_CHARTEX_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = System ID */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = CATEGORY
        Description = Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        END GL_CHARTEX_CDD STRUCTURE.

END GL_CHARTEX.
