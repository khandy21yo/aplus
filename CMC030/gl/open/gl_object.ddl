DEFINE RECORD CDD$TOP.GL.GL_OBJECT

        DESCRIPTION IS /*General Ledger Object Mask File*/.

        GL_OBJECT_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        OBJ_MASK                DATATYPE IS TEXT SIZE IS 18.

        /* Element = DESCRIPTION6
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Account Type */
        ACCT_TYPE               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Summary Flag */
        SUMM_FLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Cash Flow */
        CASH_FLOW               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Work Capitol */
        WORK_CAPT               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Financial Type */
        FIN_TYPE                DATATYPE IS TEXT SIZE IS 10.

        END GL_OBJECT_CDD STRUCTURE.

END GL_OBJECT.
