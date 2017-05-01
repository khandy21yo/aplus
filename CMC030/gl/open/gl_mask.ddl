DEFINE RECORD CDD$TOP.GL.GL_MASK

        DESCRIPTION IS /*General Ledger Mask*/.

        GL_MASK_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT_MASK               DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Cash Flow code */
        CASH_FLOW               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Work capital code */
        WORK_CAPT               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Financial type code */
        FIN_TYPE                DATATYPE IS TEXT SIZE IS 10.

        END GL_MASK_CDD STRUCTURE.

END GL_MASK.
