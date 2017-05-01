DEFINE RECORD CDD$TOP.PR.PR_WC_DESCR

        DESCRIPTION IS /*Workman Comp Description File*/.

        PR_WC_DESCR_CDD STRUCTURE.

        /* Element =
        Description = */
        CODE                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        LIA_ACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        EX_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        END PR_WC_DESCR_CDD STRUCTURE.

END PR_WC_DESCR.
