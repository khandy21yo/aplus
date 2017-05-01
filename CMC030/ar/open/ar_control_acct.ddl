DEFINE RECORD CDD$TOP.AR.AR_CONTROL_ACCT

        DESCRIPTION IS /*Valid Accounts for Accounts Receivable*/.

        AR_CONTROL_ACCT_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        END AR_CONTROL_ACCT_CDD STRUCTURE.

END AR_CONTROL_ACCT.
