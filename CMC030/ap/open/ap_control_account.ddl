DEFINE RECORD CDD$TOP.AP.AP_CONTROL_ACCOUNT

        DESCRIPTION IS /*List of AP accounts for audit reports*/.

        AP_CONTROL_ACCOUNT_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END AP_CONTROL_ACCOUNT_CDD STRUCTURE.

END AP_CONTROL_ACCOUNT.
