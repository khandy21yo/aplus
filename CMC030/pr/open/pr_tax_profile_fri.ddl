DEFINE RECORD CDD$TOP.PR.PR_TAX_PROFILE_FRI

        DESCRIPTION IS /*Payroll Tax Fringe Expense Distribution*/.

        PR_TAX_PROFILE_FRI_CDD STRUCTURE.

        /* Element = TAXTYPE
        Description = Tax type (FI,FU,SU) */
        TAX_TYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        LABOR_ACCT              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        FRI_EX_ACCT             DATATYPE IS TEXT SIZE IS 18.

        END PR_TAX_PROFILE_FRI_CDD STRUCTURE.

END PR_TAX_PROFILE_FRI.
