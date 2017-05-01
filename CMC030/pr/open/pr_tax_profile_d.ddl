DEFINE RECORD CDD$TOP.PR.PR_TAX_PROFILE_D

        DESCRIPTION IS /*Payroll Tax Profile - County*/.

        PR_TAX_PROFILE_D_CDD STRUCTURE.

        /* Element =
        Description = 'D' county authority */
        AUTH                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Report number */
        REPNO                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        WH_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        COU_LIA_ACCT            DATATYPE IS TEXT SIZE IS 18.

        END PR_TAX_PROFILE_D_CDD STRUCTURE.

END PR_TAX_PROFILE_D.
