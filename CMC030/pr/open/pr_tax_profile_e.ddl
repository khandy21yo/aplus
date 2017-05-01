DEFINE RECORD CDD$TOP.PR.PR_TAX_PROFILE_E

        DESCRIPTION IS /*Payroll Tax Profile - School District*/.

        PR_TAX_PROFILE_E_CDD STRUCTURE.

        /* Element =
        Description = 'E' for School */
        AUTH                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = User defined */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        REPNO                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        WH_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        SCH_LIA_ACCT            DATATYPE IS TEXT SIZE IS 18.

        END PR_TAX_PROFILE_E_CDD STRUCTURE.

END PR_TAX_PROFILE_E.
