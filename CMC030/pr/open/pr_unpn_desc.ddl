DEFINE RECORD CDD$TOP.PR.PR_UNPN_DESC

        DESCRIPTION IS /*Payroll Union Pension Description*/.

        PR_UNPN_DESC_CDD STRUCTURE.

        /* Element =
        Description = UNION PENSION CODE */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        EX_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        END PR_UNPN_DESC_CDD STRUCTURE.

END PR_UNPN_DESC.
