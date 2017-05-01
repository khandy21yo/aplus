DEFINE RECORD CDD$TOP.PR.PR_OVERHEAD_DESC

        DESCRIPTION IS /*Payroll Overhead Description File*/.

        PR_OVERHEAD_DESC_CDD STRUCTURE.

        /* Element =
        Description = Overhead key */
        OVH_KEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Overhead rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Basis (1-hours, 2-amount) */
        BASIS                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        PREM_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        OVRHD_ACCT              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        EX_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        END PR_OVERHEAD_DESC_CDD STRUCTURE.

END PR_OVERHEAD_DESC.
