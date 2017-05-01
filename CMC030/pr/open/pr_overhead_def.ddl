DEFINE RECORD CDD$TOP.PR.PR_OVERHEAD_DEF

        DESCRIPTION IS /*Payroll Overhead Subject File*/.

        PR_OVERHEAD_DEF_CDD STRUCTURE.

        /* Element =
        Description = Overhead key */
        OVH_KEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        SUBJ_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = OPERATION
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        END PR_OVERHEAD_DEF_CDD STRUCTURE.

END PR_OVERHEAD_DEF.
