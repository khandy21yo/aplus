DEFINE RECORD CDD$TOP.PR.PR_WC_DEFINITION

        DESCRIPTION IS /*Workman Comp Definition File*/.

        PR_WC_DEFINITION_CDD STRUCTURE.

        /* Element =
        Description = */
        CODE                    DATATYPE IS TEXT SIZE IS 6.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        SUBJ_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = OPERATION
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        END PR_WC_DEFINITION_CDD STRUCTURE.

END PR_WC_DEFINITION.
