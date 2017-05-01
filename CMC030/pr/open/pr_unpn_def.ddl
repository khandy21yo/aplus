DEFINE RECORD CDD$TOP.PR.PR_UNPN_DEF

        DESCRIPTION IS /*Payroll Union Pension Definition File*/.

        PR_UNPN_DEF_CDD STRUCTURE.

        /* Element =
        Description = Union Pension Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Union Pension Type */
        DTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Paid by Whom */
        PAID_BY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Employee Rate */
        EMPE_RATE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Employer Rate */
        EMPR_RATE               DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        LIA_ACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Basis */
        BASIS                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Deduction Code */
        DED_CODE                DATATYPE IS TEXT SIZE IS 2.

        END PR_UNPN_DEF_CDD STRUCTURE.

END PR_UNPN_DEF.
