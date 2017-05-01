DEFINE RECORD CDD$TOP.PR.PR_JWOD_CROSS_REF

        DESCRIPTION IS /*Payroll JWOD Cross (Identify JWOD Jobs)*/.

        PR_JWOD_CROSS_REF_CDD STRUCTURE.

        /* Element =
        Description = Sub account */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = J-JWOD, N-NONJWOD */
        FLAG                    DATATYPE IS TEXT SIZE IS 1.

        END PR_JWOD_CROSS_REF_CDD STRUCTURE.

END PR_JWOD_CROSS_REF.
