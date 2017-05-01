DEFINE RECORD CDD$TOP.PR.PR_CERT_MIN_WAGE

        DESCRIPTION IS /*Certificate of Minimum Wage*/.

        PR_CERT_MIN_WAGE_CDD STRUCTURE.

        /* Element = DATE
        Description = Date */
        EFF_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Minimum wage rate */
        RATE                    DATATYPE IS G_FLOATING.

        END PR_CERT_MIN_WAGE_CDD STRUCTURE.

END PR_CERT_MIN_WAGE.
