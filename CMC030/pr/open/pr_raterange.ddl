DEFINE RECORD CDD$TOP.PR.PR_RATERANGE

        DESCRIPTION IS /*Payroll Rate Range*/.

        PR_RATERANGE_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Age */
        AGE                     DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Minimum Rate allowed */
        MIN_RATE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Maximum Rate Allowed */
        MAX_RATE                DATATYPE IS G_FLOATING.

        END PR_RATERANGE_CDD STRUCTURE.

END PR_RATERANGE.
