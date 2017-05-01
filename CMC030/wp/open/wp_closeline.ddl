DEFINE RECORD CDD$TOP.WP.WP_CLOSELINE

        DESCRIPTION IS /*WIP Closing Variance Journal*/.

        WP_CLOSELINE_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Flag */
        LFLAG                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Variance Class */
        VCLASS                  DATATYPE IS TEXT SIZE IS 4.

        /* Element = ACCOUNT
        Description = Variance Account Number */
        VACCT                   DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Variance Amount */
        VAMOUNT                 DATATYPE IS G_FLOATING.

        END WP_CLOSELINE_CDD STRUCTURE.

END WP_CLOSELINE.
