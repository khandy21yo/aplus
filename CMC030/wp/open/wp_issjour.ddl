DEFINE RECORD CDD$TOP.WP.WP_ISSJOUR

        DESCRIPTION IS /*Material Issue Journal*/.

        WP_ISSJOUR_CDD STRUCTURE.

        /* Element = REQNUM
        Description = Requisition Number */
        REQNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        END WP_ISSJOUR_CDD STRUCTURE.

END WP_ISSJOUR.
