DEFINE RECORD CDD$TOP.WP.WP_REQJOUR

        DESCRIPTION IS /*Requisition Journal Header File*/.

        WP_REQJOUR_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Job Line NUmber */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Requisition Date (YYYYMMDD) */
        REQDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = NOTES
        Description = Notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        END WP_REQJOUR_CDD STRUCTURE.

END WP_REQJOUR.
