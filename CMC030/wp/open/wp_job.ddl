DEFINE RECORD CDD$TOP.WP.WP_JOB

        DESCRIPTION IS /*Manufacturing Work In Process Journal Header File*/.

        WP_JOB_CDD STRUCTURE.

        /* Element = SUBACCT
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION
        Description = Job Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Job Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CLASS
        Description = Job Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Creation Date (YYYYMMDD) */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = LOCATION
        Description = Job Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element = REFNO
        Description = Reference number */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        END WP_JOB_CDD STRUCTURE.

END WP_JOB.
