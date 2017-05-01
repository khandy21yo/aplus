DEFINE RECORD CDD$TOP.WP.WP_REGHEADER

        DESCRIPTION IS /*WP Register Header*/.

        WP_REGHEADER_CDD STRUCTURE.

        /* Element =
        Description = Subject type for Job "J" */
        SUBJECT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = SUBACCT
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION6
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

        /* Element =
        Description = Job Status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Closed Date (YYYYMMDD) */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = LOCATION
        Description = Job Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element = REFNO
        Description = Reference number */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element = BATCH
        Description = Batch No */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = TIME
        Description = Post Time */
        POST_TIME               DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Post Date */
        POST_DATE               DATATYPE IS TEXT SIZE IS 8.

        END WP_REGHEADER_CDD STRUCTURE.

END WP_REGHEADER.
