DEFINE RECORD CDD$TOP.WP.WP_REGLINE

        DESCRIPTION IS /*Manufacturing Work In Process Register Line File*/.

        WP_REGLINE_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line Number */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Record Type(01-order,02-comp,03-cancel) */
        REC_TYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Trans Type(M=material,L=labor) */
        TTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Product Number or Operation Code */
        ITEMCODE                DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Cost per Unit Of measure */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = DESCRIPTION
        Description = Description of product or additional */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Original Qty */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Date expected to start production */
        START_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Date Production expected to be complete */
        COMP_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = BATCH
        Description = Batch No */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = TIME
        Description = Post Time */
        POST_TIME               DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Post Date */
        POST_DATE               DATATYPE IS TEXT SIZE IS 8.

        END WP_REGLINE_CDD STRUCTURE.

END WP_REGLINE.
