DEFINE RECORD CDD$TOP.WP.WP_ORDERLINE

        DESCRIPTION IS /*Manufacturing Work In Process Production Line File*/.

        WP_ORDERLINE_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Type of Line(M=material,L=labor) */
        TTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRODUCT
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

        /* Element =
        Description = Line Number */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        END WP_ORDERLINE_CDD STRUCTURE.

END WP_ORDERLINE.
