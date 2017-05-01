DEFINE RECORD CDD$TOP.WP.WP_REQREGISTER

        DESCRIPTION IS /*Material Requisition Register File*/.

        WP_REQREGISTER_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Job Line */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = REQNUM
        Description = Requisition Number */
        REQNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Requisition Line */
        REQLIN                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Record Type */
        RECTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = QUANTITY
        Description = Quantity */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = Dollar Amount */
        AMT                     DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Transactin Date (YYYYMMDD) */
        TRANDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Posting Date (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END WP_REQREGISTER_CDD STRUCTURE.

END WP_REQREGISTER.
