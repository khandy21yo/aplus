DEFINE RECORD CDD$TOP.WP.WP_ISSLINE

        DESCRIPTION IS /*Material Issue Line File*/.

        WP_ISSLINE_CDD STRUCTURE.

        /* Element = REQNUM
        Description = Requisition Number */
        REQNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Job Line Number */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Requisition Line Number */
        REQLINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Product Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity Issued */
        QTYISSUE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity Canceled */
        QTYCANCEL               DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Issue Date (YYYYMMDD) */
        ISSDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Running Quantity */
        QTYRUN                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Product Flag */
        PROD_FLAG               DATATYPE IS TEXT SIZE IS 1.

        END WP_ISSLINE_CDD STRUCTURE.

END WP_ISSLINE.
