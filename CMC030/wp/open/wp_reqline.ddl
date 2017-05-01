DEFINE RECORD CDD$TOP.WP.WP_REQLINE

        DESCRIPTION IS /*Material Requisition Journal Line*/.

        WP_REQLINE_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Job Line */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = REQNUM
        Description = Requisition Number */
        REQNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATION
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity Required */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element = REQLINE
        Description = Requistition Line */
        REQLINE                 DATATYPE IS TEXT SIZE IS 4.

        END WP_REQLINE_CDD STRUCTURE.

END WP_REQLINE.
