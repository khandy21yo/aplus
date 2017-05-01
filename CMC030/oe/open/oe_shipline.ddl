DEFINE RECORD CDD$TOP.OE.OE_SHIPLINE

        DESCRIPTION IS /*Shipping Journal Line File*/.

        OE_SHIPLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Quantity to Ship */
        SHPQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Canceled Quantity */
        CANCELQTY               DATATYPE IS G_FLOATING.

        END OE_SHIPLINE_CDD STRUCTURE.

END OE_SHIPLINE.
