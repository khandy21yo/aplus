DEFINE RECORD CDD$TOP.OE.OE_INVLINE

        DESCRIPTION IS /*Order Invoice Journal Line File*/.

        OE_INVLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Quantity (shipped) Invoiced */
        INVQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity Cancelled */
        CANCELQTY               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Sales Price per Unit */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount Percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Promo Amount Off */
        PROMO                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Misc Line Charges */
        MISCH                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Line Notes */
        NOTES                   DATATYPE IS TEXT SIZE IS 40.

        END OE_INVLINE_CDD STRUCTURE.

END OE_INVLINE.
