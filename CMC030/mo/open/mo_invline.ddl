DEFINE RECORD CDD$TOP.MO.MO_INVLINE

        DESCRIPTION IS /*Manufacturing Order Invoice Line Journal File*/.

        MO_INVLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Number */
        OLINE                   DATATYPE IS TEXT SIZE IS 4.

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

        END MO_INVLINE_CDD STRUCTURE.

END MO_INVLINE.
