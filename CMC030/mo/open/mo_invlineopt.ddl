DEFINE RECORD CDD$TOP.MO.MO_INVLINEOPT

        DESCRIPTION IS /*Manufacturing Invoice Line Options File*/.

        MO_INVLINEOPT_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Order Line Number */
        OLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Order Line Option Number */
        OPTLINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Quantity Invoiced (Shipped) */
        INVQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity Cancelled */
        CANCELQTY               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Sales Price Per Unit */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount Percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        END MO_INVLINEOPT_CDD STRUCTURE.

END MO_INVLINEOPT.
