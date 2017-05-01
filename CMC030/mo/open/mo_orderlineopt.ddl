DEFINE RECORD CDD$TOP.MO.MO_ORDERLINEOPT

        DESCRIPTION IS /*Manufacturing Order Line File*/.

        MO_ORDERLINEOPT_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Model record line number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Option Group */
        OPTGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Option Code */
        OPTN                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Order Quantity */
        ORDQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cost Per Unit */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price Per Unit */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Option Description */
        OPTDESCR                DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Option Line number */
        LINOPT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Quantity to ship */
        SHPQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity on backorder */
        BCKQTY                  DATATYPE IS G_FLOATING.

        /* Element = MAKE
        Description = Make */
        MAKE                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        END MO_ORDERLINEOPT_CDD STRUCTURE.

END MO_ORDERLINEOPT.
