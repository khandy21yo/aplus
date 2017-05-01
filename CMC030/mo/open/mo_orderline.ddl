DEFINE RECORD CDD$TOP.MO.MO_ORDERLINE

        DESCRIPTION IS /*Manufacturing Orders Model Lines*/.

        MO_ORDERLINE_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Model line Number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make of Dealer's Model */
        MAKE                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Year of Make */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Type of Make */
        MTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Size of Make */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Inventory Product */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity Ordered */
        ORDQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        REQDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Quantity to ship */
        SHPQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity on back order */
        BCKQTY                  DATATYPE IS G_FLOATING.

        /* Element = NOTES
        Description = Line notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Discount */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Serial Number */
        IDNUM                   DATATYPE IS TEXT SIZE IS 10.

        END MO_ORDERLINE_CDD STRUCTURE.

END MO_ORDERLINE.
