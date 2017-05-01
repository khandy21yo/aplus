DEFINE RECORD CDD$TOP.OE.OE_CREDITLINE

        DESCRIPTION IS /*Credit Memo Line File*/.

        OE_CREDITLINE_CDD STRUCTURE.

        /* Element =
        Description = Memo Number */
        MEMONUM                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Total Credited Quantity */
        CREDQTY                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Qty Returned back into Inventory */
        INVQTY                  DATATYPE IS G_FLOATING.

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
        Description = Miscellaneous Charges */
        MISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Reason Code */
        REASON                  DATATYPE IS TEXT SIZE IS 2.

        END OE_CREDITLINE_CDD STRUCTURE.

END OE_CREDITLINE.
