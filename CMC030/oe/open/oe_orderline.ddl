DEFINE RECORD CDD$TOP.OE.OE_ORDERLINE

        DESCRIPTION IS /*Sales Order Line File*/.

        OE_ORDERLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity Ordered */
        ORDQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity to Ship */
        SHPQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount Percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Request Date (YYYYMMDD) */
        REQDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Promo Amount */
        PROMO                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Miscellaneous Charges */
        MISCH                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity on Backorder */
        BCKQTY                  DATATYPE IS G_FLOATING.

        /* Element = NOTES
        Description = Notes */
        NOTES1                  DATATYPE IS TEXT SIZE IS 40.

        /* Element = NOTES
        Description = Notes */
        NOTES2                  DATATYPE IS TEXT SIZE IS 30.

        /* Element = SUBACCT
        Description = Sub account (job number)/Serial Number */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Miscellaneous Charges (2) */
        MISCH2                  DATATYPE IS G_FLOATING.

        END OE_ORDERLINE_CDD STRUCTURE.

END OE_ORDERLINE.
