DEFINE RECORD CDD$TOP.OE.OE_REGLINE

        DESCRIPTION IS /*Sales Order Register Line File*/.

        OE_REGLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Transaction Type */
        TRANTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity Recorded */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TDATE                   DATATYPE IS TEXT SIZE IS 8.

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
        Description = Posting Date (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Posting Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Packing List Release Number */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Promo Amount */
        PROMO                   DATATYPE IS G_FLOATING.

        /* Element = REFNUM
        Description = Reference Number,Invoice Number */
        REFNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Miscellaneous Charges */
        MISCH                   DATATYPE IS G_FLOATING.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = NOTES
        Description = Notes */
        NOTES1                  DATATYPE IS TEXT SIZE IS 40.

        /* Element = NOTES
        Description = Notes */
        NOTES2                  DATATYPE IS TEXT SIZE IS 30.

        /* Element = SUBACCT
        Description = Sub account (job number)/Serail Number */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Miscellaneous Charges (2) */
        MISCH2                  DATATYPE IS G_FLOATING.

        END OE_REGLINE_CDD STRUCTURE.

END OE_REGLINE.
