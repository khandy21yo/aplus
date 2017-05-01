DEFINE RECORD CDD$TOP.MO.MO_REGLINE

        DESCRIPTION IS /*Manufacturing Order Register Line File*/.

        MO_REGLINE_CDD STRUCTURE.

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

        /* Element = REFNUM
        Description = Reference number */
        REFNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = NOTES
        Description = Line notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Discount */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Serial Num */
        IDNUM                   DATATYPE IS TEXT SIZE IS 10.

        END MO_REGLINE_CDD STRUCTURE.

END MO_REGLINE.
