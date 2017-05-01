DEFINE RECORD CDD$TOP.PO.PO_ARCHIVE_SUB_LINE

        DESCRIPTION IS /*Purchase Order Archive Sub Line*/.

        PO_ARCHIVE_SUB_LINE_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Action */
        PO_ACTION               DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        ACTION_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Our Quanity */
        OUR_QTY                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Vendor Quanity */
        VEN_QTY                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Expected Price */
        VEN_RATE                DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Expected Date (YYYYMMDD) */
        RECEIVEDATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = POSTDATE
        Description = Date of posting (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = POSTTIME
        Description = Time of posting */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        END PO_ARCHIVE_SUB_LINE_CDD STRUCTURE.

END PO_ARCHIVE_SUB_LINE.
