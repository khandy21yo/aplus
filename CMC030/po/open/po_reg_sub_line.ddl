DEFINE RECORD CDD$TOP.PO.PO_REG_SUB_LINE

        DESCRIPTION IS /*Purcahse Order Segister Sub-Line*/.

        PO_REG_SUB_LINE_CDD STRUCTURE.

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
        Description = Quantity */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = POSTDATE
        Description = Date of posting (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = POSTTIME
        Description = Time of posting */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        END PO_REG_SUB_LINE_CDD STRUCTURE.

END PO_REG_SUB_LINE.
