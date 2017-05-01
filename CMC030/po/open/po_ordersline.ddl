DEFINE RECORD CDD$TOP.PO.PO_ORDERSLINE

        DESCRIPTION IS /*PO Subline Journal File*/.

        PO_ORDERSLINE_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Our Quanity */
        OUR_QTY                 DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Expected Date (YYYYMMDD) */
        RECEIVEDATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        GL_ACCOUNT              DATATYPE IS TEXT SIZE IS 18.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        END PO_ORDERSLINE_CDD STRUCTURE.

END PO_ORDERSLINE.
