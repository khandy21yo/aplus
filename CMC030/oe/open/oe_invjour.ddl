DEFINE RECORD CDD$TOP.OE.OE_INVJOUR

        DESCRIPTION IS /*Order Invoice Journal Header File*/.

        OE_INVJOUR_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Invoice Date */
        INVDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Date */
        SHIPDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Handling */
        HANDLING                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Order Discount Amount */
        DISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Miscellaneous Charges */
        MISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Reason Code */
        CREASON                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Number of Payments */
        PAYMNT                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = unused field */
        UNUSED                  DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Freight Amount */
        FREIGHT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Sales Tax Amount */
        SALESTAX                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:3 DATATYPE IS TEXT SIZE IS 40.

        /* Element = INVOICE
        Description = Invoice number */
        INVOICE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Packing List Release Number */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = CARRIER
        Description = Carrier Code (Ship Via) */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        MISCACCT                DATATYPE IS TEXT SIZE IS 18.

        END OE_INVJOUR_CDD STRUCTURE.

END OE_INVJOUR.
