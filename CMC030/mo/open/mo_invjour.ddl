DEFINE RECORD CDD$TOP.MO.MO_INVJOUR

        DESCRIPTION IS /*Manufacturing Order Invoice Header Journal File*/.

        MO_INVJOUR_CDD STRUCTURE.

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
        Description = Miscellaneous Charges GL Account */
        MISCACCT                DATATYPE IS TEXT SIZE IS 18.

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

        END MO_INVJOUR_CDD STRUCTURE.

END MO_INVJOUR.
