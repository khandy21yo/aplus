DEFINE RECORD CDD$TOP.MO.MO_ORDERJOUR

        DESCRIPTION IS /*Manufacturing Order Journal*/.

        MO_ORDERJOUR_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Order Date */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Order Category */
        ORDCAT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Order Discount */
        DISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Miscellaneous Charges */
        MISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Ship Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Ship To Address 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Ship To Address 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Ship To Address 3 */
        ADD3                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Ship To City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Ship To State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Ship To Zip Code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Ship To Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Customer PO. */
        CUSTPO                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Date */
        SHIPDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Ship Via */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Taxes */
        SALESTAX                DATATYPE IS G_FLOATING.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commission amount */
        COMMAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Commission percentage */
        COMMPERC                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salesmen */
        SALESMAN                ARRAY 0:1 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commission for salesmen */
        SALCOMM                 ARRAY 0:1 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Paid Amount */
        AMTPAID                 DATATYPE IS G_FLOATING.

        /* Element = CHECK
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:3 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Freight Number */
        FREIGHT                 DATATYPE IS G_FLOATING.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXFLAG
        Description = Tax Flag */
        TAXFLAG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Line Number of Ship To Code */
        SHIPLIN                 DATATYPE IS TEXT SIZE IS 4.

        END MO_ORDERJOUR_CDD STRUCTURE.

END MO_ORDERJOUR.
