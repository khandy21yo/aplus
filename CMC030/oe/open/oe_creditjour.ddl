DEFINE RECORD CDD$TOP.OE.OE_CREDITJOUR

        DESCRIPTION IS /*Credit Memo Journal Header*/.

        OE_CREDITJOUR_CDD STRUCTURE.

        /* Element =
        Description = Memo Number */
        MEMONUM                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Memo Date */
        MEMODATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Order Date */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

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
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Reason Code */
        REASON                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = ORDTYPE
        Description = Order type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = SALESMAN
        Description = Salesperson number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Shipping Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Address, line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 3 */
        ADD3                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Zip Code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        END OE_CREDITJOUR_CDD STRUCTURE.

END OE_CREDITJOUR.
