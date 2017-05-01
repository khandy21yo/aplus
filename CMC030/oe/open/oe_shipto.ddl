DEFINE RECORD CDD$TOP.OE.OE_SHIPTO

        DESCRIPTION IS /*Ship to Address*/.

        OE_SHIPTO_CDD STRUCTURE.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        LINES                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Shipping Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Address 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address 3 */
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

        /* Element =
        Description = Ship Via */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Salesmen */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXEXEMPT
        Description = Tax Exampt Permit Number */
        TAXEXEMP                DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Ship to adrress notes */
        NOTES                   ARRAY 0:2 DATATYPE IS TEXT SIZE IS 40.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        END OE_SHIPTO_CDD STRUCTURE.

END OE_SHIPTO.
