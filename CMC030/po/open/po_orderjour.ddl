DEFINE RECORD CDD$TOP.PO.PO_ORDERJOUR

        DESCRIPTION IS /*Purchase Order Header*/.

        PO_ORDERJOUR_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Purchase Order Type */
        POTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Release Date (YYYYMMDD) */
        PODATE                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = VENDOR
        Description = Vendor Number (Seller) */
        VENDOR                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Buyer */
        BUYER                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element = TERM
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Carrier */
        CARRIER                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = FOB Code */
        FOB                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Acknowledgement code */
        ACKNOW                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Collect / Pre-paid */
        COL_PPD                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Notes */
        NOTE                    ARRAY 0:3 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Person who typed in data */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Print form (Yes/No) */
        PRINTFORM               DATATYPE IS TEXT SIZE IS 1.

        /* Element = LOCATION
        Description = Location number */
        FROMLOCATION            DATATYPE IS TEXT SIZE IS 4.

        /* Element = NAME
        Description = Name */
        TONAME                  DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Address 1 */
        TOADD1                  DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address 2 */
        TOADD2                  DATATYPE IS TEXT SIZE IS 25.

        /* Element = CITY
        Description = City */
        TOCITY                  DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        TOSTATE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        TOZIP                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTRY
        Description = Country */
        TOCOUNTRY               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Batch Number */
        BATCH                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = FLAG
        Description = Po Flag if this is a new PO */
        NEW                     DATATYPE IS TEXT SIZE IS 1.

        END PO_ORDERJOUR_CDD STRUCTURE.

END PO_ORDERJOUR.
