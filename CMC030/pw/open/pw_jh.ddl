DEFINE RECORD CDD$TOP.PW.PW_JH

        DESCRIPTION IS /*PW Journal Header*/.

        PW_JH_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTOMER
        Description = Customer Sold To */
        SOLDTO                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTOMER
        Description = Customer Shipped To */
        SHIPTO                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Invoice Date (YYYYMMDD) */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Ship Date (YYYYMMDD) */
        SHPDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = PO
        Description = Purchase order number */
        CUSPO                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Sold By */
        SOLDBY                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = TERMS
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CARRIER
        Description = Carrier Code (Ship Via) */
        CARNAM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = FOB
        Description = F.O.B. */
        FOBFLG                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Line Count */
        LINE1                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Line Count */
        LINE2                   DATATYPE IS SIGNED LONGWORD.

        END PW_JH_CDD STRUCTURE.

END PW_JH.
