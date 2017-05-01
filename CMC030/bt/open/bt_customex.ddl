DEFINE RECORD CDD$TOP.BT.BT_CUSTOMEX

        DESCRIPTION IS /*Extra Info for Customer File*/.

        BT_CUSTOMEX_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Customer Type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        END BT_CUSTOMEX_CDD STRUCTURE.

END BT_CUSTOMEX.
