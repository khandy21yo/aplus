DEFINE RECORD CDD$TOP.BI.BI_INSUREX

        DESCRIPTION IS /*Insurance Carrier Extra File*/.

        BI_INSUREX_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Reference number */
        REFERENCE               DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = Group Number */
        GROUPNUM                DATATYPE IS TEXT SIZE IS 16.

        END BI_INSUREX_CDD STRUCTURE.

END BI_INSUREX.
