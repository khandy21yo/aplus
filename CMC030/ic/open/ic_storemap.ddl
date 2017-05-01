DEFINE RECORD CDD$TOP.IC.IC_STOREMAP

        DESCRIPTION IS /*Cycle Count Map by Day*/.

        IC_STOREMAP_CDD STRUCTURE.

        /* Element = PRODUCT_NUM
        Description = Product number */
        PRODUCT_NUM             DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Bin location */
        BIN                     ARRAY 0:3 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = ABC flag */
        ABC                     DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Cycle daily map */
        CYCLE_MAP               ARRAY 0:11 DATATYPE IS TEXT SIZE IS 4.

        END IC_STOREMAP_CDD STRUCTURE.

END IC_STOREMAP.
