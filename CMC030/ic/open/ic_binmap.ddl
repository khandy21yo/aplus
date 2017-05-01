DEFINE RECORD CDD$TOP.IC.IC_BINMAP

        DESCRIPTION IS /*Product Bin and Level Location*/.

        IC_BINMAP_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Bin location */
        BIN                     ARRAY 0:3 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Safety stock */
        SAFETY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Maximum stock level */
        MAXLEVEL                DATATYPE IS G_FLOATING.

        /* Element =
        Description = ABC flag */
        ABC                     DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Cycle count map */
        CYCLEMAP                DATATYPE IS TEXT SIZE IS 8.

        END IC_BINMAP_CDD STRUCTURE.

END IC_BINMAP.
