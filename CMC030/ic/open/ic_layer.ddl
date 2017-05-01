DEFINE RECORD CDD$TOP.IC.IC_LAYER

        DESCRIPTION IS /*Product Cost Layer*/.

        IC_LAYER_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        TRANSDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Inventory cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Vendor Name */
        VENDORALF               DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Invoice Number */
        INVOICE                 DATATYPE IS TEXT SIZE IS 15.

        /* Element = DATE
        Description = Post Date (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END IC_LAYER_CDD STRUCTURE.

END IC_LAYER.
