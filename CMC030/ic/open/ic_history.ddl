DEFINE RECORD CDD$TOP.IC.IC_HISTORY

        DESCRIPTION IS /*Product Balance History*/.

        IC_HISTORY_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Period (YYYYPP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Transaction type */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Beginning Quantity */
        BQUANTITY               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Period Posted Quantity */
        PQUANTITY               DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Post date */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post time */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Amount of Sale */
        SALEAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount of Cost of Sale */
        COSTAMT                 DATATYPE IS G_FLOATING.

        END IC_HISTORY_CDD STRUCTURE.

END IC_HISTORY.
