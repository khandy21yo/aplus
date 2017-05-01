DEFINE RECORD CDD$TOP.IC.IC_BALANCE

        DESCRIPTION IS /*Product Balance File*/.

        IC_BALANCE_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Record type */
        RECTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction class */
        CLASS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Post date (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END IC_BALANCE_CDD STRUCTURE.

END IC_BALANCE.
