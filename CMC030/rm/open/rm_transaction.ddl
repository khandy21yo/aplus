DEFINE RECORD CDD$TOP.RM.RM_TRANSACTION

        DESCRIPTION IS /*Restaurant Transaction File*/.

        RM_TRANSACTION_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        TRANSDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = STATIONMAN
        Description = Station man (operator) */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Post date */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post time */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END RM_TRANSACTION_CDD STRUCTURE.

END RM_TRANSACTION.
