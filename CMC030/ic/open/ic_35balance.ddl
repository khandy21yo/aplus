DEFINE RECORD CDD$TOP.IC.IC_35BALANCE

        DESCRIPTION IS /*Inventory Product Balance File*/.

        IC_35BALANCE_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Beginning Balance */
        BBALANCE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Posted Quantity */
        PBALANCE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Running Balance (journals) */
        RBALANCE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Control Balance */
        CBALANCE                DATATYPE IS G_FLOATING.

        END IC_35BALANCE_CDD STRUCTURE.

END IC_35BALANCE.
