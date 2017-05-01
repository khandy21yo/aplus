DEFINE RECORD CDD$TOP.IC.IC_JOURADJUST

        DESCRIPTION IS /*Inventory Adjustment Journal*/.

        IC_JOURADJUST_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Adjusted quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END IC_JOURADJUST_CDD STRUCTURE.

END IC_JOURADJUST.
