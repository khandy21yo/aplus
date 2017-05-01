DEFINE RECORD CDD$TOP.IC.IC_35HISTORY

        DESCRIPTION IS /*Inventory Transaction History File*/.

        IC_35HISTORY_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Transaction type */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = CROSSREF
        Description = Cross Reference */
        CROSSREF                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Subaccount */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Period Quantity */
        PQUANTITY               ARRAY 0:12 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount of Price */
        PRICEAMT                ARRAY 0:12 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount of Cost of Sale */
        COSTAMT                 ARRAY 0:12 DATATYPE IS G_FLOATING.

        END IC_35HISTORY_CDD STRUCTURE.

END IC_35HISTORY.
