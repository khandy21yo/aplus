DEFINE RECORD CDD$TOP.IC.IC_JOURCOUNT

        DESCRIPTION IS /*Inventory Cycle Count Entry Journal*/.

        IC_JOURCOUNT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity count */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cycle Count Control Number */
        CONTROL                 DATATYPE IS TEXT SIZE IS 6.

        END IC_JOURCOUNT_CDD STRUCTURE.

END IC_JOURCOUNT.
