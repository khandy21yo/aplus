DEFINE RECORD CDD$TOP.PD.PD_PRODACCT

        DESCRIPTION IS /*Product GL Account Table*/.

        PD_PRODACCT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODTYPE
        Description = Inventory Product Type */
        PRODTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = Inventory GL Account Number */
        INVACC                  DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Cost of Sale GL Account Number */
        COSACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Product Discount GL Account Number */
        DISCACCT                DATATYPE IS TEXT SIZE IS 18.

        END PD_PRODACCT_CDD STRUCTURE.

END PD_PRODACCT.
