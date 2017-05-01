DEFINE RECORD CDD$TOP.PD.PD_ACCOUNT

        DESCRIPTION IS /*Product Type Account Table*/.

        PD_ACCOUNT_CDD STRUCTURE.

        /* Element = PRODTYPE
        Description = Inventory Product Type */
        PRODTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = ACCOUNT
        Description = Inventory General Ledger Account Number */
        INVACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Work in Process Account */
        WIPACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = COS General Ledger Account Number */
        COSACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Inv Disc General Ledger Account Number */
        DISCACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Miscellaneous Charges Account Num */
        MISCHACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Product Price Variance Account */
        PRICEVARACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Miscellaneous (2) Changes Account */
        MISCH2ACCT              DATATYPE IS TEXT SIZE IS 18.

        END PD_ACCOUNT_CDD STRUCTURE.

END PD_ACCOUNT.
