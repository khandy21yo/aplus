DEFINE RECORD CDD$TOP.UTL.UTL_TRANSACCT

        DESCRIPTION IS /*Inventory Transaction GL Accounts*/.

        UTL_TRANSACCT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODTYPE
        Description = Inventory Product Type */
        PRODTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END UTL_TRANSACCT_CDD STRUCTURE.

END UTL_TRANSACCT.
