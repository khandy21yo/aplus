DEFINE RECORD CDD$TOP.UTL.UTL_ACCOUNT

        DESCRIPTION IS /*Transaction Account Table*/.

        UTL_ACCOUNT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END UTL_ACCOUNT_CDD STRUCTURE.

END UTL_ACCOUNT.
