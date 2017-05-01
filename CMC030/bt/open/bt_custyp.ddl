DEFINE RECORD CDD$TOP.BT.BT_CUSTYP

        DESCRIPTION IS /*Customer Type Definition*/.

        BT_CUSTYP_CDD STRUCTURE.

        /* Element =
        Description = Customer Type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        REV_MASK                DATATYPE IS TEXT SIZE IS 18.

        END BT_CUSTYP_CDD STRUCTURE.

END BT_CUSTYP.
