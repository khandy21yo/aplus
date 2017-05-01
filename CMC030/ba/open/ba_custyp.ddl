DEFINE RECORD CDD$TOP.BA.BA_CUSTYP

        DESCRIPTION IS /*Customer Type Definitions*/.

        BA_CUSTYP_CDD STRUCTURE.

        /* Element =
        Description = Customer Type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number Mask */
        REV_MASK                DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END BA_CUSTYP_CDD STRUCTURE.

END BA_CUSTYP.
