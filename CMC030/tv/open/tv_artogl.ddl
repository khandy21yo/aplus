DEFINE RECORD CDD$TOP.TV.TV_ARTOGL

        DESCRIPTION IS /*TV Ar to Gl Conversion Table*/.

        TV_ARTOGL_CDD STRUCTURE.

        /* Element =
        Description = Customer type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        SALE_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        DISC_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        PRODUCTION_ACCT         DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Discount Percentage */
        DISC_PER                DATATYPE IS SIGNED WORD.

        END TV_ARTOGL_CDD STRUCTURE.

END TV_ARTOGL.
