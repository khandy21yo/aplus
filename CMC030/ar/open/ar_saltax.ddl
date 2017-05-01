DEFINE RECORD CDD$TOP.AR.AR_SALTAX

        DESCRIPTION IS /*Sales Tax Table*/.

        AR_SALTAX_CDD STRUCTURE.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTY
        Description = County */
        COUNTY                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Sales Tax Percentage */
        PERCENT                 DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END AR_SALTAX_CDD STRUCTURE.

END AR_SALTAX.
