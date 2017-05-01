DEFINE RECORD CDD$TOP.OE.OE_PROMO

        DESCRIPTION IS /*Promotional Sales Master File*/.

        OE_PROMO_CDD STRUCTURE.

        /* Element = REFNO
        Description = Reference number */
        REFPROMO                DATATYPE IS TEXT SIZE IS 16.

        /* Element = DATE
        Description = From Date (YYYYMMDD) */
        FROMDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = To Date (YYYYMMDD) */
        TODATE                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END OE_PROMO_CDD STRUCTURE.

END OE_PROMO.
